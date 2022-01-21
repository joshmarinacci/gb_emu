mod cpu;
mod bootrom;
mod mmu;
mod opcodes;
mod debugger;
mod common;

use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::BitXorAssign;
use std::path::{Path, PathBuf};
use std::{fs, thread};
use std::fs::read_to_string;
use std::io::Error;
use std::time::Duration;
use log4rs::append::file::FileAppender;
use log4rs::Config;
use log4rs::config::{Appender, Root};
use log::{info, LevelFilter};
use serde_json::Value;
use structopt::StructOpt;
use common::RomFile;
use crate::cpu::{OpList, Z80};
use crate::debugger::start_debugger;
use crate::mmu::MMU;


fn fetch_opcode_from_memory(cpu:&mut Z80, mmu:&mut MMU) -> (u16,u16) {
    let pc = cpu.r.pc;
    // println!("pc is {}",pc);
    let fb:u8 = mmu.read8(pc);
    // println!("fb is {:x}",fb);
    if fb == 0xcb {
        let sb:u8 = mmu.read8(pc+1);
        (0xcb00 | sb as u16,2)
    } else {
        (fb as u16, 1)
    }
}

fn main() {
    let args = init_setup();
    println!("runnnig with args {:?}",args);
    if let Some(ref pth) = args.romfile {
        println!("loading the romfile {:?}", pth.as_path());
        if let Ok(cart) = load_romfile(pth) {
            run_romfile(cart, args.interactive, &args);
        }
    } else {
        if args.interactive {
            run_bootrom_interactive(&args);
        } else {
            run_bootrom(&args);
        }
    }
}

fn run_bootrom_interactive(args: &Cli) {
    println!("running the bootrom");
    let mut cpu = Z80::init();
    let mut mmu = MMU::init_with_bootrom();
    cpu.reset();
    cpu.r.pc = 0x00;
    let OPCODE_MAP = load_opcode_map();
    start_debugger(cpu,mmu,OPCODE_MAP, None,args.fastforward);
}

fn load_opcode_map() -> serde_json::Value {
    let raw= read_to_string(Path::new("./resources/opcodes.json")).unwrap();
    return serde_json::from_str(&raw).unwrap();
}

fn run_romfile(cart: RomFile, interactive: bool, args:&Cli) {
    println!("running the cart {:?}", cart.data.len());
    let mut cpu = Z80::init();
    let mut mmu = MMU::init_with_rom_no_header(&cart.data);
    cpu.reset();
    cpu.r.pc = 0x100;
    let OPCODE_MAP = load_opcode_map();

    if interactive {
        start_debugger(cpu, mmu, OPCODE_MAP, Some(cart), args.fastforward);
    } else {
        loop {
            execute(&mut cpu, &mut mmu, &OPCODE_MAP);
        }
    }
}


fn load_romfile(pth: &PathBuf) -> Result<RomFile,Error> {
    let pth2:String = pth.as_path().to_str().unwrap().parse().unwrap();
    let data:Vec<u8> = fs::read(pth)?;
    Ok(RomFile {
        data:data,
        path: pth2,
    })
}

fn run_bootrom(x: &Cli) {
    println!("======= running the bootrom ===== ");
    let mut cpu = Z80::init();
    let mut mmu = MMU::init_with_bootrom();
    cpu.reset();

    let OPCODE_MAP = load_opcode_map();
    //by following along from
    // https://realboyemulator.wordpress.com/2013/01/03/a-look-at-the-game-boy-bootstrap-let-the-fun-begin/

    // starting at the beginning of the boot rom
    // 0x0000 -> 0x31, 0xFE, 0xFF,  -> LD SP, $0xFFFE
    assert_eq!(cpu.r.pc,0);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    assert_eq!(cpu.r.sp,0xFFFE);
    assert_eq!(cpu.r.pc,3);

    // 0x0003 -> 0xAF -> XOR A
    execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    assert_eq!(cpu.r.a,0);

    // 0x0004 -> 0x21, 0xFF, 0x9F -> LD HL 16
    // load 0x9FFF into the HL register
    execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    assert_eq!(cpu.r.get_hl(),0x9FFF);

    // 0x0007 -> LD (HL-),A
    // load register A to the memory address pointed to by HL
    // meaning, write 0 to 0x9FFF
    // then decrement HL
    execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    assert_eq!(mmu.data[0x9FFF],0);
    assert_eq!(cpu.r.get_hl(),0x9FFE);

    // 0x0008 -> BIT 7, H
    // test MSB of H, set or clear ZERO flag
    execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    // zero flag should be cleared
    assert_eq!(cpu.r.zero_flag,false);

    //0x000A -> JRNZ .+0xfb
    //jump if not zero to the address 0xFB relative to the current address
    // 0xFB should be interpreted as signed
    // so jump to 0x000C - 0x0005 = 0x0007
    execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    assert_eq!(cpu.r.pc, 0x0007);

    //loop util PC equals 0x000C
    while cpu.r.pc != 0x000C {
        execute(&mut cpu, &mut mmu, &OPCODE_MAP );
    }
    // setup audio device stuff
    println!("setting up audio device");
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x000C  LD HL $0xFF26  # load 0xFF26 into HL
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x000F  LD C, $0x11    # load 0x11 into C
    assert_eq!(cpu.r.c,0x11);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x0011  LD A, $0x80    # load 0x80 into A
    assert_eq!(cpu.r.a,0x80);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x0013  LD (HL-), A # load A to address pointed to by HL and Dec HL
    assert_eq!(mmu.data[0xFF26],0x80);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x0014  LD ($0xFF00+C), A # load A to address 0xFF00+C (0xFF11)
    assert_eq!(mmu.data[0xFF11],0x80);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x0015 – INC C # increment C register
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x0016 – LD A, $0xF3 # load 0xF3 to A
    assert_eq!(cpu.r.a,0xf3);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x0018 – LD ($0xFF00+C), A # load A to address 0xFF00+C (0xFF12)
    assert_eq!(mmu.data[0xff12],0xf3);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); //0x0019 – LD (HL-), A # load A to address pointed to by HL and Dec HL
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); //0x001A – LD A, $0x77 # load 0x77 to A
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); //0x001C – LD (HL), A # load A to address pointed to by HL

    println!("setup palette");
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x001D  LD A, $0xFC  # A represents the color number's mapping
    assert_eq!(cpu.r.a,0xFC);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x001F  LD (0xFF00 + 0x47), A #initialize the palette
    assert_eq!(mmu.data[0xFF47],0xFC);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x0021  LD DE 0x0104 # pointer to Nintendo logo
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x0024  LD HL 0x8010 # pointer to vram
    assert_eq!(cpu.r.get_hl(),0x8010);
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x0027  LD A, (DE) # load next byte from Nintendo logo
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x0028  CALL $0x0095 # decompress, scale and write pixels to VRAM (1)
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); // 0x002B  CALL $0x0096 # decompress, scale and write pixels to VRAM (2)
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); // 0x002E  INC DE # advance pointer
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); // 0x002F – LD A, E # …
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); // 0x0030 – CP $0x34 # compare accumulator to 0x34
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); // 0x0032 – JRNZ .+0xf3 # loop if not finished comparing
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); // 0x0034 – LD DE, $0x00D8 # …
    execute(&mut cpu, &mut mmu, &OPCODE_MAP ); // 0x0037 – LD B, $0x8 # …
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x0039 – LD A, (DE) # …
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x003A – INC DE # …
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x003B – LD (HL+), A # …
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x003C – INC HL # …
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x003D – DEC B # …
    execute(&mut cpu, &mut mmu, &OPCODE_MAP); // 0x003E – JRNZ .+0xf9 # jump if not zero to 0x0039
    println!("tile loading?");
    for n in 0..11 {
        execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    }

    println!("scrolling");
    for n in 0..6 {
        execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    }
    println!("the LCD should be on now");
    // assert_eq!(&mut hardware.lcd.on,true);
    assert_eq!(mmu.data[0xFF42],0x64); // vertical scroll register
    assert_eq!(cpu.r.a,0x91);
    assert_eq!(mmu.data[0xFF40],0x91); // turn on the LCD display

    execute(&mut cpu, &mut mmu, &OPCODE_MAP);
    assert_eq!(cpu.r.b,1);

    //at 0x0076  CP $0x62 # when scroll count is 0x62, play sound 1
    //at 0x007C  CP $0x64 # when scroll count is 0x64, play sound 2
    //at 0x00E0  LD HL $0x0104 # should point to the nintendo logo in the cartridge
    //at 0x00E3  LD HL $0x0104 # should point to the nintendo logo in the bootrom
    //at 0x00FE  LD ($0xFF00+$0x50), A # disable the boot ROM. assert mmu.bootroom_enabled == false
    //start the cartridge at 0x0100
}


//
// // INC C
// fn op_000c_INC_C(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
//     // println!("INC C");
//     cpu.r.c += 1;
//     (1,8)
// }
//
// // LD (C),A
// fn op_00e2_LD_CA(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
//     let addr:u16 = ((0xFF00 as u16) + (cpu.r.c as u16)) as u16;
//     mmu.write8(addr, cpu.r.a);
//     (1,8)
// }
//
//
//
// // XOR A
// fn op_00AF_XOR_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
//     cpu.r.a.bitxor_assign(cpu.r.a);
//     (1,4)
// }
// //16bit register loads
//
// // load register A into memory pointed at by HL, then decrement HL
// fn op_0032_LD_HLm_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
//     mmu.write8(cpu.r.get_hl(),cpu.r.a);
//     cpu.r.set_hl(cpu.r.get_hl()-1);
//     (1,8)
// }
// // load register A into memory pointed at by HL
// fn op_0077_LD_HL_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
//     mmu.write8(cpu.r.get_hl(),cpu.r.a);
//     (1,8)
// }
// // BIT 7,H
// fn op_CB76_BIT_7_H(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
//     cpu.r.zero_flag = !((cpu.r.h & 0b1000_0000) > 0);
//     (2,8)
// }

fn execute(cpu: &mut Z80, mmu: &mut MMU, opcodes: &Value) {
    // println!("PC at {:04x}",cpu.r.pc);
    let (opcode, off) = fetch_opcode_from_memory(cpu, mmu);
    // println!("op {:0x} arg {:0x}", opcode, off);
    let (off,size_of_inst) = opcodes::decode(opcode, off, cpu, mmu, opcodes);
    // println!("off is {}",off);
    let (v2, went_over) = cpu.r.pc.overflowing_add(off as u16);
    if went_over {
        mmu.print_cram();
        panic!("PC overflowed memory");
    }
    cpu.r.pc = v2;
    mmu.update();
    // println!("inst size was {}",size_of_inst);
    // cpu.r.pc = cpu.r.pc.wrapping_add(size_of_inst as u16);
}



#[derive(StructOpt, Debug)]
#[structopt(name = "gbemu", about = "gb emulator")]
struct Cli {
    #[structopt(long)]
    debug:bool,
    #[structopt(parse(from_os_str))]
    romfile: Option<PathBuf>,
    #[structopt(long)]
    boot:bool,
    #[structopt(long)]
    interactive:bool,
    #[structopt(long, default_value="0")]
    fastforward:u32,
}

fn init_setup() -> Cli {
    let args:Cli = Cli::from_args();
    let loglevel = if args.debug { LevelFilter::Debug } else { LevelFilter::Error };

    // create file appender with target file path
    let logfile = FileAppender::builder()
        .build("log/output.log").expect("error setting up file appender");

    // make a config
    let config = Config::builder()
        //add the file appender
        .appender(Appender::builder().build("logfile", Box::new(logfile)))
        //now make it
        .build(Root::builder()
            .appender("logfile") // why do we need to mention logfile again?
            .build(loglevel)).expect("error setting up log file");

    log4rs::init_config(config).expect("error initing config");

    thread::sleep(Duration::from_millis(100));
    println!("logging to log/output.log");
    for i in 0..5 {
        info!("        ");
    }
    info!("==============");
    info!("starting new run");
    info!("running with args {:?}",args);
    return args;
}
