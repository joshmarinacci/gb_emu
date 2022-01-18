mod cpu;
mod bootrom;
mod mmu;
mod opcodes;

use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::BitXorAssign;
use crate::cpu::{OpList, Z80};
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
// NOOP
// fn op_0000(arg:u16, cpu:&mut Z80, mmu:&mut MMU) -> (usize, usize) {
//     (4,1)
// }
// LD (HL),nn -> load to
// LD (HL),n -> load to memory at address in HL from arg n
// fn op_0036(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
//     mmu.write16(cpu.r.get_hl(),arg);
//     cpu.r.sp += 2; //increment PC
//     (3,12)
// }
fn decode(code:u16, arg:u16, cpu:&mut Z80, mmu:&mut MMU) -> (usize, usize) {

    if let Some(op) = cpu.ops.ops.get(&code) {
        println!("PC {:04x}: OP {:04x}: {}",cpu.r.pc, code, op.name);
        let il = op.inst_len;
        let tl = op.tim_len;
        (op.fun)(cpu,mmu);
        return (il,tl);
    }

    if cpu.r.pc >= 0x0007 && cpu.r.pc <= 0x00a {
        // println!("in the loop")
    } else {
        println!("PC {:04x}: OP {:04x}: {}", cpu.r.pc, code, op_to_name(code));
    }

    match code {
        // 0x0000 => op_0000(arg,cpu,mmu),
        0x00_AF => op_00AF_XOR_A(arg,cpu,mmu),
        0x00_32 => op_0032_LD_HLm_A(arg,cpu,mmu),
        0xCB_7C => op_CB76_BIT_7_H(arg,cpu,mmu),
        0x00_20 => op_0020_JR_NZ_r8(arg,cpu,mmu),
        0x00_e2 => op_00e2_LD_CA(arg,cpu,mmu),
        0x00_0c => op_000c_INC_C(arg,cpu,mmu),
        0x00_77 => op_0077_LD_HL_A(arg,cpu,mmu),
        0x00_1a => op_001a_LD_A_DE(arg,cpu,mmu),
        0x00_cd => op_00cd_call_a16(arg,cpu,mmu),
        0x00_13 => op_0013_inc_de(arg,cpu,mmu),
        0x00_7b => op_007b_LD_A_E(arg,cpu,mmu),
        0x00_fe => op_00fe_CP_d8(arg,cpu,mmu),
        0x00_05 => op_0005(arg,cpu,mmu),
        0x00_22 => op_0022(arg,cpu,mmu),
        0x00_23 => op_0023(arg,cpu,mmu),
        0x00_ea => op_00ea(arg,cpu,mmu),
        0x00_3d => op_003d(arg,cpu,mmu),
        0x00_28 => op_0028(arg,cpu,mmu),
        0x00_0d => op_000d(arg,cpu,mmu),
        0x00_18 => op_0018(arg,cpu,mmu),
        0x00_04 => op_0004(arg,cpu,mmu),
        0x00_1d => op_001d(arg,cpu,mmu),
        0x00_f0 => op_00f0(arg,cpu,mmu),
        0x00_24 => op_0024(arg,cpu,mmu),
        0x00_7c => op_007c(arg,cpu,mmu),
        0x00_f2 => op_00f2(arg,cpu,mmu),
        0x00_90 => op_0090(arg,cpu,mmu),
        0x00_15 => op_0015(arg,cpu,mmu),
        0x00_17 => op_0017(arg,cpu,mmu),
        0x00_c5 => op_00c5(arg,cpu,mmu),
        0xcb_11 => op_cb11(arg,cpu,mmu),
        0x00_c1 => op_00c1(arg,cpu,mmu),
        0x00_c9 => op_00c9(arg,cpu,mmu),

        _ => {
            panic!("unknown op code {:04x}:  {:?}",code, cpu);
        }
    }
}

fn op_0028(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}

fn op_000d(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}

fn op_0023(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,8)
}

fn op_0022(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,8)
}

fn op_0013_inc_de(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,8)
}

fn op_00cd_call_a16(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (3,24)
}

fn op_001a_LD_A_DE(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,8)
}
fn op_007b_LD_A_E(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_00fe_CP_d8(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_0005(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_00ea(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (3,16)
}
fn op_003d(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_002e(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_0018(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,12)
}
fn op_0004(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_001e(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_00f0(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,12)
}
fn op_001d(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_0024(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_007c(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_00f2(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}


fn op_0090(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_0015(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_0017(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}

fn op_00c5(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.sp = cpu.r.sp -1;
    mmu.write8(cpu.r.sp,cpu.r.b);
    cpu.r.sp = cpu.r.sp -1;
    mmu.write8(cpu.r.sp,cpu.r.c);
    (1,16)
}
fn op_cb11(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_00c1(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,12)
}
fn op_00c9(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,16)
}


fn op_to_name(op: u16) -> &'static str {
    match op {
        0x00_AF => "XOR A",
        0x00_32 => "LD (HL-) A",
        0xCB_7C => "BIT 7 H",
        0xCB_11 => "RL C",
        0x00_20 => "JR NZ.+",
        0x00_e2 => "LD (C),A",
        0x00_0c => "INC C",
        0x00_77 => "LD (HL) A",
        0x00_e0 => "LDH (a8),A",
        0x00_1a => "LD A,(DE)",
        0x00_cd => "CALL a16",
        0x00_13 => "INC DE",
        0x00_7b => "LD A,E",
        0x00_fe => "CP d8",
        0x00_05 => "DEC B",
        0x00_22 => "LD (HL+),A",
        0x00_23 => "INC HL",
        0x00_ea => "LD (a16), A",
        0x00_3d => "DEC A",
        0x00_28 => "JR Z, r8",
        0x00_0d => "DEC C",
        0x00_18 => "JR r8",
        0x00_04 => "INC B",
        0x00_f0 => "LDH A,(a8)",
        0x00_1d => "DEC E",
        0x00_24 => "INC H",
        0x00_7c => "LD A,H",
        0x00_f2 => "LD A,(C)",


        0x00_90 => "SUB B",
        0x00_15 => "DEC D",
        0x00_c5 => "PUSH BC",
        0x00_17 => "RLA",
        0x00_c1 => "POP BC",
        0x00_c9 => "RET",
        _ => {
            panic!("unknown op code {:04x}:",op);
        }
    }
}


fn main() {
    let mut cpu = Z80::init();
    let mut mmu = MMU::init();
    cpu.reset();

    //by following along from
    // https://realboyemulator.wordpress.com/2013/01/03/a-look-at-the-game-boy-bootstrap-let-the-fun-begin/

    // starting at the beginning of the boot rom
    // 0x0000 -> 0x31, 0xFE, 0xFF,  -> LD SP, $0xFFFE
    assert_eq!(cpu.r.pc,0);
    execute(&mut cpu,&mut mmu);
    assert_eq!(cpu.r.sp,0xFFFE);
    assert_eq!(cpu.r.pc,3);

    // 0x0003 -> 0xAF -> XOR A
    execute(&mut cpu, &mut mmu);
    assert_eq!(cpu.r.a,0);

    // 0x0004 -> 0x21, 0xFF, 0x9F -> LD HL 16
    // load 0x9FFF into the HL register
    execute(&mut cpu, &mut mmu);
    assert_eq!(cpu.r.get_hl(),0x9FFF);

    // 0x0007 -> LD (HL-),A
    // load register A to the memory address pointed to by HL
    // meaning, write 0 to 0x9FFF
    // then decrement HL
    execute(&mut cpu, &mut mmu);
    assert_eq!(mmu.data[0x9FFF],0);
    assert_eq!(cpu.r.get_hl(),0x9FFE);

    // 0x0008 -> BIT 7, H
    // test MSB of H, set or clear ZERO flag
    execute(&mut cpu, &mut mmu);
    // zero flag should be cleared
    assert_eq!(cpu.r.zero_flag,false);

    //0x000A -> JRNZ .+0xfb
    //jump if not zero to the address 0xFB relative to the current address
    // 0xFB should be interpreted as signed
    // so jump to 0x000C - 0x0005 = 0x0007
    execute(&mut cpu, &mut mmu);
    assert_eq!(cpu.r.pc, 0x0007);

    //loop util PC equals 0x000C
    while cpu.r.pc != 0x000C {
        execute(&mut cpu, &mut mmu);
    }
    // setup audio device stuff
    println!("setting up audio device");
    execute(&mut cpu, &mut mmu); //0x000C  LD HL $0xFF26  # load 0xFF26 into HL
    execute(&mut cpu, &mut mmu); //0x000F  LD C, $0x11    # load 0x11 into C
    assert_eq!(cpu.r.c,0x11);
    execute(&mut cpu, &mut mmu); //0x0011  LD A, $0x80    # load 0x80 into A
    assert_eq!(cpu.r.a,0x80);
    execute(&mut cpu, &mut mmu); //0x0013  LD (HL-), A # load A to address pointed to by HL and Dec HL
    assert_eq!(mmu.data[0xFF26],0x80);
    execute(&mut cpu, &mut mmu); //0x0014  LD ($0xFF00+C), A # load A to address 0xFF00+C (0xFF11)
    assert_eq!(mmu.data[0xFF11],0x80);
    execute(&mut cpu, &mut mmu); //0x0015 – INC C # increment C register
    execute(&mut cpu, &mut mmu); //0x0016 – LD A, $0xF3 # load 0xF3 to A
    assert_eq!(cpu.r.a,0xf3);
    execute(&mut cpu, &mut mmu); //0x0018 – LD ($0xFF00+C), A # load A to address 0xFF00+C (0xFF12)
    assert_eq!(mmu.data[0xff12],0xf3);
    execute(&mut cpu, &mut mmu); //0x0019 – LD (HL-), A # load A to address pointed to by HL and Dec HL
    execute(&mut cpu, &mut mmu); //0x001A – LD A, $0x77 # load 0x77 to A
    execute(&mut cpu, &mut mmu); //0x001C – LD (HL), A # load A to address pointed to by HL

    println!("setup palette");
    execute(&mut cpu, &mut mmu); // 0x001D  LD A, $0xFC  # A represents the color number's mapping
    assert_eq!(cpu.r.a,0xFC);
    execute(&mut cpu, &mut mmu); // 0x001F  LD (0xFF00 + 0x47), A #initialize the palette
    assert_eq!(mmu.data[0xFF47],0xFC);
    execute(&mut cpu, &mut mmu); // 0x0021  LD DE 0x0104 # pointer to Nintendo logo
    execute(&mut cpu, &mut mmu); // 0x0024  LD HL 0x8010 # pointer to vram
    assert_eq!(cpu.r.get_hl(),0x8010);
    execute(&mut cpu, &mut mmu); // 0x0027  LD A, (DE) # load next byte from Nintendo logo
    execute(&mut cpu, &mut mmu); // 0x0028  CALL $0x0095 # decompress, scale and write pixels to VRAM (1)
    execute(&mut cpu, &mut mmu); // 0x002B  CALL $0x0096 # decompress, scale and write pixels to VRAM (2)
    execute(&mut cpu, &mut mmu); // 0x002E  INC DE # advance pointer
    execute(&mut cpu, &mut mmu); // 0x002F – LD A, E # …
    execute(&mut cpu, &mut mmu); // 0x0030 – CP $0x34 # compare accumulator to 0x34
    execute(&mut cpu, &mut mmu); // 0x0032 – JRNZ .+0xf3 # loop if not finished comparing
    execute(&mut cpu, &mut mmu); // 0x0034 – LD DE, $0x00D8 # …
    execute(&mut cpu, &mut mmu); // 0x0037 – LD B, $0x8 # …
    execute(&mut cpu, &mut mmu); // 0x0039 – LD A, (DE) # …
    execute(&mut cpu, &mut mmu); // 0x003A – INC DE # …
    execute(&mut cpu, &mut mmu); // 0x003B – LD (HL+), A # …
    execute(&mut cpu, &mut mmu); // 0x003C – INC HL # …
    execute(&mut cpu, &mut mmu); // 0x003D – DEC B # …
    execute(&mut cpu, &mut mmu); // 0x003E – JRNZ .+0xf9 # jump if not zero to 0x0039
    println!("tile loading?");
    for n in 0..11 {
        execute(&mut cpu, &mut mmu);
    }

    println!("scrolling");
    for n in 0..6 {
        execute(&mut cpu, &mut mmu);
    }
    println!("the LCD should be on now");
    // assert_eq!(&mut hardware.lcd.on,true);
    assert_eq!(mmu.data[0xFF42],0x64); // vertical scroll register
    assert_eq!(cpu.r.a,0x91);
    assert_eq!(mmu.data[0xFF40],0x91); // turn on the LCD display
}



// INC C
fn op_000c_INC_C(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    // println!("INC C");
    cpu.r.c += 1;
    (1,8)
}

// LD (C),A
fn op_00e2_LD_CA(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    let addr:u16 = ((0xFF00 as u16) + (cpu.r.c as u16)) as u16;
    mmu.write8(addr, cpu.r.a);
    (1,8)
}

// JUMP if not zero to the address
fn op_0020_JR_NZ_r8(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    if !cpu.r.zero_flag {
        let off = mmu.read8(cpu.r.pc+1);
        //convert off to i8 then i32 so it will be interpreted as signed
        let addr = ((cpu.r.pc+2) as i32) + (off as i8 as i32);
        //subtract off an extra two because the CPU will automatically move us forward two
        cpu.r.pc =  (addr - 2) as u16;
    }
    (2,12)
}


// XOR A
fn op_00AF_XOR_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.a.bitxor_assign(cpu.r.a);
    (1,4)
}
//16bit register loads

// load register A into memory pointed at by HL, then decrement HL
fn op_0032_LD_HLm_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    mmu.write8(cpu.r.get_hl(),cpu.r.a);
    cpu.r.set_hl(cpu.r.get_hl()-1);
    (1,8)
}
// load register A into memory pointed at by HL
fn op_0077_LD_HL_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    mmu.write8(cpu.r.get_hl(),cpu.r.a);
    (1,8)
}
// BIT 7,H
fn op_CB76_BIT_7_H(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.zero_flag = !((cpu.r.h & 0b1000_0000) > 0);
    (2,8)
}

fn execute(cpu: &mut Z80, mmu: &mut MMU) {
    // println!("PC at {:04x}",cpu.r.pc);
    let (opcode, off) = fetch_opcode_from_memory(cpu, mmu);
    // println!("op {:0x} arg {:0x}", opcode, off);
    let (off,size_of_inst) = decode(opcode, off, cpu, mmu);
    // println!("off is {}",off);
    cpu.r.pc = cpu.r.pc.wrapping_add(off as u16);
    // println!("inst size was {}",size_of_inst);
    // cpu.r.pc = cpu.r.pc.wrapping_add(size_of_inst as u16);
}

