#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unused)]

mod cpu;
mod bootrom;
mod mmu;
mod opcodes;
mod debugger;
mod common;
mod screen;
mod ppu;

use std::fmt::Debug;
use std::path::PathBuf;
use std::{fs, thread};
use std::io::Result;
use std::num::ParseIntError;
use std::time::Duration;
use log4rs::append::file::FileAppender;
use log4rs::Config;
use log4rs::config::{Appender, Root};
use log::{debug, info, LevelFilter};
use structopt::StructOpt;
use common::RomFile;
use crate::cpu::Z80;
use crate::debugger::start_debugger;
use crate::mmu::MMU;
use crate::screen::ScreenSettings;

fn main() -> Result<()>{
    let args = init_setup();
    println!("runnnig with args {:?}",args);
    if let Some(ref pth) = args.romfile {
        println!("loading the romfile {:?}", pth.as_path());
        if let Ok(cart) = load_romfile(pth) {
            run_romfile(cart, &args)?;
        }
    } else {
        println!("you must specify a rom file");
    }
    Ok(())
}

fn run_romfile(cart: RomFile, args:&Cli) -> Result<()>{
    println!("running the cart {:?}", cart.data.len());
    let mut cpu = Z80::init();
    let mut mmu = MMU::init(&cart.data);
    cpu.reset();
    if args.boot {
        mmu.overlay_boot();
        cpu.r.pc = 0x0000;
    } else {
        cpu.r.pc = 0x100;
    }

    let settings = ScreenSettings {
        x: args.x,
        y: args.y,
        scale: args.scale,
        enabled: args.screen
    };
    start_debugger(cpu, mmu, Some(cart), args.fastforward, &settings, args.breakpoint,  args.interactive)?;
    Ok(())
}

fn load_romfile(pth: &PathBuf) -> Result<RomFile> {
    let pth2:String = pth.as_path().to_str().unwrap().parse().unwrap();
    let data:Vec<u8> = fs::read(pth)?;
    println!("0x0104. start of Nintendo graphic {:02X} {:02X} (should be CE ED)",data[0x0104],data[0x0105]);
    print!("name = ");
    for ch in 0x0134 .. 0x0142 {
        print!("{}", char::from_u32(data[ch] as u32).unwrap());
    }
    println!("   ");
    // println!("0x0134. start of name {:?}",data[0x0134..0x0142]);
    println!("0x0143 color or not {:02x}",data[0x0143]);
    println!("0x0146 SGB indicator {:02x}",data[0x0146]);
    println!("0x0147 cart type {:02x}",data[0x0147]);
    println!("0x0148 ROM size {:02x}",data[0x0148]);
    println!("0x0149 RAM size {:02x}",data[0x0149]);
    println!("0x014A dest code {:02x}",data[0x014A]);

    let cart_type = data[0x0147];
    match cart_type {
        0x0 => println!("ROM only. Great!"),
        0x1..=0x3 => println!("MBC1! Not supported!"),
        0x5|0x6 => println!("MBC2! Not supported!"),
        0x12|0x13 => println!("MBC3! Not supported!"),
        0x19|0x1A|0x1B|0x1C|0x1D|0x1E => println!("MBC5! Not supported!"),
        0x1F => println!("Bocket Camera, unsupported!"),
        0xFD => println!("Bocket Camera, unsupported!"),
        0xFE|0xFF => println!("Hudson HuC, unsupported!"),
        _ => {
            println!("trying anyway");
        }
    }
    Ok(RomFile {
        data,
        path: pth2,
    })
}

fn parse_hex(src:&str) -> std::result::Result<u16, ParseIntError> {
    u16::from_str_radix(src,16)
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
    #[structopt(long)]
    verbose:bool,
    #[structopt(long, parse(try_from_str = parse_hex), default_value="0")]
    breakpoint:u16,
    #[structopt(long)]
    screen:bool,
    #[structopt(long, default_value="100")]
    x:i32,
    #[structopt(long, default_value="100")]
    y:i32,
    #[structopt(long, default_value="1")]
    scale:f32,
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
    args
}
