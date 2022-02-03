#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unused)]

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
use gb_emu::common::{load_romfile, RomFile};
use gb_emu::cpu::Z80;
use gb_emu::debugger::start_debugger;
use gb_emu::mmu::MMU;
use gb_emu::screen::ScreenSettings;

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
        .build("../../log/output.log").expect("error setting up file appender");

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
