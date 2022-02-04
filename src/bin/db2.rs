use console::Color::White;
use console::{Color, Style, Term};
use dialoguer::theme::ColorfulTheme;
use gb_emu::optest::{setup_test_rom, GBState, Op};
use log::{info, LevelFilter};
use log4rs::append::file::FileAppender;
use log4rs::config::{Appender, Root};
use log4rs::Config;
use std::io::Result;
use std::path::PathBuf;
use std::thread;
use std::time::Duration;
use structopt::StructOpt;
use gb_emu::mmu2::IORegister;

/*

new debugger

choose rom to load

default is interactive

//press j to jump ahead 1
//press J to jump ahead 16
press l to jump ahead to the end of the loop (when the next conditional jump fails so you go past it)
press u to jump ahead 256
press U to jump ahead 256*16
press m to view memory:
    particular blocks of memory
    or the range around the current PC
    or the current stack
press v to dump current VRAM to an image by drawing
press t to turn on and off logging of
    interrupts fired
    io registers written to
    io registers read from
always show the status of the current registers:
    PC & current op & SP
    A,B,C etc.HL, DE, etc.
    IO regs: LCDC, STAT, BGP, LY, SCX, SCY, IE, IME,

 */
fn main() -> Result<()> {
    let args = init_setup();
    if let None = args.romfile {
        println!("you must specify a rom file");
        return Ok(());
    }
    let pth = args.romfile.unwrap();
    println!("loading the romfile {:?}", pth.as_path());
    let str = pth.to_str().unwrap().to_string();
    let mut gb = setup_test_rom(&str).unwrap();
    gb.set_pc(0x100);
    let mut term = Term::stdout();
    loop {
        let commands = Style::new().reverse();
        term.write_line(
            &commands
                .apply_to("========================================")
                .to_string(),
        )?;


        // status
        term.write_line(&format!(
            "PC: {:04x}  SP:{:04x}    clock={}  cycles={}",
            gb.cpu.get_pc(),
            gb.cpu.get_sp(),
            gb.clock,
            gb.count
        ))?;
        //registers
        let reg_style = Style::new().bg(Color::White).red().underlined();
        let regs = gb.cpu.reg_to_str();
        term.write_line(&reg_style.apply_to(regs).to_string())?;
        //flags
        let flag_style = Style::new().blue();
        term.write_line(&format!(
            "flags Z:{}   N:{}  H:{}  C:{}",
            flag_style.apply_to(gb.cpu.r.zero),
            flag_style.apply_to(gb.cpu.r.subn),
            flag_style.apply_to(gb.cpu.r.half),
            flag_style.apply_to(gb.cpu.r.carry)
        ))?;
        //IO status bits
        term.write_line(&format!(
            "IME = {}",
            gb.cpu.IME,
        ))?;

        //current instruction
        let code = gb.fetch_opcode_at(gb.get_pc());
        println!("current op {:04x}", code);
        if let Some(opx) = gb.lookup_op(&code) {
            let op: Op = (*opx).clone();
            println!("instr {:02x}  ->  {}  ->  {}",code, op.to_asm(), op.real(&gb));
        }


            let commands = Style::new().reverse();
        term.write_line(
            &commands
                .apply_to(&format!("j=step J=16 u=256 U=4096 "))
                .to_string(),
        )?;
        term.write_line(&commands.apply_to(&format!("m=memory  q=quit")).to_string())?;

        let ch = term.read_char().unwrap();
        match ch {
            'j' => gb.execute_n(1),
            'J' => gb.execute_n(16),
            'u' => gb.execute_n(256),
            'U' => gb.execute_n(256 * 16),
            'm' => dump_memory(&gb, &term)?,
            'i' => request_interrupt(&mut gb, &term)?,
            'v' => dump_vram_png(&mut gb, &term)?,
            'g' => gb.execute_n(100_000_000),
            'q' => break,
            _ => println!("??"),
        };
    }
    Ok(())
}

fn dump_vram_png(gb: &mut GBState, term: &Term) -> Result<()> {
    term.write_line("drawing to vram.png")?;
    gb.draw_full_screen();
    gb.ppu.backbuffer.write_to_file("vram.png");
    Ok(())
}

#[derive(Debug)]
struct RamChunk {
    start: usize,
    len: usize,
    name: &'static str,
}

fn dump_memory(gb: &GBState, term: &Term) -> Result<()> {
    let ranges = [
        RamChunk {
            start: 0x8000,
            len: 0x800,
            name: "Tile Data Block 0",
        },
        RamChunk {
            start: 0x8800,
            len: 0x800,
            name: "Tile Data Block 1",
        },
        RamChunk {
            start: 0x9000,
            len: 0x800,
            name: "Tile Data Block 2",
        },
        RamChunk {
            start: 0x9800,
            len: 0x400,
            name: "Tile Map Block 0",
        },
        RamChunk {
            start: 0x9C00,
            len: 0x400,
            name: "Tile Map Block 1",
        },
        RamChunk {
            start: 0xFE00,
            len: 0xA0,
            name: "OAM: sprite attribute table",
        },
        RamChunk {
            start: 0xFF80,
            len: 0x80,
            name: "High RAM",
        },
    ];
    let mut selections: Vec<String> = vec![];
    for reg in &ranges {
        selections.push(reg.name.to_string());
    }
    let selection = dialoguer::Select::with_theme(&ColorfulTheme::default())
        .with_prompt("view memory section:")
        .default(0)
        .items(&selections[..])
        .interact()
        .unwrap();
    let range = &ranges[selection];
    println!(
        "chose range {} {:04x} len{:04x}",
        range.name, range.start, range.len
    );
    // println!("total memory len {}",gb.mmu.data.len());
    let data = gb.mmu.borrow_slice(range.start, (range.start + range.len));
    for (n, chunk) in data.chunks(32 * 2).enumerate() {
        let line_str: String = chunk.iter().map(|b| format!("{:02x}", b)).collect();
        println!("{:04X} {}", (n * 32 * 2) + range.start, line_str);
    }
    Ok(())
}

fn request_interrupt(gb: &mut GBState, term: &Term) -> Result<()> {
    let selections = ["vblank"].to_vec();
    let selection = dialoguer::Select::with_theme(&ColorfulTheme::default())
        .with_prompt("request interrupt")
        .default(0)
        .items(&selections[..])
        .interact()
        .unwrap();
    let sel = &selections[selection];
    if selection == 0 {
        term.write_line("requesting a vblank interrupt")?;
        let mut val = gb.mmu.read8_IO(IORegister::IE);
        val = val | 0b0000_0001; //turn on the vblank interrupt
        gb.mmu.write8_IO(IORegister::IE,val);

        let mut val2 = gb.mmu.read8_IO(IORegister::IF);
        val2 = val2 | 0b0000_0001;
        gb.mmu.write8_IO(IORegister::IF,val2);

        term.write_line("will fire after the next instruction");
    }
    Ok(())
}

#[derive(StructOpt, Debug)]
#[structopt(name = "gbemu", about = "gb emulator")]
struct Cli {
    #[structopt(long)]
    debug: bool,
    #[structopt(parse(from_os_str))]
    romfile: Option<PathBuf>,
    // #[structopt(long)]
    // boot:bool,
    // #[structopt(long)]
    // interactive:bool,
    // #[structopt(long, default_value="0")]
    // fastforward:u32,
    // #[structopt(long)]
    // verbose:bool,
    // #[structopt(long, parse(try_from_str = parse_hex), default_value="0")]
    // breakpoint:u16,
    // #[structopt(long)]
    // screen:bool,
    // #[structopt(long, default_value="100")]
    // x:i32,
    // #[structopt(long, default_value="100")]
    // y:i32,
    // #[structopt(long, default_value="1")]
    // scale:f32,
}

fn init_setup() -> Cli {
    let args: Cli = Cli::from_args();
    let loglevel = if args.debug {
        LevelFilter::Debug
    } else {
        LevelFilter::Error
    };

    // create file appender with target file path
    let logfile = FileAppender::builder()
        .build("../../log/output.log")
        .expect("error setting up file appender");

    // make a config
    let config = Config::builder()
        //add the file appender
        .appender(Appender::builder().build("logfile", Box::new(logfile)))
        //now make it
        .build(
            Root::builder()
                .appender("logfile") // why do we need to mention logfile again?
                .build(loglevel),
        )
        .expect("error setting up log file");

    log4rs::init_config(config).expect("error initing config");

    thread::sleep(Duration::from_millis(100));
    println!("logging to log/output.log");
    for i in 0..5 {
        info!("        ");
    }
    info!("==============");
    info!("starting new run");
    info!("running with args {:?}", args);
    args
}
