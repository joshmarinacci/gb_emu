use std::collections::{HashMap, HashSet};
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
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::time::Duration;
use structopt::StructOpt;
use gb_emu::common::{get_bit, get_bit_as_bool, InputEvent, JoyPadKey};
use gb_emu::mmu2::IORegister;
use gb_emu::screen::{Screen, ScreenSettings};

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
fn main() -> Result<()>{
    let args = init_setup();
    if let None = args.romfile {
        println!("you must specify a rom file");
        return Ok(());
    }
    let pth = args.romfile.unwrap();
    println!("loading the romfile {:?}", pth.as_path());
    let str = pth.to_str().unwrap().to_string();
    let mut gb = setup_test_rom(&str).unwrap();
    let sss = gb.ppu.sss.clone();


    let settings = ScreenSettings {
        x: args.x,
        y: args.y,
        scale: args.scale,
        enabled: args.screen,
    };

    let (to_screen, receive_screen) = channel::<String>();
    let (to_cpu, receive_cpu) = channel::<InputEvent>();

    let hand = thread::spawn(move || {
        if args.run {
            start_run(&mut gb, to_screen, receive_cpu).unwrap();
        } else {
            start_debugger(&mut gb, args.fastforward, to_screen, receive_cpu, args.print_reg).unwrap();
        }
    });

    if settings.enabled {
        let mut screen_obj = Screen::init(&settings,sss);
        screen_obj.update_screen();
        loop {
            if !screen_obj.process_input(&to_cpu) {
                break;
            }
            // screen_obj.update_screen();
            if let Ok(str) = receive_screen.try_recv() {
                // println!("screen got message {}",str);
                screen_obj.update_screen();
            }
        }
    } else {
        hand.join().unwrap();
    }

    Ok(())
}

fn start_run(gb: &mut GBState, to_screen: Sender<String>, receive_cpu: Receiver<InputEvent>) -> Result<()> {
    gb.set_pc(0x100);
    gb.mmu.write8_IO(&IORegister::LCDC,0x00);
    gb.mmu.write8_IO(&IORegister::STAT, 0x00);
    // gb.mmu.write8_IO(IORegister::LY,0x00);
    gb.mmu.write8_IO(&IORegister::LYC,0x00);

    loop {
        gb.execute();
        if gb.ppu.entered_vram {
            to_screen.send(String::from("redraw"));
            ::std::thread::sleep(Duration::from_millis(1000 / 60));
        }
        if let Ok(evt) = receive_cpu.try_recv() {
            match evt {
                InputEvent::Press(JoyPadKey::A) => {    gb.mmu.joypad.a = true;  }
                InputEvent::Release(JoyPadKey::A) => {  gb.mmu.joypad.a = false; }
                InputEvent::Press(JoyPadKey::B) => {    gb.mmu.joypad.b = true;  }
                InputEvent::Release(JoyPadKey::B) => {  gb.mmu.joypad.b = false; }
                InputEvent::Press(JoyPadKey::Start) => { gb.mmu.joypad.start = true; }
                InputEvent::Release(JoyPadKey::Start) => { gb.mmu.joypad.start = false; }
                InputEvent::Press(JoyPadKey::Select) => {  gb.mmu.joypad.select = true; }
                InputEvent::Release(JoyPadKey::Select) => { gb.mmu.joypad.select = false;  }
                InputEvent::Press(JoyPadKey::Left) => {   gb.mmu.joypad.left = true;   }
                InputEvent::Release(JoyPadKey::Left) => { gb.mmu.joypad.left = false;  }
                InputEvent::Press(JoyPadKey::Right) => {  gb.mmu.joypad.right = true;  }
                InputEvent::Release(JoyPadKey::Right) => { gb.mmu.joypad.right = false; }
                InputEvent::Press(JoyPadKey::Up) => {      gb.mmu.joypad.up = true;     }
                InputEvent::Release(JoyPadKey::Up) => {    gb.mmu.joypad.up = false;    }
                InputEvent::Press(JoyPadKey::Down) => {    gb.mmu.joypad.down = true;   }
                InputEvent::Release(JoyPadKey::Down) => {  gb.mmu.joypad.down = false;  }
                // InputEvent::JumpNextVBlank() => {
                //     if ctx.interactive {
                //         println!("got a jump next vblank");
                //         ctx.jump_to_next_vblank(&mut term, &mut ss1).unwrap();
                //     }
                // }
                InputEvent::Stop() => {
                    println!("emu stopping. current status is");
                    gb.dump_current_state();
                    break;
                },
                _ => {
                    println!("unhandled event {:?}", evt);
                }
            }
        }
    }
    Ok(())
}

fn start_debugger(gb: &mut GBState, fastforward: u32, to_screen: Sender<String>, receive_cpu: Receiver<InputEvent>, print_reg: String) -> Result<()> {
    gb.set_pc(0x100);
    gb.mmu.write8_IO(&IORegister::LCDC,0x00);
    gb.mmu.write8_IO(&IORegister::STAT, 0x00);
    gb.mmu.write8_IO(&IORegister::LY,0x00);
    gb.mmu.write8_IO(&IORegister::LYC,0x00);
    let term = Term::stdout();

    let regs = print_reg.split(",").filter(|s|!s.trim().is_empty()).collect::<Vec<&str>>();

    for reg in regs {
        // println!("reg is {}",reg);
        match IORegister::match_name(reg) {
            Some(reg) => {
                println!("monitoring for writing to register {:?}",reg);
                gb.mmu.debug_registers.insert(reg);
            }
            None => {
                println!("cannot find register with name {}",reg);
                panic!("cannot monitor unknown register {}",reg);
            }
        }
    }



    // fast forward by however much is specified
    gb.execute_n(fastforward as usize);

    loop {
        let commands = Style::new().reverse();
        term.write_line(
            &commands
                .apply_to("========================================")
                .to_string(),
        )?;


        // status
        term.write_line(&format!(
            "PC: {:04x}  SP:{:04x}    clock={}  PPU clock={}",
            gb.cpu.get_pc(),
            gb.cpu.get_sp(),
            gb.clock,
            gb.ppu.next_clock,
        ))?;
        //current instruction
        let code = gb.fetch_opcode_at(gb.get_pc());
        // println!("current op {:04x}", code);
        if let Some(opx) = gb.lookup_op(&code) {
            let op: Op = (*opx).clone();
            println!("instr {:02x}  ->  {}  ->  {}",code, op.to_asm(), op.real(&gb));
        }
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
            "IME = {} IF = {:08b} IE = {:08b}  LCDC: {:08b}   STAT: {:08b}",
            gb.cpu.IME,
            gb.mmu.read8_IO(&IORegister::IF),
            gb.mmu.read8_IO(&IORegister::IE),
            gb.mmu.read8_IO(&IORegister::LCDC),
            gb.mmu.read8_IO(&IORegister::STAT),
        ))?;
        term.write_line(&format!(
            "LY = {}  LYC {}   SCY {} SCX {}   WY {} WX {} ",
            gb.mmu.read8_IO(&IORegister::LY),
            gb.mmu.read8_IO(&IORegister::LYC),
            gb.mmu.read8_IO(&IORegister::SCY),
            gb.mmu.read8_IO(&IORegister::SCX),
            gb.mmu.read8_IO(&IORegister::WY),
            gb.mmu.read8_IO(&IORegister::WX),
        ))?;

        //current lcd mode
        //is lcd on
        //which interrupts are set on the stat register
        term.write_line(&format!(
            "LCD {}   mode = {:?}  hi={}, vi={} spi={} sci={}",
            gb.mmu.lcdc.enabled,
            gb.mmu.stat.mode,
            gb.mmu.stat.hblank_interrupt_enabled,
            gb.mmu.stat.vblank_interrupt_enabled,
            gb.mmu.stat.sprite_interrupt_enabled,
            gb.mmu.stat.scanline_match_interrupt_enabled,
        ));

        let IE = gb.mmu.read8_IO(&IORegister::IE);
        let IF = gb.mmu.read8_IO(&IORegister::IE);
        term.write_line(&format!(
            "interr: IE = {:08b} vblank {}   lcd stat {}   timer {}   serial {}   joy {} ",
            IE,
            get_bit(IE,0),
            get_bit(IE,1),
            get_bit(IE,2),
            get_bit(IE,3),
            get_bit(IE,4),
        ));

        term.write_line(&format!(
            "timer: DIV = {}  TIMA {}   TMA {} TAC {} ",
            gb.mmu.read8_IO(&IORegister::DIV),
            gb.mmu.read8_IO(&IORegister::TIMA),
            gb.mmu.read8_IO(&IORegister::TMA),
            gb.mmu.read8_IO(&IORegister::TAC),
        ))?;


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
            'i' => request_interrupt(gb, &term)?,
            'v' => dump_vram_png(gb, &term)?,
            'g' => gb.execute_n(100_000_000),
            'V' => gb.run_to_vblank(),
            'q' => break,
            _ => println!("??"),
        };

        if gb.ppu.entered_vram {
            to_screen.send(String::from("redraw"));
        }
    }
    Ok(())
}

fn dump_vram_png(gb: &mut GBState, term: &Term) -> Result<()> {
    term.write_line("drawing to vram.png")?;
    gb.draw_full_screen();
    {
        let sss = gb.ppu.sss.lock().unwrap();
        sss.backbuffer.write_to_file("vram.png");
    }
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
            start:0xC000,
            len:0x800,
            name:"User Ram ",
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
    term.write_line(&format!(
        "chose range {} {:04x} len{:04x}",
        range.name, range.start, range.len
    ))?;
    // println!("total memory len {}",gb.mmu.data.len());
    let data = gb.mmu.borrow_slice(range.start, (range.start + range.len));
    for (n, chunk) in data.chunks(32 * 2).enumerate() {
        let line_str: String = chunk.iter().map(|b| format!("{:02x}", b)).collect();
        term.write_line(&format!("{:04X} {}", (n * 32 * 2) + range.start, line_str))?;
    }
    Ok(())
}

fn request_interrupt(gb: &mut GBState, term: &Term) -> Result<()> {
    let selections = ["vblank","enable all"].to_vec();
    let selection = dialoguer::Select::with_theme(&ColorfulTheme::default())
        .with_prompt("request interrupt")
        .default(0)
        .items(&selections[..])
        .interact()
        .unwrap();
    // let sel = &selections[selection];
    if selection == 0 {
        term.write_line("requesting a vblank interrupt")?;
        gb.mmu.set_IO_bit(&IORegister::IE,0,true);
        gb.mmu.set_IO_bit(&IORegister::IF,1,true);
        term.write_line("will fire after the next instruction")?;
    }
    if selection == 1 {
        term.write_line("enabling all")?;
        gb.cpu.IME = true;
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
    #[structopt(long)]
    run:bool,
    #[structopt(long, default_value="0")]
    fastforward:u32,

    #[structopt(long, default_value="")]
    print_reg:String,

    // #[structopt(long, parse(try_from_str = parse_hex), default_value="0")]
    // breakpoint:u16,
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
    for _ in 0..5 {
        info!("        ");
    }
    info!("==============");
    info!("starting new run");
    info!("running with args {:?}", args);
    args
}
