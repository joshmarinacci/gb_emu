use std::{io, thread};
use console::{Color, Style, Term};
use io::Result;
use std::sync::{Arc, Mutex, MutexGuard};
use std::sync::mpsc::{channel, Receiver, Sender};
use console::Color::{Black, Red, White};
use log::{debug, info};
use Load::Load_R_u8;
use crate::{common, MMU, opcodes, ScreenSettings, Z80};
use crate::common::{Bitmap, get_bit_as_bool, RomFile};
use crate::mmu::{TEST_ADDR, TIMER_INTERRUPT_ADDR};
use crate::opcodes::{Compare, DoubleRegister, Instr, Jump, Load, lookup_opcode, Math, RegisterName, Special, u8_as_i8};
use crate::opcodes::Compare::CP_A_r;
use crate::opcodes::DoubleRegister::BC;
use crate::opcodes::Instr::{LoadInstr, SpecialInstr};
use crate::opcodes::RegisterName::{A, B, C, D};
use crate::ppu::{PPU, ScreenState};
use crate::screen::Screen;

struct Ctx {
    cpu:Z80,
    mmu:MMU,
    clock:u32,
    last_cpu_clock:u32,
    running:bool,
    interactive:bool,
    cart: Option<RomFile>,
    full_registers_visible: bool,
    test_memory_visible: bool,
    show_interrupts:bool,
    last_status:String,
    ppu: PPU,
    screen_enabled:bool,
}

impl Ctx {
    fn make_test_context(rom: &[u8]) -> Ctx {
        Ctx {
            cpu:Z80::init(),
            ppu:PPU::init(),
            mmu: MMU::init(rom),
            clock: 0,
            last_cpu_clock: 0,
            running: true,
            interactive: false,
            cart: None,
            full_registers_visible: false,
            test_memory_visible: false,
            show_interrupts: false,
            last_status: String::from("--nothing--"),
            screen_enabled: false
        }
    }
}

impl Ctx {
    pub(crate) fn execute(&mut self, term: &mut Term, verbose: bool, ss: &mut Arc<Mutex<ScreenState>>, to_screen: &Sender<String>, receive_cpu: &Receiver<InputEvent>) -> Result<()>{
        let (opcode, _off) = fetch_opcode_from_memory(&self.cpu, &self.mmu);
        if verbose {
            term.write_line(&format!("--PC at {:04x}  op {:0x}", self.cpu.r.pc, opcode))?;
        }
        if let Some(instr) = lookup_opcode(opcode) {
            self.execute_instruction(&instr);
        } else {
            term.write_line(&format!("current cycle {}", self.clock))?;
            println!("unknown op code {:04x}",opcode);
            println!("current memory is PC {:04x} ",self.cpu.get_pc());
            panic!("unknown op code");
        }
        self.mmu.update(&mut self.cpu, ss, &mut self.clock);
        self.ppu.update(&mut self.mmu, ss, &mut self.clock, to_screen, receive_cpu, self.screen_enabled);
        self.clock+=1;
        Ok(())
    }

    fn execute_instruction(&mut self, inst: &Instr) {
        match inst {
            Instr::SpecialInstr(special)  => opcodes::execute_special_instructions(&mut self.cpu, &mut self.mmu, special),
            Instr::LoadInstr(load)         => opcodes::execute_load_instructions(&mut self.cpu, &mut self.mmu, load),
            Instr::CompareInst(comp)   => opcodes::execute_compare_instructions(&mut self.cpu, &mut self.mmu, comp),
            Instr::MathInst(math)         => opcodes::execute_math_instructions(&mut self.cpu, &mut self.mmu, math),
            Instr::JumpInstr(jump)         => opcodes::execute_jump_instructions(&mut self.cpu, &mut self.mmu, jump),
        }
    }
}

#[derive(Debug)]
pub enum JoyPadKey {
    A,
    B,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right
}
#[derive(Debug)]
pub enum InputEvent {
    Press(JoyPadKey),
    Release(JoyPadKey),
    Stop(),
}

pub fn start_debugger(cpu: Z80, mmu: MMU, cart: Option<RomFile>,
                      fast_forward: u32,
                      screen_settings:&ScreenSettings,
                      breakpoint:u16,
                      verbose:bool,
                      interactive:bool) -> Result<()> {
    let screen_state = ScreenState::init();
    let mut shared_screen_state = Arc::new(Mutex::new(ScreenState::init()));
    let mut ctx = Ctx {
        cpu, mmu,
        ppu:PPU::init(),
        clock:0,
        last_cpu_clock: 0,
        running:true,
        interactive,
        cart,
        full_registers_visible:false,
        test_memory_visible: false,
        show_interrupts: false,
        last_status: "".to_string(),
        screen_enabled: screen_settings.enabled,
    };
    let mut term = Term::stdout();

    println!("fast forwarding: {}",fast_forward);

    let (to_screen,receive_screen) = channel::<String>();
    let (to_cpu, receive_cpu) = channel::<InputEvent>();

    for n in 0..fast_forward {
        ctx.execute(&mut term, false, &mut shared_screen_state, &to_screen, &receive_cpu )?;
    }
    let mut ss1 = shared_screen_state.clone();
    let ss2 = shared_screen_state.clone();
    let screen_enabled = screen_settings.enabled;
    let hand = thread::spawn(move | |
        {
        while ctx.running {

            //handle breakpoints
            if breakpoint > 0 && (ctx.cpu.r.pc == breakpoint) {
                println!("done hit the breakpoint");
                ctx.interactive = true;
            }

            //step forward or execute next CPU clock cycle
            if ctx.interactive {
                step_forward(&mut ctx, &mut term, &mut ss1, &to_screen, &receive_cpu).unwrap();
            } else {
                ctx.execute(&mut term, verbose, &mut ss1, &to_screen, &receive_cpu ).unwrap();
                if let Ok(evt) = receive_cpu.try_recv() {
                    match evt {
                        InputEvent::Press(JoyPadKey::A) => {        ctx.mmu.joypad.a = true;       }
                        InputEvent::Release(JoyPadKey::A) => {      ctx.mmu.joypad.a = false;      }
                        InputEvent::Press(JoyPadKey::B) => {        ctx.mmu.joypad.b = true;       }
                        InputEvent::Release(JoyPadKey::B) => {      ctx.mmu.joypad.b = false;      }
                        InputEvent::Press(JoyPadKey::Start) => {    ctx.mmu.joypad.start = true;   }
                        InputEvent::Release(JoyPadKey::Start) => {  ctx.mmu.joypad.start = false;  }
                        InputEvent::Press(JoyPadKey::Select) => {   ctx.mmu.joypad.select = true;  }
                        InputEvent::Release(JoyPadKey::Select) => { ctx.mmu.joypad.select = false; }
                        InputEvent::Press(JoyPadKey::Left) => {     ctx.mmu.joypad.left = true;    }
                        InputEvent::Release(JoyPadKey::Left) => {   ctx.mmu.joypad.left = false;   }
                        InputEvent::Press(JoyPadKey::Right) => {    ctx.mmu.joypad.right = true;   }
                        InputEvent::Release(JoyPadKey::Right) => {  ctx.mmu.joypad.right = false;  }
                        InputEvent::Press(JoyPadKey::Up) => {       ctx.mmu.joypad.up = true;      }
                        InputEvent::Release(JoyPadKey::Up)=> {      ctx.mmu.joypad.up = false;     }
                        InputEvent::Press(JoyPadKey::Down) => {     ctx.mmu.joypad.down = true;    }
                        InputEvent::Release(JoyPadKey::Down)=> {    ctx.mmu.joypad.down = false;   }
                        _ => {
                            println!("unhandled event {:?}",evt);
                        }
                    }
                }
            }
        }
    });
    if screen_settings.enabled {
        let mut screen_obj = Screen::init(screen_settings);
        while true {
            if !screen_obj.process_input(&to_cpu) { break; }
            if let Ok(str) = receive_screen.try_recv() {
                screen_obj.update_screen(&shared_screen_state);
                to_cpu.send(InputEvent::Stop());
            }
        }
    } else {
        hand.join();
    }
    Ok(())
}

fn step_forward(ctx: &mut Ctx, term: &mut Term, screenstate: &mut Arc<Mutex<ScreenState>>, to_screen: &Sender<String>, receive_cpu: &Receiver<InputEvent>) -> Result<()>{
    let border = Style::new().bg(Color::Magenta).black();
    // term.clear_screen()?;
    if let Some(cart) = &ctx.cart {
        term.write_line(&format!("executing rom {}", cart.path))?;
    }
    term.write_line(&format!("clock {}",&ctx.clock))?;
    term.write_line(&border.apply_to("========================================").to_string())?;

    // print the current memory
    // {
    //     let start = ctx.cpu.r.pc;
    //     let back: i32 = 2;
    //     for n in 0..5 {
    //         let iv = (start as i32) + n - back;
    //         // println!("n is {} {}",n,iv);
    //         if iv < 0 {
    //             term.write_line("----")?;
    //             continue;
    //         }
    //         let addr = iv as u16;
    //         // println!("address is {:04x}",addr);
    //         let prefix = if addr == start { " *" } else { "  " };
    //         let data = ctx.mmu.read8(addr);
    //         term.write_line(&format!("{} {:04x}  {:02x}", prefix, addr, data))?;
    //     }
    // }

    {

        let s = ctx.cpu.r.pc as f32;
        let pc = ctx.cpu.r.pc as usize;
        let s2 = (s / 16.0).floor() as u32;
        let s3 = (s2 * 16) as usize;
        let mut e = s3 + 16*5 as usize;
        if e >= 0xFFFF {
            e = 0xFFFE;
        }
        let data = &ctx.mmu.data[s3 .. e];
        let nums = 0..16;
        let plain_style = Style::new().bg(White).red();
        let highl_style = Style::new().bg(Black).white().underlined();
        let line_str:String = nums.into_iter().map(|n|(format!("{:02x} ",n))).collect();
        term.write_line(&format!("       {}",line_str));
        for (i, row) in data.chunks(16).enumerate() {
            let abs_off = i*16;
            let line_str:String = row.into_iter().enumerate().map(|(n,v)|{
                let abs = abs_off + n + s3;
                let style = (if abs  == pc { &highl_style  } else { &plain_style });
                style.apply_to(format!("{:02x} ",v)).to_string()
            }).collect();
            // let line_str:String = row.iter()
            //     .map(|b|format!("{:02x} ",b))
            //     .collect();
            term.write_line(&format!("{:04x}   {} ", s3+ i, line_str));
        }
    }

    {
        // print the registers
        let reg_style = Style::new().bg(White).red().underlined();
        term.write_line(&format!("PC: {:04x}  SP:{:04x}", ctx.cpu.r.pc, ctx.cpu.r.sp))?;
        let regs_u8 = format!("A:{:02x}  B:{:02x}  C:{:02x}  D:{:02x}  E:{:02x}  H:{:02x}  L:{:02x} ",
                              ctx.cpu.r.a,
                              ctx.cpu.r.b,
                              ctx.cpu.r.c,
                              ctx.cpu.r.d,
                              ctx.cpu.r.e,
                              ctx.cpu.r.h,
                              ctx.cpu.r.l,
        );
        term.write_line(&reg_style.apply_to(regs_u8).to_string())?;
        let regs_u16 = format!("BC:{:04x}  DE:{:04x}  HL:{:04x}",
                               ctx.cpu.r.get_bc(),
                               ctx.cpu.r.get_de(),
                               ctx.cpu.r.get_hl(),
        );
        term.write_line(&regs_u16)?;
        let flag_style = Style::new().blue();
        term.write_line(&format!("flags Z:{}   N:{}  H:{}  C:{}",
                                 flag_style.apply_to(ctx.cpu.r.zero_flag),
                                 flag_style.apply_to(ctx.cpu.r.subtract_n_flag),
                                 flag_style.apply_to(ctx.cpu.r.half_flag),
                                 flag_style.apply_to(ctx.cpu.r.carry_flag),
        ))?;
        // term.write_line(&format!(" Screen: LY {:02x}",ctx.mmu.hardware.LY))?;
    }
    if ctx.full_registers_visible {
        show_full_hardware_registers(term,ctx)?;
    }
    if ctx.test_memory_visible {
        show_test_memory(term, ctx)?;
    }
    if ctx.show_interrupts {
        dump_interrupts(term, ctx)?;
    }

    // print info about the next opcode
    let primary = Style::new().green().bold();
    let bold = Style::new().reverse().red();
    let italic = Style::new().italic();
    let (opcode, _instr_len) = fetch_opcode(&ctx.cpu, &ctx.mmu);
    let op = lookup_opcode(opcode);
    if let Some(ld) = op {
        term.write_line(&primary.apply_to(&format!("${:04x} : {}: {}",
                                                   ctx.cpu.r.pc,
                                                   bold.apply_to(format!("{:02X}",opcode)),
                                                   italic.apply_to(opcodes::lookup_opcode_info(ld)),
        )).to_string())?;
    } else {
        println!("current cycle {}", ctx.clock);
        panic!("unknown op code")
    }

    println!("{}",ctx.last_status);


    let commands = Style::new().reverse();
    term.write_line(&commands.apply_to(&format!("j=step, J=step 16, r=hdw reg, v=vram dump s=screen dump c=cart_dump o=objram dump")).to_string())?;
    let ch = term.read_char().unwrap();
    match ch{
        'r' => ctx.full_registers_visible = !ctx.full_registers_visible,
        'j' => ctx.execute(term, false, screenstate, to_screen, receive_cpu)?,
        'J' => {
            term.write_line("doing 16 instructions")?;
            for n in 0..16 {
                ctx.execute(term, false, screenstate, to_screen, receive_cpu)?;
            }
        },
        'u' => {
            for n in 0..256 {
                ctx.execute(term, false, screenstate, to_screen, receive_cpu)?;
            }
        },
        'U' => {
            for n in 0..(256*16) {
                ctx.execute(term, false, screenstate, to_screen, receive_cpu)?;
            }
        },
        'c' => dump_cart_rom(term, ctx)?,
        // 'v' => dump_vram(term, ctx)?,
        'v' => {
            println!("running to next frame done");
            while ctx.mmu.hardware.LY > 1 {
                // println!("LY is {}",ctx.mmu.hardware.LY);
                ctx.execute(term, false, screenstate, to_screen, receive_cpu)?;
            }
        }
        'o' => dump_oram(term, ctx)?,
        's' => {
            // screenstate.clear_with(0, 0, 0);
            // draw_vram(&mut ctx.mmu, screenstate);
            // screenstate.write_to_file("./screen.png");
            ctx.last_status = String::from("wrote to screen.png")
        },
        'i' => ctx.show_interrupts = !ctx.show_interrupts,
        't' => ctx.test_memory_visible = !ctx.test_memory_visible,
        'm' => dump_all_memory(term,ctx)?,
        'b' => {
            ctx.interactive = false;
            ctx.execute(term, false, screenstate, to_screen, receive_cpu)?;
        },
        _ => {}
    };
    Ok(())
}

fn dump_all_memory(term: &Term, ctx: &Ctx) -> Result<()> {
    for (n,chunk) in ctx.mmu.data.chunks(32*2).enumerate() {
        let line_str:String = chunk.iter().map(|b|format!("{:02x}",b)).collect();
        let addr:u32 = (n * 32 * 2) as u32;
        // println!("addr {:04x}",addr);
        match addr {
            0x0000 => println!("start: cart fixed"),
            0x4000 => println!("cart: switchable"),
            0x8000 => println!("8 KiB Video RAM        (VRAM)\n  Tile Data Block 0"),
            0x8800 => println!("8 KiB Video RAM        (VRAM)\n  Tile Data Block 1"),
            0x9000 => println!("8 KiB Video RAM        (VRAM)\n  Tile Data Block 2"),
            0xA000 => println!("8 KiB External RAM"),
            0xC000 => println!("4 KiB Work RAM         (WRAM)"),
            0xD000 => println!("4 KiB Work RAM         (WRAM)"),
            0xE000 => println!("mirror of C000~DDFF"),
            0xFE00 => println!("Sprite attribute table (OAM)"),
            0xFEA0 => println!("not usable"),
            0xFF00 => println!("I/O Registers"),
            0xFF80 => println!("High RAM"),
            _ => {}
        }
        term.write_line(&format!("{:04x}  {}", addr, line_str))?;
    }
    Ok(())
}

fn dump_interrupts(term: &Term, ctx: &Ctx) -> Result<()>{
    let reg = Style::new().bg(Color::Cyan).red().bold();
    term.write_line(&reg.apply_to("INTERRUPTS").to_string())?;
    term.write_line(&format!("active interrupt = {:?}",ctx.mmu.hardware.active_interrupt))?;
    term.write_line(&format!("vblank = {}",ctx.mmu.hardware.vblank_interrupt_enabled))?;
    term.write_line(&format!("timer = {}",ctx.mmu.hardware.timer_interrupt_enabled))?;
    term.write_line(&format!("lcdc = {}",ctx.mmu.hardware.lcdc_interrupt_enabled))?;
    term.write_line(&format!("serial = {}",ctx.mmu.hardware.serial_interrupt_enabled))?;
    term.write_line(&format!("transition = {}",ctx.mmu.hardware.transition_interrupt_enabled))?;
    Ok(())
}

fn dump_oram(term: &Term, ctx: &Ctx) -> Result<()>{
    term.write_line("object memory")?;
    let data = ctx.mmu.fetch_oram();
    for line in data.chunks(32) {
        let line_str:String = line.iter()
            .map(|b|format!("{:02x}",b))
            .collect();
        term.write_line(&line_str)?;
    }
    Ok(())
}

fn dump_vram(term: &Term, ctx: &Ctx) -> Result<()>{
    term.write_line("vram memory")?;
    {
        term.write_line("tiledata block 0")?;
        let data = ctx.mmu.fetch_tiledata_block0();
        for line in data.chunks(64) {
            let line_str: String = line.iter()
                .map(|b| format!("{:02x}", b))
                .collect();
            term.write_line(&line_str)?;
        }
    }
    {
        term.write_line("tiledata block 1")?;
        let data = ctx.mmu.fetch_tiledata_block1();
        for line in data.chunks(64) {
            let line_str: String = line.iter()
                .map(|b| format!("{:02x}", b))
                .collect();
            term.write_line(&line_str)?;
        }
    }
    {
        term.write_line("tiledata block 2")?;
        let data = ctx.mmu.fetch_tiledata_block2();
        for line in data.chunks(64) {
            let line_str: String = line.iter()
                .map(|b| format!("{:02x}", b))
                .collect();
            term.write_line(&line_str)?;
        }
    }
    Ok(())
}
fn show_test_memory(term: &Term, ctx: &Ctx) -> Result<()>{
    term.write_line(&format!("memory at {:04X}",TEST_ADDR))?;
    let data = ctx.mmu.fetch_test_memory();
    for line in data.chunks(32) {
        let line_str:String = line.iter()
            .map(|b|format!("{:02x}",b))
            .collect();
        term.write_line(&line_str)?;
    }

    Ok(())
}

fn dump_cart_rom(term: &Term, ctx: &Ctx) -> Result<()>  {
    term.write_line("cart rom starting at 0x0100")?;
    let rom = ctx.mmu.fetch_rom_bank_1();
    print_memory_to_console(rom,term,ctx)?;
    Ok(())
}

fn print_memory_to_console(mem: &[u8], term: &Term, ctx: &Ctx) ->Result<()>{
    for line in mem.chunks(64) {
        let line_str:String = line.iter()
            .map(|b|format!("{:02x}",b))
            .collect();
        term.write_line(&line_str)?;
    }
    Ok(())
}

fn show_full_hardware_registers(term: &Term, ctx: &Ctx) -> Result<()>{
    let reg = Style::new().bg(Color::Cyan).red().bold();
    let regs = &ctx.mmu.hardware;
    term.write_line(&format!("registers are {} ",reg.apply_to("cool")))?;
    term.write_line(&format!("LY   {:02x}    LYC  {:02x}    SCX {:02x}  SCY {:02x}",regs.LY, regs.LYC,  regs.SCX,  regs.SCY))?;
    term.write_line(&format!("LCDC {:08b}  STAT {:08b}",regs.LCDC, regs.STAT))?;
    term.write_line(&format!("IME  {:08b}    IE {:08b}",regs.IME, regs.IE,))?;
    term.write_line(&format!("BGP  {:08b}",ctx.mmu.hardware.BGP))?;
    // term.write_line(&format!("OBP0 {:8b}",ctx.mmu.hardware.OBP0))?;
    // term.write_line(&format!("OBP1 {:8b}",ctx.mmu.hardware.OBP1))?;
    // term.write_line(&format!("WY   {:8b}",ctx.mmu.hardware.WY))?;
    // term.write_line(&format!("WX   {:8b}",ctx.mmu.hardware.WX))?;
    Ok(())
}

fn fetch_opcode(cpu: &Z80, mmu: &MMU) -> (u16,u16) {
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

impl Ctx {
    pub(crate) fn execute_test(&mut self) {
        let (opcode, _off) = fetch_opcode_from_memory(&self.cpu, &self.mmu);
        if let Some(instr) = lookup_opcode(opcode) {
            println!("executing {}  PC {:04x}  SP {:04x}  {:02x}  {:?}",  self.clock,
                     self.cpu.r.pc, self.cpu.r.sp, opcode, instr);
            self.execute_instruction(&instr);
            print_registers(&self.cpu, &self.mmu);
        } else {
            println!("current cycle {}", self.clock);
            println!("unknown op code {:04x}",opcode);
            panic!("unknown op code");
        }
        // self.mmu.update(&mut self.cpu, );
        self.clock+=1;
    }
}

fn print_registers(cpu: &Z80, mmu:&MMU) {
    println!("A {:02x}", cpu.r.get_u8reg(&RegisterName::A));
    println!("B {:02x}", cpu.r.get_u8reg(&RegisterName::B));
    println!("LY {:02x} ", mmu.hardware.LY);
    println!("PC {:02x} ", cpu.get_pc());
    println!("SP {:02x} ", cpu.get_sp());
}

#[test]
fn test_write_register_A() {
    let rom:[u8;2] = [0x3e,0x05]; // LD A, 0x05
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    ctx.execute_test();
    assert_eq!(ctx.cpu.r.get_u8reg(&A),0x05);
}
#[test]
fn test_write_register_D() {
    let rom:[u8;2] = [0x16,0x05]; // LD D, 0x05
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    ctx.execute_test();
    assert_eq!(ctx.cpu.r.get_u8reg(&D),0x05);
}
#[test]
fn test_write_register_DE() {
    let rom:[u8;2] = [0x16,0x05]; // LD D, 0x05
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    ctx.execute_test();
    assert_eq!(ctx.cpu.r.get_u8reg(&D),0x05);
}

#[test]
fn test_push_pop() {
    let rom:[u8;10] = [
        0x31, 0xfe, 0xff, // LD SP u16, set the stack pointer to fffe
        0x06, 0x04, // LD B, 04
        0xC5, // PUSH BC
        0xCB, 0x11, // RL C
        0x17,// RLA
        0xC1, // POP BC
    ];
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());

    println!("stack {:?} ",ctx.mmu.get_stack_16());
    ctx.execute_test(); // set SP to fffe
    ctx.execute_test(); // LD B, 04
    println!("stack {:?} ",ctx.mmu.get_stack_16());
    assert_eq!(ctx.cpu.r.sp,0xfffe);
    println!("BC {:04x}",ctx.cpu.r.get_u16reg(&BC));
    println!("B {:02x}",ctx.cpu.r.get_u8reg(&B));
    println!("C {:02x}",ctx.cpu.r.get_u8reg(&C));
    assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
    assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
    assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);
    // println!("stack {:?} ",ctx.mmu.get_stack_16());
    println!("B {:02x} C {:02x} BC {:04x}",ctx.cpu.r.get_u8reg(&B), ctx.cpu.r.get_u8reg(&C), ctx.cpu.r.get_u16reg(&BC));

    println!("pushing BC");
    ctx.execute_test(); // PUSH BC
    assert_eq!(ctx.cpu.r.sp,0xfffe-2);
    assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
    assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
    assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);
    println!("stack {:?} ",ctx.mmu.get_stack_16());

    ctx.execute_test(); // RL C
    ctx.execute_test(); // RLA

    println!("POPPING BC");
    ctx.execute_test(); // POP BC
    println!("stack {:?} ",ctx.mmu.get_stack_16());
    println!("B {:02x} C {:02x} BC {:04x}",ctx.cpu.r.get_u8reg(&B), ctx.cpu.r.get_u8reg(&C), ctx.cpu.r.get_u16reg(&BC));
    assert_eq!(ctx.cpu.r.sp,0xfffe);
    assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
    assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
    assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);

}

#[test]
fn test_vblank_interrupt() {
    let mut rom = vec![
        // start at 0x0000
        op(Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::SP))), // LD SP 16 init SP to 0xFFFE
        0xFE, 0xFF,
        //register handler?
        //enable interrupts

        // enable vblank interrupts by setting 0xFFF0 to 0x01
        op(Instr::LoadInstr(Load_R_u8(B))), // LD B, 0x01
        0x01,
        // LD HL, 0xFF0F
        op(Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::HL))),
        0x0F,
        0xFF,
        op(Instr::LoadInstr(Load::Load_addr_R2_r(DoubleRegister::HL,RegisterName::B))), // write 0x01 to the IE register

        //turn on interrupts
        op(Instr::SpecialInstr(Special::EnableInterrupts())), // EI code

        // set B to 65
        op(Instr::LoadInstr(Load_R_u8(B))), // LD B 0x65
        0x65, //02

        op(Instr::LoadInstr(Load::Load_R_R(RegisterName::A,RegisterName::B))), //copy B to A
        op(Instr::CompareInst(Compare::CP_A_n())), //spin loop looking for A is 66
        0x66, // u8 66
        op(Instr::JumpInstr(Jump::Relative_cond_notzero_i8())),// JR NZ jump back two spaces if not zero yet//05
        0xfb, // -2


        op(Instr::LoadInstr(Load_R_u8(B))), //if got here then test is over, set B to 67
        0x67,
    ];

    println!("length is {}",rom.len());
    for i in rom.len() .. 0x40 {
        rom.push(op(Instr::SpecialInstr(Special::NOOP())));
    }
    println!("length is {}",rom.len());
    // add the vblank handler
    rom.push(op(Instr::LoadInstr(Load_R_u8(B)))); // set B to 66
    rom.push(0x66);
    rom.push(op(Instr::SpecialInstr(Special::RETI())));
    println!("length is {}",rom.len());


    // test loops until B is 67
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    print_registers(&ctx.cpu, &ctx.mmu);
    while ctx.cpu.r.get_u8reg(&B) != 0x67 {
        ctx.execute_test();
        // if instruction counter reaches some high number then we clearly got stuck
        if ctx.clock > 3090 {
            panic!("stuck inside a loop!");
        }
        // if ctx.mmu.hardware.LY == 0x99 {
        //     panic!("hit the vblank");
        // }
    }
    println!("reached the end");
}

#[test]
fn test_timer_interrupt() {
    let mut rom = vec![
        // start at 0x0000
        op(Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::SP))), // LD SP 16 init SP to 0xFFFE
        0xFE, 0xFF,


        // enable timer interrupts by setting 0xFFF0 to b0000_0010
        op(Instr::LoadInstr(Load_R_u8(B))), // LD B, 0x04
        0b0000_0100,
        // LD HL, 0xFFFF
        op(Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::HL))),
        0xFF,
        0xFF,
        op(Instr::LoadInstr(Load::Load_addr_R2_r(DoubleRegister::HL,RegisterName::B))), // write 0x01 to the IE register

        //enable timer using TAC
        op(Instr::LoadInstr(Load_R_u8(B))), // LD B, 0x04
        0b0000_0100,
        op(Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::HL))),
        0x07,
        0xFF,
        op(Instr::LoadInstr(Load::Load_addr_R2_r(DoubleRegister::HL,RegisterName::B))), // write b100 to the TAC register

        //turn on interrupts in general
        op(Instr::SpecialInstr(Special::EnableInterrupts())), // EI code

        // set B to 65
        op(Instr::LoadInstr(Load_R_u8(B))), // LD B 0x65
        0x65, //02

        op(Instr::LoadInstr(Load::Load_R_R(RegisterName::A,RegisterName::B))), //copy B to A
        op(Instr::CompareInst(Compare::CP_A_n())), //spin loop looking for A is 66
        0x66, // u8 66
        op(Instr::JumpInstr(Jump::Relative_cond_notzero_i8())),// JR NZ jump back two spaces if not zero yet//05
        0xfb, // -2


        op(Instr::LoadInstr(Load_R_u8(B))), //if got here then test is over, set B to 67
        0x67,
    ];

    println!("length is {}",rom.len());
    for i in rom.len() .. (TIMER_INTERRUPT_ADDR as usize) {
        rom.push(op(Instr::SpecialInstr(Special::NOOP())));
    }
    println!("length is {}",rom.len());

    // add the timer interrupt handler
    rom.push(op(Instr::LoadInstr(Load_R_u8(B)))); // set B to 66
    rom.push(0x66);
    rom.push(op(Instr::SpecialInstr(Special::RETI())));
    println!("length is {}",rom.len());

    // test loops until B is 67
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    print_registers(&ctx.cpu, &ctx.mmu);
    while ctx.cpu.r.get_u8reg(&B) != 0x67 {
        ctx.execute_test();
        println!("timer info DIV {:04} TIMA {:04} TMA {:04} TAC {:04}",
                 ctx.mmu.hardware.DIV,
                 ctx.mmu.hardware.TIMA,
                 ctx.mmu.hardware.TMA,
                 ctx.mmu.hardware.TAC,
        );
        // if instruction counter reaches some high number then we clearly got stuck
        if ctx.clock > 3090 {
            println!("timer is {} {}", ctx.mmu.hardware.timer_interrupt_enabled, ctx.mmu.hardware.IME);
            panic!("stuck inside a loop!");
        }
        // if ctx.mmu.hardware.LY == 0x99 {
        //     panic!("hit the vblank");
        // }
    }
    println!("reached the end");
}

fn op(ins: Instr) -> u8 {
    println!("lookup opcode for instruction {:?}",ins);
    match ins {
        Instr::LoadInstr(Load::Load_R_u8(RegisterName::B)) => 0x06,
        Instr::LoadInstr(Load::Load_R_R(RegisterName::A,RegisterName::B)) => 0x78,
        Instr::CompareInst(Compare::CP_A_n()) => 0xFE,
        Instr::JumpInstr(Jump::Relative_cond_notzero_i8()) => 0x20,
        Instr::SpecialInstr(Special::EnableInterrupts()) => 0xFB,
        Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::HL)) => 0x21,
        Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::SP)) => 0x31,
        Instr::LoadInstr(Load::Load_addr_R2_r(DoubleRegister::HL, B)) => 0x70,
        Instr::SpecialInstr(Special::NOOP()) => 0x00,
        Instr::SpecialInstr(Special::RETI()) => 0xD9,
        _ => panic!("unknown instruction to parse"),
    }
}


fn fetch_opcode_from_memory(cpu:&Z80, mmu:&MMU) -> (u16, u16) {
    let pc = cpu.r.pc;
    let fb:u8 = mmu.read8(pc);
    if fb == 0xcb {
        let sb:u8 = mmu.read8(pc+1);
        (0xcb00 | sb as u16,2)
    } else {
        (fb as u16, 1)
    }
}
