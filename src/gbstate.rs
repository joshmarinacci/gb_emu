use std::fs;
use std::path::{Path, PathBuf};
use crate::common::{get_bit, get_bit_as_bool, load_romfile, MBC};
use crate::cpu::{CPU, CPUR16, CPUR8};
use crate::mmu::{IORegister, MMU2};
use crate::ops::{execute_op, make_op_table, Op, OpTable};
use crate::ppu::PPU2;

pub struct GBState {
    pub cpu: CPU,
    pub mmu: MMU2,
    pub ppu: PPU2,
    pub clock: u32,
    pub count: u32,
    pub(crate) ops: OpTable,
    pub debug: bool,
}

impl GBState {
    pub fn lookup_op(&self, code: &u16) -> Option<&Op> {
        self.ops.ops.get(code)
    }
    pub fn draw_full_screen(&mut self) {
        self.ppu.draw_full_screen(&self.mmu);
    }
    pub fn get_pc(&self) -> u16 {
        self.cpu.get_pc()
    }
    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.real_set_pc(pc);
    }
    pub fn fetch_opcode_at(&self, pc: u16) -> u16 {
        let fb: u8 = self.mmu.read8(pc);
        if fb == 0xcb {
            let sb: u8 = self.mmu.read8(pc + 1);
            0xcb00 | sb as u16
        } else {
            fb as u16
        }
    }
    pub(crate) fn fetch_next_opcode(&self) -> u16 {
        self.fetch_opcode_at(self.cpu.get_pc())
    }
    pub(crate) fn make_test_context(rom: &Vec<u8>) -> GBState {
        let mut gb = GBState {
            cpu: CPU::init(),
            mmu: MMU2::init(rom, MBC::RomOnly()),
            ppu: PPU2::init(),
            clock: 0,
            count: 0,
            ops: make_op_table(),
            debug: false,
        };
        gb.set_pc(0);
        return gb;
    }
    pub fn dump_current_state(&self) {
        println!(
            "PC: {:04x}  OP: {:04x}  clock={}   count={}",
            self.cpu.get_pc(),
            self.mmu.read8(self.cpu.get_pc()),
            self.clock,
            self.count,
        );
        println!(
            "A:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X}  ",
            self.cpu.get_r8(CPUR8::R8A),
            self.cpu.get_r8(CPUR8::R8B),
            self.cpu.get_r8(CPUR8::R8C),
            self.cpu.get_r8(CPUR8::R8D),
            self.cpu.get_r8(CPUR8::R8E),
            self.cpu.get_r8(CPUR8::R8H),
            self.cpu.get_r8(CPUR8::R8L),
        );

        println!(
            "BC:{:04X} DE:{:04X} HL:{:04X}   ",
            self.cpu.get_r16(CPUR16::BC),
            self.cpu.get_r16(CPUR16::DE),
            self.cpu.get_r16(CPUR16::HL),
        );


        //IO status bits
        println!(
            "IME = {} IF = {:08b} IE = {:08b}  LCDC: {:08b}   STAT: {:08b}  HALT={}",
            self.cpu.IME,
            self.mmu.read8_IO(&IORegister::IF),
            self.mmu.read8_IO(&IORegister::IE),
            self.mmu.read8_IO(&IORegister::LCDC),
            self.mmu.read8_IO(&IORegister::STAT),
            self.cpu.halt,
        );
        println!(
            "LY = {}  LYC {}   SCY {} SCX {}   WY {} WX {} ",
            self.mmu.read8_IO(&IORegister::LY),
            self.mmu.read8_IO(&IORegister::LYC),
            self.mmu.read8_IO(&IORegister::SCY),
            self.mmu.read8_IO(&IORegister::SCX),
            self.mmu.read8_IO(&IORegister::WY),
            self.mmu.read8_IO(&IORegister::WX),
        );

        println!(
            "LCD {}   mode = {:?}  hi={}, vi={} spi={} sci={}",
            self.mmu.lcdc.enabled,
            self.mmu.stat.mode,
            self.mmu.stat.hblank_interrupt_enabled,
            self.mmu.stat.vblank_interrupt_enabled,
            self.mmu.stat.sprite_interrupt_enabled,
            self.mmu.stat.scanline_match_interrupt_enabled,
        );

        let IE = self.mmu.read8_IO(&IORegister::IE);
        let IF = self.mmu.read8_IO(&IORegister::IF);
        println!(
            "interr: IE = {:08b} vblank {}   lcd stat {}   timer {}   serial {}   joy {} ",
            IE,
            get_bit(IE,0),
            get_bit(IE,1),
            get_bit(IE,2),
            get_bit(IE,3),
            get_bit(IE,4),
        );
        println!(
            "interr: IF = {:08b} vblank {}   lcd stat {}   timer {}   serial {}   joy {} ",
            IF,
            get_bit(IF,0),
            get_bit(IF,1),
            get_bit(IF,2),
            get_bit(IF,3),
            get_bit(IF,4),
        );

        println!(
            "timer: DIV = {}  TIMA {}   TMA {} TAC {} ",
            self.mmu.read8_IO(&IORegister::DIV),
            self.mmu.read8_IO(&IORegister::TIMA),
            self.mmu.read8_IO(&IORegister::TMA),
            self.mmu.read8_IO(&IORegister::TAC),
        );

        //back PC up to nearest 32byt boundary, then back up another 32 bytes, then print out 10 rows of memory
        let mut start = self.cpu.get_pc() / 32;
        if start > 2 {
            start = start - 2
        } else {
            start = 0;
        }
        let pc = start * 32;
        println!("went from {:04x} to {:04x}", self.cpu.get_pc(), pc);
        self.print_ram_at(pc, 16 * 8);

        let line_str: String = self.cpu.recent_pcs.iter().map(|b| format!("{:02x} ", b)).collect();
        println!("recent PCs {}",line_str);

        for pc in self.cpu.recent_pcs.iter() {
            let opcode = self.fetch_opcode_at(*pc);
            if let Some(inst) = self.lookup_op(&opcode) {
                println!("${:04X}  {:04x} {}  {}", pc, opcode, inst.to_asm(), inst.real(self));
            }
        }
        println!("here again");
    }
    fn print_ram_at(&self, pc: u16, len: u16) {
        let nums = 0..16;
        let line_str: String = nums.into_iter().map(|n| (format!("{:02x} ", n))).collect();
        println!("     {}",line_str);
        let ram = self.mmu.borrow_slice(pc as usize, (pc + len + 1) as usize);
        for (n, row) in ram.chunks_exact(16).enumerate() {
            let line_str: String = row.iter().map(|b| format!("{:02x} ", b)).collect();
            println!("{:04x} {}", (pc + ((n as u16) * 16)) as u16, line_str);
        }
    }
    pub fn execute(&mut self) {
        let code = self.fetch_next_opcode();
        if let Some(opx) = self.ops.ops.get(&code) {
            let op: Op = (*opx).clone();
            if self.debug {
                let pc = self.cpu.get_pc();
                println!("BEFORE: PC:{:04x} SP:{:04x}  op:{:04x} {:?}  reg:{}   next mem = {:02x}  {:02x}  {:02x}",
                         pc,
                         self.cpu.get_sp(),
                         code,
                         op.to_asm(),
                         self.cpu.reg_to_str(),
                         self.mmu.read8(pc + 0), self.mmu.read8(pc + 1), self.mmu.read8(pc + 2)
                );
            }

            // perform the actual operation
            if !self.cpu.halt {
                execute_op(self,&op);
            }
            self.ppu.update(&mut self.mmu, self.clock);
            {
                //update DIV
                let mut div = self.mmu.read8_IO(&IORegister::DIV);
                if self.count % 256 == 0 {
                    div = div.wrapping_add(1);
                    self.mmu.write8_IO_raw(IORegister::DIV,div);
                }
            }

            {
                //update timer
                let TAC = self.mmu.read8_IO(&IORegister::TAC);
                let b1 = get_bit_as_bool(TAC,1);
                let b0 = get_bit_as_bool(TAC,0);
                let factor = match (b1,b0) {
                    (false,false) => 1024,
                    (false,true) => 16,
                    (true,false) => 64,
                    (true,true) => 256,
                };
                if get_bit_as_bool(TAC,2) {
                    // println!("timer enabled. updating with factor {}",factor);
                    let mut tima = self.mmu.read8_IO(&IORegister::TIMA);
                    let mut tma = self.mmu.read8_IO(&IORegister::TMA);
                    if self.count % factor == 0 {
                        let (t2,over) = tima.overflowing_add(1);
                        if over {
                            tima = tma;
                            self.mmu.set_IO_bit(&IORegister::IF, 2, true);
                        } else {
                            tima = t2;
                        }
                        self.mmu.write8_IO_raw(IORegister::TIMA, tima);
                    }
                }
            }

            //check for interrupts
            if self.cpu.IME {
                if self.mmu.read8_IO(&IORegister::IF) > 0 {
                    self.process_interrupts();
                }
            }

            // if self.debug {
            //     println!("AFTER:  PC {:04x}  op:{:04x} {:?}  reg:{}",
            //              self.cpu.get_pc(),
            //              code,
            //              op.to_asm(),
            //              self.cpu.reg_to_str());
            // }
            self.clock += op.cycles as u32;
            self.count += 1;
        } else {
            println!("invalid op code: {:04x}", code);
            self.dump_current_state();
            panic!("invalid op code")
        }
    }
    pub fn execute_n(&mut self, n: usize) {
        for _ in 0..n {
            if self.count % 1_000_000 == 0{
                println!("{}",self.count/1_000_000);
            }
            self.execute();
        }
    }
    pub fn run_to_vblank(&mut self)  {
        self.ppu.entered_vram = false;
        loop {
            if self.ppu.entered_vram {
                break;
            }
            self.execute();
        }
    }

    pub fn run_to_register_write(&mut self, address:u16) {
        loop {
            self.execute();
            if self.mmu.last_write_address == address {
                println!("just wrote");
                break;
            }
        }
    }
    pub fn run_to_instruction(&mut self, code:u16) {
        loop {
            self.execute();
            let current_code = self.fetch_opcode_at(self.get_pc());
            if current_code == code {
                println!("hit instruction code {:04x}",code);
                break;
            }
        }
    }
    pub fn run_to_pc(&mut self, new_pc: u16) {
        loop {
            self.execute();
            if self.get_pc() == new_pc {
                println!("hit the specified PC");
                break;
            }
        }
    }

    fn trigger_interrupt(&mut self, handler_address:u16, bit:u8) {
        // println!("doing interrupt for bit {}",bit);
        self.cpu.IME = false;
        self.cpu.dec_sp();
        self.cpu.dec_sp();
        self.mmu.write16(self.cpu.get_sp(), self.cpu.get_pc());
        self.cpu.halt = false;
        //jump to start of interrupt
        self.set_pc(handler_address);
        //reset the IF register
        self.mmu.set_IO_bit(&IORegister::IF,bit,false);
    }
    fn process_interrupts(&mut self) {
        // println!("IF requested for {:08b}",self.mmu.read8_IO(&IORegister::IF));
        if self.mmu.get_IO_bit(&IORegister::IF,0) && self.mmu.get_IO_bit(&IORegister::IE,0) {
            self.trigger_interrupt(VBLANK_HANDLER_ADDRESS, 0);
            return;
        }
        if self.mmu.get_IO_bit(&IORegister::IF,1) && self.mmu.get_IO_bit(&IORegister::IE,1) {
            self.trigger_interrupt(STAT_HANDLER_ADDRESS, 1);
            return;
        }
        if self.mmu.get_IO_bit(&IORegister::IF,2) && self.mmu.get_IO_bit(&IORegister::IE,2) {
            self.trigger_interrupt(TIMER_HANDLER_ADDRESS, 2);
            return;
        }
        if self.mmu.get_IO_bit(&IORegister::IF,3) && self.mmu.get_IO_bit(&IORegister::IE,3) {
            self.trigger_interrupt(SERIAL_HANDLER_ADDRESS, 3);
            return;
        }
        if self.mmu.get_IO_bit(&IORegister::IF,4) && self.mmu.get_IO_bit(&IORegister::IE,4) {
            self.trigger_interrupt(JOYPAD_HANDLER_ADDRESS, 4);
            return;
        }
        // println!("some other interrupt requested but not handled");
        // println!("IE ={:08b}",self.mmu.read8_IO(&IORegister::IE));
        // println!("IF ={:08b}",self.mmu.read8_IO(&IORegister::IF));
    }
}


const VBLANK_HANDLER_ADDRESS:u16 = 0x40;
const STAT_HANDLER_ADDRESS:u16 = 0x48;
const TIMER_HANDLER_ADDRESS:u16 = 0x50;
const SERIAL_HANDLER_ADDRESS:u16 = 0x58;
const JOYPAD_HANDLER_ADDRESS:u16 = 0x60;



fn fetch_opcode_from_memory(gb: &GBState) -> u16 {
    let pc = gb.cpu.get_pc();
    let fb: u8 = gb.mmu.read8(pc);
    if fb == 0xcb {
        let sb: u8 = gb.mmu.read8(pc + 1);
        0xcb00 | sb as u16
    } else {
        fb as u16
    }
}

pub fn make_core_from_rom(fname: &str) -> Option<GBState> {
    match load_romfile(&PathBuf::from(fname)) {
        Ok(cart) => {
            let mut gb = GBState {
                cpu: CPU::init(),
                mmu: MMU2::init(&cart.data, cart.mbc),
                ppu: PPU2::init(),
                clock: 0,
                count: 0,
                ops: make_op_table(),
                debug: false,
            };
            gb.set_pc(0x100);
            gb.mmu.write8_IO_raw(IORegister::IF,0);
            gb.mmu.write8_IO_raw(IORegister::IE,0);
            gb.mmu.stat.reset();
            gb.mmu.lcdc.reset();
            println!("SETUP the rom {}",fname);
            return Some(gb);
        }
        Err(_) => {
            panic!("couldnt load the rom file {}",fname);
        }
    }
}

#[test]
fn test_cpuins() {
    let mut gb =
        make_core_from_rom("./resources/testroms/cpu_instrs/individual/10-bit ops.gb").unwrap();

    loop {
        println!("PC {:04x}", gb.cpu.get_pc());
        let prev_pc = gb.cpu.get_pc();
        gb.execute();
        // op.execute(&mut gb);
        if gb.cpu.get_pc() == prev_pc {
            println!("stuck in an infinite loop. pc = {:04x}", gb.cpu.get_pc());
            gb.dump_current_state();
            panic!("stuck in an infinite loop");
        }
    }
    // let goal = 7;
    // println!(
    //     "hopefully we reached count = {}  really = {} ",
    //     goal, gb.count
    // );
    // assert_eq!(gb.count >= goal, true);
}

#[test]
fn test_hellogithub() {
    let mut gb = make_core_from_rom("./resources/testroms/hello-world.gb").unwrap();
    let goal = 20_000;
    gb.set_pc(0);

    let mut debug = false;
    loop {
        if gb.count > 20000 {
            break;
        }
        if gb.cpu.get_pc() == 0x00_2d {
            println!("finished clearing the memory");
            debug = true;
        }
        if gb.cpu.get_pc() == 0x0035 {
            println!("got to the end loop. horrray!");
            break;
        }
        if debug {
            println!("==========");
            println!(
                "PC {:04x}    clock = {}   count = {}",
                gb.cpu.get_pc(),
                gb.clock,
                gb.count
            );
        }
        let prev_pc = gb.cpu.get_pc();
        gb.execute();
        if debug {
            println!(
                "after A:{:02x} B:{:02x} C:{:02x}     BC:{:04x} DE:{:04x} HL:{:04x}  Z={}  C={}",
                gb.cpu.get_r8(CPUR8::R8A),
                gb.cpu.get_r8(CPUR8::R8B),
                gb.cpu.get_r8(CPUR8::R8C),
                gb.cpu.get_r16(CPUR16::BC),
                gb.cpu.get_r16(CPUR16::DE),
                gb.cpu.get_r16(CPUR16::HL),
                gb.cpu.r.zero,
                gb.cpu.r.carry,
            );
        }
        // println!("BC {:04x}",gb.cpu.r.get_bc());
        if gb.cpu.get_pc() == prev_pc {
            println!("stuck in an infinite loop. pc = {:04x}", gb.cpu.get_pc());
            gb.dump_current_state();
            panic!("stuck in an infinite loop");
        }

        if gb.count % 20 == 0 {
            let mut v = gb.mmu.read8_IO(&IORegister::LY);
            if v >= 154 {
                v = 0;
            }
            gb.mmu.write8_IO(&IORegister::LY, v + 1);
        }

        if gb.count > goal {
            break;
        }
    }
    println!(
        "hopefully we reached count = {}  really = {} ",
        goal, gb.count
    );
    gb.dump_current_state();
    assert_eq!(gb.cpu.get_pc(), 0x35);
}

#[test]
fn test_bootrom() {
    let op_table = make_op_table();
    let pth = Path::new("./resources/testroms/hello-world.gb");
    let data: Vec<u8> = fs::read(pth).unwrap();

    let mut gb = GBState {
        cpu: CPU::init(),
        mmu: MMU2::init(&data, MBC::RomOnly()),
        ppu: PPU2::init(),
        clock: 0,
        count: 0,
        ops: make_op_table(),
        debug: false,
    };
    gb.mmu.enable_bootrom();

    let goal = 3_250_000;
    gb.set_pc(0);

    let debug = false;
    loop {
        if debug {
            println!("==========");
            println!(
                "PC {:04x}    clock = {}   count = {}",
                gb.cpu.get_pc(),
                gb.clock,
                gb.count
            );
            println!("LY = {:02x}", gb.mmu.read8(0xFF44))
        }
        let opcode = fetch_opcode_from_memory(&gb);
        if let None = op_table.lookup(&opcode) {
            println!("failed to lookup op for code {:04x}", opcode);
            break;
        }
        let op = op_table.lookup(&opcode).unwrap();
        if debug {
            println!(
                "PC {:04x} {:04x}  =  {}      ({},{})",
                gb.cpu.get_pc(),
                op.code,
                op.to_asm(),
                op.len,
                op.cycles
            );
            println!("                 {}", op.real(&gb));
        }
        let prev_pc = gb.cpu.get_pc();
        gb.execute();
        if debug {
            println!("after A:{:02x} B:{:02x} C:{:02x}  H:{:02x}    BC:{:04x} DE:{:04x} HL:{:04x}  Z={}  C={}",
                     gb.cpu.get_r8(CPUR8::R8A),
                     gb.cpu.get_r8(CPUR8::R8B),
                     gb.cpu.get_r8(CPUR8::R8C),
                     gb.cpu.get_r8(CPUR8::R8H),
                     gb.cpu.get_r16(CPUR16::BC),
                     gb.cpu.get_r16(CPUR16::DE),
                     gb.cpu.get_r16(CPUR16::HL),
                     gb.cpu.r.zero,
                     gb.cpu.r.carry,
            );
        }
        if gb.cpu.get_pc() == prev_pc {
            println!("stuck in an infinite loop. pc = {:04x}", gb.cpu.get_pc());
            gb.dump_current_state();
            panic!("stuck in an infinite loop");
        }

        let pc = gb.cpu.get_pc();
        match pc {
            0x0000 => println!("at the start"),
            0x000c => println!("setting up audio"),
            0x0021 => println!("Loading the logo"),
            0x0034 => println!("adding extra bits"),
            0x0040 => println!("setup background tilemap"),
            0x0055 => println!("setting scroll count to {:02x}", gb.cpu.get_r8(CPUR8::R8H)),
            0x0059 => println!("setting vertical register to {:02x} ", gb.mmu.read8(0xFF42)),
            // 0x005f => println!("set b to {:02x}",gb.cpu.r.b),
            // 0x006a => println!("dec c to {:02x}",gb.cpu.r.c),
            // 0x006d => println!("reached 006d e is {:02x}",gb.cpu.r.e),
            // 0x0070 => println!("reached 70!"),
            // 0x0072 => println!("incremented scroll count {:02x}",gb.cpu.r.h),
            // 0x0064 => println!("waiting for screenframe {:02x}",gb.mmu.read8(0xFF44)),
            // 0x0080 => println!("playing sound one"),
            // 0x0086 => println!("playing sound two"),
            // 0x0089 => println!("scroll logo up if b=1 {:02x} {:02x}",gb.cpu.r.b, gb.mmu.read8(0xFF42)),
            0x008e => println!("got to 8e"),
            0x00E0 => println!("doing check"),
            // 0x00E8 => println!("checking"),
            0x00f1 => println!("made it past the check!"),
            0x0100 => {
                println!("made it to the end!. turned off the rom");
                // gb.mmu.disable_bootrom();
                break;
            }
            _ => {
                // println!("PC {:04x}",pc);
            }
        }

        if gb.count % 500 == 0 {
            let mut v = gb.mmu.read8_IO(&IORegister::LY);
            if v >= 154 {
                v = 0;
            }
            gb.mmu.write8_IO(&IORegister::LY, v + 1);
        }

        if gb.count > goal {
            break;
        }
    }
    println!("PC {:04x}", gb.cpu.get_pc());
    println!(
        "hopefully we reached count = {}  really = {} ",
        goal, gb.count
    );
    assert_eq!(gb.mmu.read8(0x0100), 0xF3);
    // assert_eq!(gb.count>=goal,true);
}

#[test]
fn test_tetris() {
    let op_table = make_op_table();
    let pth = Path::new("./resources/testroms/tetris.gb");
    let data: Vec<u8> = fs::read(pth).unwrap();

    let mut gb = GBState {
        cpu: CPU::init(),
        mmu: MMU2::init(&data, MBC::RomOnly()),
        ppu: PPU2::init(),
        clock: 0,
        count: 0,
        ops: make_op_table(),
        debug: false,
    };
    // gb.mmu.enable_bootrom();

    let goal = 120_100;
    gb.set_pc(0x100);

    let mut debug = false;
    loop {
        match gb.cpu.get_pc() {
            0x020c => println!("setting A to zero"),
            // 0x0218 => println!("in outer loop"),
            0x0239 => println!("end of next loop"),
            0x0247 => println!("register setup"),
            // 0x026f => println!("end of next next loop"),
            0x0281 => println!("more loop"),
            0x0293 => println!("loop again"),
            0x02a0 => println!("got more and calling"),
            0x2795 => {
                println!("called function");
                gb.dump_current_state();
                // debug = true;
            }
            0x279e => {
                // println!("bc is {:04x}",gb.cpu.r.get_bc());
                if gb.cpu.get_r16(CPUR16::BC) == 1 {
                    // debug = true;
                }
            }
            0x27a3 => {
                println!("got to end of next foo");
            }
            0x2a3 => {
                println!("returning to where we came from");
                // debug = true;
            }
            0x7ff3 => {
                println!("ad the high jump");
                // debug = true;
            }

            0x69da => {
                println!("more load highs");
            }
            0x02a6 => {
                println!("back now");
                debug = true;
            }
            0x29a8 => {
                println!("got to checking the joypad?");
                panic!("ill come back to this later");
            }

            0x2828 => println!("checking ly {:04x}", gb.mmu.read8_IO(&IORegister::LY)),
            0x282e => println!("end of vblank wait"),
            // 0x0217 => println!("made it past the loop"),
            _ => {}
        }
        if gb.cpu.get_pc() == 0x33 {
            gb.dump_current_state();
            println!(
                "got to 33, jump to contents of HL {:04x}",
                gb.cpu.get_r16(CPUR16::HL)
            );
            debug = true;
        }
        if debug {
            println!("==========");
            println!(
                "PC {:04x}    clock = {}   count = {}",
                gb.cpu.get_pc(),
                gb.clock,
                gb.count
            );
            println!("LY = {:02x}", gb.mmu.read8(0xFF44))
        }
        let opcode = fetch_opcode_from_memory(&gb);
        if let None = op_table.lookup(&opcode) {
            println!("failed to lookup op for code {:04x}", opcode);
            println!("current PC is {:04x}", gb.cpu.get_pc());
            gb.dump_current_state();
            panic!("failed to find opcode");
        }

        let prev_pc = gb.cpu.get_pc();
        gb.execute();
        if debug {
            println!("after A:{:02x} B:{:02x} C:{:02x}  H:{:02x}    BC:{:04x} DE:{:04x} HL:{:04x}  Z={}  C={}",
                     gb.cpu.get_r8(CPUR8::R8A),
                     gb.cpu.get_r8(CPUR8::R8B),
                     gb.cpu.get_r8(CPUR8::R8C),
                     gb.cpu.get_r8(CPUR8::R8H),
                     gb.cpu.get_r16(CPUR16::BC),
                     gb.cpu.get_r16(CPUR16::DE),
                     gb.cpu.get_r16(CPUR16::HL),
                     gb.cpu.r.zero,
                     gb.cpu.r.carry,
            );
        }
        if gb.cpu.get_pc() == prev_pc {
            println!("stuck in an infinite loop. pc = {:04x}", gb.cpu.get_pc());
            gb.dump_current_state();
            panic!("stuck in an infinite loop");
        }

        let pc = gb.cpu.get_pc();
        match pc {
            0x0000 => println!("at the start"),
            _ => {
                // println!("PC {:04x}",pc);
            }
        }

        // gb.clock += (op.cycles as u32);
        gb.count += 1;
        if gb.count % 500 == 0 {
            let v = gb.mmu.read8_IO(&IORegister::LY);
            // println!("v is {}",v);
            let mut v2 = v;
            if v >= 154 {
                v2 = 0;
            } else {
                v2 = v + 1
            }
            if v2 == 40 {
                println!("vblank flip");
                // gb.ppu.draw_full_screen(&gb.mmu)
            }
            gb.mmu.write8_IO(&IORegister::LY, v2);
        }

        if gb.count > goal {
            break;
        }
    }
    println!("PC {:04x}", gb.cpu.get_pc());
    println!(
        "hopefully we reached count = {}  really = {} ",
        goal, gb.count
    );
    assert_eq!(gb.mmu.read8(0x0101), 0xC3);
    // assert_eq!(gb.count>=goal,true);
}

#[test]
fn read_n_right_test() {
    let mut mmu: MMU2 = MMU2::init_empty(0x66);
    assert_eq!(mmu.read8(0x142), 0x66);
    mmu.write8(0x142, 0x42);
    assert_eq!(mmu.read8(0x142), 0x42);
    mmu.write8_IO(&IORegister::LY, 0x85);
    assert_eq!(mmu.read8_IO(&IORegister::LY), 0x85);
}

#[test]
fn test_write_register_A() {
    let rom: [u8; 2] = [0x3e, 0x05]; // LD A, 0x05
    let mut gb: GBState = GBState::make_test_context(&rom.to_vec());
    gb.execute();
    // println!("op is {:?}", op);
    assert_eq!(gb.cpu.get_r8(CPUR8::R8A), 0x05);
}

#[test]
fn test_write_register_D() {
    let rom: [u8; 2] = [0x16, 0x05]; // LD D, 0x05
    let mut gb: GBState = GBState::make_test_context(&rom.to_vec());
    gb.execute();
    // let code = gb.fetch_next_opcode();
    // let op = gb.ops.lookup(&code).unwrap();
    assert_eq!(gb.cpu.get_r8(CPUR8::R8D), 0x05);
}

#[test]
fn test_write_register_DE() {
    let rom: [u8; 2] = [0x16, 0x05]; // LD D, 0x05
    let mut gb: GBState = GBState::make_test_context(&rom.to_vec());
    gb.execute();
    assert_eq!(gb.cpu.get_r8(CPUR8::R8D), 0x05);
}
// #[test]
// fn test_push_pop() {
//     let rom:[u8;10] = [
//         0x31, 0xfe, 0xff, // LD SP u16, set the stack pointer to fffe
//         0x06, 0x04, // LD B, 04
//         0xC5, // PUSH BC
//         0xCB, 0x11, // RL C
//         0x17,// RLA
//         0xC1, // POP BC
//     ];
//     let mut gb:GBState = GBState::make_test_context(&rom.to_vec());
//     let op_table = make_op_table();
//     let code = fetch_opcode_from_memory(&gb);
//     let op = op_table.lookup(&code).unwrap();
//
//     println!("stack {:?} ",gb.mmu.get_stack_16());
//     ctx.execute_test(); // set SP to fffe
//     ctx.execute_test(); // LD B, 04
//     println!("stack {:?} ",ctx.mmu.get_stack_16());
//     assert_eq!(ctx.cpu.r.sp,0xfffe);
//     println!("BC {:04x}",ctx.cpu.r.get_u16reg(&BC));
//     println!("B {:02x}",ctx.cpu.r.get_u8reg(&B));
//     println!("C {:02x}",ctx.cpu.r.get_u8reg(&C));
//     assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
//     assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
//     assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);
//     // println!("stack {:?} ",ctx.mmu.get_stack_16());
//     println!("B {:02x} C {:02x} BC {:04x}",ctx.cpu.r.get_u8reg(&B), ctx.cpu.r.get_u8reg(&C), ctx.cpu.r.get_u16reg(&BC));
//
//     println!("pushing BC");
//     ctx.execute_test(); // PUSH BC
//     assert_eq!(ctx.cpu.r.sp,0xfffe-2);
//     assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
//     assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
//     assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);
//     println!("stack {:?} ",ctx.mmu.get_stack_16());
//
//     ctx.execute_test(); // RL C
//     ctx.execute_test(); // RLA
//
//     println!("POPPING BC");
//     ctx.execute_test(); // POP BC
//     println!("stack {:?} ",ctx.mmu.get_stack_16());
//     println!("B {:02x} C {:02x} BC {:04x}",ctx.cpu.r.get_u8reg(&B), ctx.cpu.r.get_u8reg(&C), ctx.cpu.r.get_u16reg(&BC));
//     assert_eq!(ctx.cpu.r.sp,0xfffe);
//     assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
//     assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
//     assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);
//
// }
