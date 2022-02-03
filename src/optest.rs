use std::collections::HashMap;
use std::fmt::format;
use std::fs;
use std::path::{Path, PathBuf};
use BinOp::{And, Or, SUB, Xor};
use BitOps::{BIT, RES, SET};
use CallType::{CallU16, Pop, Push, Ret, RetCond};
use Cond::{Carry, NotCarry, NotZero, Zero};
use CPUR8::{R8A, R8B, R8C};
use CPURegister::CpuR8;
use Dst16::DstR16;
use Dst8::AddrDst;
use JumpType::{Absolute, Restart};
use OpType::{BitOp, Call, Dec16, Dec8, Inc16, Inc8, Jump, Load16, Math};
use Src16::{Im16, SrcR16};
use crate::common::{get_bit_as_bool, load_romfile, set_bit};
use crate::cpu2::{CPU, CPUR16, CPUR8, CPURegister};
use crate::cpu2::CPUR8::{R8D, R8E, R8H, R8L};
use crate::cpu::Z80;
use crate::mmu::{BGP, LCDC_LCDCONTROL, NR50_SOUND, NR52_SOUND};
use crate::mmu2::{IORegister, MMU2};
use crate::opcodes::{DoubleRegister, RegisterName, u8_as_i8};
use crate::optest::AddrSrc::Imu16;
use crate::optest::BinOp::Add;
use crate::optest::BitOps::{RL, RLC, RR, RRC, SLA, SRA, SRL, SWAP};
use crate::optest::CallType::CallCondU16;
use crate::optest::Dst8::DstR8;
use crate::optest::JumpType::{AbsoluteCond, Relative, RelativeCond};
use crate::optest::OpType::{Load8, Math16};
use crate::optest::R8::{A, B, C, D, E, H, L};
use crate::optest::R16::{AF, BC, DE, HL, SP};
use crate::optest::Src8::{HiMemIm8, Im8, Mem, SrcR8};
use crate::ppu2::PPU2;

#[derive(Debug, Copy, Clone)]
pub enum R8  { A, B, C, D, E, H, L,}
#[derive(Debug, Copy, Clone)]
pub enum R16 { BC, HL, DE, SP, AF}
#[derive(Debug, Copy, Clone)]
pub enum Src8  {  SrcR8(R8), Mem(R16), MemWithInc(R16), MemWithDec(R16), Im8(), HiMemIm8(), MemIm16() }
#[derive(Debug, Copy, Clone)]
pub enum Src16 {  SrcR16(R16), Im16(),   }
#[derive(Debug, Copy, Clone)]
pub enum Dst8  {  DstR8(R8), AddrDst(R16), HiMemIm8(), HiMemR8(R8), MemIm16(), MemWithInc(R16), MemWithDec(R16)  }
#[derive(Debug, Copy, Clone)]
pub enum Dst16 {  DstR16(R16), }
#[derive(Debug, Copy, Clone)]
enum AddrSrc {
    Imu16(),
    Src16(R16)
}
#[derive(Debug, Copy, Clone)]
enum BinOp {
    Or,
    Xor,
    And,
    SUB,
    Add,
    ADC,
}
#[derive(Debug, Copy, Clone)]
enum BitOps {
    BIT(u8, R8),
    RES(u8, R8),
    SET(u8, R8),
    RLC(R8),
    RRC(R8),
    RL(R8),
    RR(R8),
    SLA(R8),
    SRA(R8),
    SRL(R8),
    SWAP(R8),
    RLA(),
    RRA(),
    CPL(),
}

#[derive(Debug, Copy, Clone)]
pub enum Cond {
    Carry(),
    NotCarry(),
    Zero(),
    NotZero(),
}
#[derive(Debug, Copy, Clone)]
enum JumpType {
    Absolute(AddrSrc),
    AbsoluteCond(Cond,AddrSrc),
    RelativeCond(Cond,Src8),
    Relative(Src8),
    Restart(u8)
}
#[derive(Debug, Copy, Clone)]
enum CallType {
    CallU16(),
    CallCondU16(Cond),
    Ret(),
    RetCond(Cond),
    Push(R16),
    Pop(R16),
}

#[derive(Debug, Copy, Clone)]
enum OpType {
    Noop(),
    Jump(JumpType),
    Call(CallType),
    Load16(Dst16, Src16),
    Load8(Dst8, Src8),
    DisableInterrupts(),
    EnableInterrupts(),
    Compare(Dst8, Src8),
    // Xor(Dst8, Src8),
    // Or(Dst8, Src8),
    Math(BinOp, Dst8, Src8),
    Math16(BinOp, Dst16, Src16),
    // And(Dst8, Src8),
    Inc16(R16),
    Dec16(R16),
    Inc8(Dst8),
    Dec8(Dst8),
    BitOp(BitOps),
}
#[derive(Debug, Clone)]
pub struct Op {
    code:u16,
    len:u16,
    cycles:u16,
    typ:OpType,
}

pub struct GBState {
    pub cpu:CPU,
    pub mmu:MMU2,
    pub ppu:PPU2,
    pub clock:u32,
    pub count:u32,
    ops:OpTable,
    pub debug:bool,
}

impl GBState {
    pub fn draw_full_screen(&mut self) {
        self.ppu.draw_full_screen(&self.mmu);
    }
    pub fn set_pc(&mut self, pc:u16) {
        // if pc == 0x29b3 {
        //     self.debug = true;
        // }
        // if pc == 0xbec3 {
        //     println!("error error erro. bad new pc. current pc is {:04x}",self.cpu.pc);
        //     println!("curren regs are: {}", self.cpu.reg_to_str());
        //     self.dump_current_state();
        //
        //     let str:String = self.cpu.recent_pcs.iter().map(|b|format!("{:04x} ",b)).collect();
        //     println!("previous pcs are {:?}",str);
        //
        //     for p in self.cpu.recent_pcs.iter() {
        //         let opcode = self.fetch_opcode_at(*p);
        //         if let Some(op) = self.ops.lookup(&opcode) {
        //             println!("PC:{:04x}  op:{:02x} {:?}", p, opcode, op);
        //         } else {
        //             println!("PC:{:04x}  op:{:02x} unknown opcode", p, opcode);
        //         }
        //     }
        //
        //     panic!("jumpoing to no mans land");
        // }
        self.cpu.real_set_pc(pc);
    }
}


impl GBState {
    fn fetch_opcode_at(&self, pc: u16) -> u16 {
        let fb:u8 = self.mmu.read8(pc);
        if fb == 0xcb {
            let sb:u8 = self.mmu.read8(pc+1);
            0xcb00 | sb as u16
        } else {
            fb as u16
        }
    }
    pub(crate) fn fetch_next_opcode(&self) -> u16 {
        self.fetch_opcode_at(self.cpu.get_pc())
    }
}

impl GBState {
    fn make_test_context(rom: &Vec<u8>) -> GBState {
        let mut gb = GBState {
            cpu: CPU::init(),
            mmu: MMU2::init(rom),
            ppu: PPU2::init(),
            clock: 0,
            count: 0,
            ops:make_op_table(),
            debug: false
        };
        gb.set_pc(0);
        return gb;
    }
    pub(crate) fn dump_current_state(&self) {
        println!("PC: {:04x}  OP: {:04x}  clock={}   count={}",
                 self.cpu.get_pc(),
                 self.mmu.read8(self.cpu.get_pc()),
                 self.clock,
                 self.count,
        );
        println!("A:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X}  ",
                 self.cpu.get_r8(CPUR8::R8A),
                 self.cpu.get_r8(CPUR8::R8B),
                 self.cpu.get_r8(CPUR8::R8C),
                 self.cpu.get_r8(CPUR8::R8D),
                 self.cpu.get_r8(CPUR8::R8E),
                 self.cpu.get_r8(CPUR8::R8H),
                 self.cpu.get_r8(CPUR8::R8L),
        );

        println!("BC:{:04X} DE:{:04X} HL:{:04X}   ",
                 self.cpu.get_r16(CPUR16::BC),
                 self.cpu.get_r16(CPUR16::DE),
                 self.cpu.get_r16(CPUR16::HL),
        );

        //back PC up to nearest 32byt boundary, then back up another 32 bytes, then print out 10 rows of memory
        let mut start = self.cpu.get_pc()/32;
        if start > 2 {
            start = start -2
        } else {
            start = 0;
        }
        let pc = start *32;
        println!("went from {:04x} to {:04x}",self.cpu.get_pc(),pc);
        self.print_ram_at(pc, 16*8);

    }
    fn print_ram_at(&self, pc: u16, len: u16) {
        let ram = self.mmu.borrow_slice(pc as usize, (pc + len + 1) as usize);
        for (n, row) in ram.chunks_exact(16).enumerate() {
            let line_str:String = row.iter()
                .map(|b|format!("{:02x} ",b))
                .collect();
            println!("{:04x} {}",(pc+((n as u16)*16)) as u16, line_str);
        }

    }
}

impl R8 {
    fn get_value(&self, gb: &GBState) -> u8 {
        match self {
            R8::A => gb.cpu.get_r8(R8A),
            R8::B => gb.cpu.get_r8(R8B),
            R8::C => gb.cpu.get_r8(R8C),
            R8::D => gb.cpu.get_r8(R8D),
            R8::E => gb.cpu.get_r8(R8E),
            R8::H => gb.cpu.get_r8(R8H),
            R8::L => gb.cpu.get_r8(R8L),
        }
    }
    fn set_value(&self, gb: &mut GBState, value:u8) {
        match self {
            R8::A => gb.cpu.set_r8(R8A, value),
            R8::B => gb.cpu.set_r8(R8B, value),
            R8::C => gb.cpu.set_r8(R8C, value),
            R8::D => gb.cpu.set_r8(R8D, value),
            R8::E => gb.cpu.set_r8(R8E, value),
            R8::H => gb.cpu.set_r8(R8H, value),
            R8::L => gb.cpu.set_r8(R8L, value),
        }
    }
    fn name(&self) -> &'static str{
        match self {
            R8::A => "A",
            R8::B => "B",
            R8::C => "C",
            R8::D => "D",
            R8::E => "E",
            R8::H => "H",
            R8::L => "L",
        }
    }
}
impl R16 {
    fn get_value(&self, gb: &GBState) -> u16 {
        match self {
            BC => gb.cpu.get_r16(CPUR16::BC),
            HL => gb.cpu.get_r16(CPUR16::HL),
            DE => gb.cpu.get_r16(CPUR16::DE),
            SP => gb.cpu.get_r16(CPUR16::SP),
            AF => gb.cpu.get_r16(CPUR16::AF),
        }
    }
    fn set_value(&self, gb: &mut GBState, val:u16) {
        match self {
            BC => gb.cpu.set_r16(CPUR16::BC, val),
            HL => gb.cpu.set_r16(CPUR16::HL, val),
            DE => gb.cpu.set_r16(CPUR16::DE, val),
            SP => gb.cpu.set_r16(CPUR16::SP, val),
            AF => gb.cpu.set_r16(CPUR16::AF, val),
        }
    }
    fn name(&self) -> &'static str{
        match self {
            BC => "BC",
            HL => "HL",
            DE => "DE",
            SP => "SP",
            AF => "AF",
        }
    }
}

impl AddrSrc {
    fn name(&self) -> String {
        match self {
            AddrSrc::Imu16() => "u16".to_string(),
            AddrSrc::Src16(r) => r.name().to_string(),
        }
    }
    fn real(&self, gb:&GBState) -> String {
        match self {
            AddrSrc::Imu16() => format!("{:04x}",gb.mmu.read16(gb.cpu.get_pc()+1)),
            AddrSrc::Src16(r) => format!("{:04x}", r.get_value(gb)),
        }
    }
    pub(crate) fn get_addr(&self, gb:&GBState) -> u16 {
        match self {
            AddrSrc::Imu16() => gb.mmu.read16(gb.cpu.get_pc()+1),
            AddrSrc::Src16(r) => r.get_value(gb),
        }
    }
}

impl Src16 {
    fn name(&self) -> String {
        match self {
            Im16() => "nn".to_string(),
            SrcR16(r) => r.name().to_string(),
        }
    }
    fn get_value(&self, gb:&GBState) -> u16 {
        match self {
            Im16() => gb.mmu.read16(gb.cpu.get_pc()+1),
            SrcR16(r) => r.get_value(gb),
        }

    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            Im16() => format!("{:04x}", gb.mmu.read16(gb.cpu.get_pc()+1)),
            SrcR16(r) => format!("{:04x}", r.get_value(gb)),
        }
    }
}

impl Dst16 {
    fn name(&self) -> String {
        match self {
            DstR16(r2) => r2.name().to_string(),
        }
    }
    fn set_value(&self, gb:&mut GBState, val:u16) {
        match self {
            DstR16(r2) => r2.set_value(gb, val)
        }
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            DstR16(rr) => format!("{:04x}",rr.get_value(gb)),
        }
    }
    fn get_value(&self, gb:&GBState) -> u16 {
        match self {
            DstR16(rr) => rr.get_value(gb),
        }
    }
}



impl BinOp {
    fn name(&self) -> &str {
        match self {
            Or => "OR",
            Xor => "XOR",
            And => "AND",
            SUB => "SUB",
            Add => "ADD",
            BinOp::ADC => "ADC",
        }
    }
}
impl Cond {
    fn get_value(&self, gb: &GBState) -> bool {
        match self {
            Carry() => gb.cpu.r.carry,
            NotCarry() => !gb.cpu.r.carry,
            Zero() => gb.cpu.r.zero,
            NotZero() => !gb.cpu.r.zero,
        }
    }
    fn name(&self) -> &str {
        match self {
            Carry() => "C",
            NotCarry() => "NC",
            Zero() => "Z",
            NotZero() => "NZ",
        }
    }
    fn real(&self, gb: &GBState) -> bool {
        match self {
            Carry() => gb.cpu.r.carry,
            NotCarry() => !gb.cpu.r.carry,
            Zero() => gb.cpu.r.zero,
            NotZero() => !gb.cpu.r.zero
        }
    }
}
impl Src8 {
    fn name(&self) -> String {
        match self {
            SrcR8(r1) => r1.name().to_string(),
            Mem(r2) => format!("({})",r2.name()),
            Im8() => "u8".to_string(),
            Src8::HiMemIm8() => "($FF00+n)".to_string(),
            Src8::MemIm16() => "(nn)".to_string(),
            Src8::MemWithInc(r2) => format!("({}+)",r2.name()),
            Src8::MemWithDec(r2) => format!("({}-)",r2.name()),
        }
    }
    fn get_value(&self, gb:&GBState) -> u8 {
        match self {
            SrcR8(r1) => r1.get_value(gb),
            Mem(r2) => {
                // println!("reading memory at location {:04x}",r2.get_value(&gb.cpu));
                gb.mmu.read8(r2.get_value(gb))
            },
            Src8::MemWithInc(r2) => gb.mmu.read8(r2.get_value(gb)),
            Src8::MemWithDec(r2) => gb.mmu.read8(r2.get_value(gb)),
            Im8() => gb.mmu.read8(gb.cpu.get_pc()+1),
            Src8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc()+1);
                gb.mmu.read8(0xFF00 + im as u16)
            }
            Src8::MemIm16() => {
                let addr = gb.mmu.read16(gb.cpu.get_pc()+1);
                gb.mmu.read8(addr)
            }
        }
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc()+1);
                let addr = (0xFF00+im as u16);
                let name = named_addr(addr,gb);
                return format!("({}) is {:02x}", name,self.get_value(gb));
            }
            _ => {}
        }
        format!("{:02x}",self.get_value(gb))
    }
}
impl Dst8 {
    fn name(&self) -> String {
        match self {
            DstR8(r8) => r8.name().to_string(),
            AddrDst(r16) => r16.name().to_string(),
            Dst8::MemIm16() => "(nn)".to_string(),
            Dst8::HiMemIm8() => "($FF00+n)".to_string(),
            Dst8::HiMemR8(r8) => format!("($FF00+{})",r8.name()).to_string(),
            Dst8::MemWithInc(r16) => format!("({}+)", r16.name()).to_string(),
            Dst8::MemWithDec(r16) => format!("({}-)", r16.name()).to_string(),
        }
    }
    fn set_value(&self,gb:&mut GBState, val:u8) {
        match self {
            DstR8(r8) => r8.set_value(gb, val),
            AddrDst(r16) => gb.mmu.write8(r16.get_value(gb), val),
            Dst8::MemIm16() => gb.mmu.write8(gb.mmu.read16(gb.cpu.get_pc()+1),val),
            Dst8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc()+1);
                gb.mmu.write8(0xFF00 + im as u16,val)
            }
            Dst8::HiMemR8(r8) => {
                let addr = 0xFF00 + (r8.get_value(gb) as u16);
                gb.mmu.write8(addr,val)
            }
            Dst8::MemWithInc(r16) => gb.mmu.write8(r16.get_value(gb),val),
            Dst8::MemWithDec(r16) => gb.mmu.write8(r16.get_value(gb),val),
        }
    }
    fn get_value(&self, gb:&GBState) -> u8 {
        match self {
            DstR8(r1) => r1.get_value(gb),
            Dst8::AddrDst(r2) => {
                gb.mmu.read8(r2.get_value(gb))
            }
            Dst8::MemIm16() => {
                let addr = gb.mmu.read16(gb.cpu.get_pc()+1);
                gb.mmu.read8(addr)
            }
            Dst8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc()+1);
                gb.mmu.read8(0xFF00 + im as u16)
            }
            Dst8::HiMemR8(r8) => {
                let addr = 0xFF00 + (r8.get_value(gb) as u16);
                gb.mmu.read8(addr)
            }
            Dst8::MemWithInc(r2) => gb.mmu.read8(r2.get_value(gb)),
            Dst8::MemWithDec(r2) => gb.mmu.read8(r2.get_value(gb)),
        }
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            DstR8(r8) =>  format!("{:02x}",r8.get_value(gb)),
            AddrDst(addr) => format!("{:02x}",gb.mmu.read8(addr.get_value(gb))),
            Dst8::MemIm16() => {
                let addr = gb.mmu.read16(gb.cpu.get_pc()+1);
                named_addr(addr,gb)
            }
            Dst8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc()+1);
                let addr = 0xFF00 + im as u16;
                named_addr(addr,gb)
            }
            Dst8::HiMemR8(r8) => {
                let addr = 0xFF00 + (r8.get_value(gb) as u16);
                named_addr(addr,gb)
            }
            Dst8::MemWithInc(r16) => format!("{:02x}",gb.mmu.read8(r16.get_value(gb))),
            Dst8::MemWithDec(r16) => format!("{:02x}",gb.mmu.read8(r16.get_value(gb))),
        }
    }
}

fn named_addr(addr: u16, gb: &GBState) -> String {
    if let Some(reg) = IORegister::match_address(addr) {
        reg.name().to_string()
    } else {
        let val = gb.mmu.read8(addr);
        format!("{:02x}", val)
    }
}

impl GBState {
    pub fn execute(&mut self)  {
        let code = self.fetch_next_opcode();
        if let Some(opx) = self.ops.ops.get(&code) {
            let op: Op = (*opx).clone();
            // if self.cpu.get_pc() == 0x28 {
            //     self.debug = true;
            // }
            // if self.cpu.get_pc() == 0x29a6 {
            //     self.debug = true;
            // }
            if self.debug {
                let pc = self.cpu.get_pc();
                println!("BEFORE: PC:{:04x} SP:{:04x}  op:{:04x} {:?}  reg:{}   next mem = {:02x}  {:02x}  {:02x}",
                         pc,
                         self.cpu.get_sp(),
                         code,
                         op.to_asm(),
                         self.cpu.reg_to_str(),
                         self.mmu.read8(pc+0), self.mmu.read8(pc+1),self.mmu.read8(pc+2)
                );
            }
            self.stuff(&op);
            // if self.debug {
            //     println!("AFTER:  PC {:04x}  op:{:04x} {:?}  reg:{}",
            //              self.cpu.get_pc(),
            //              code,
            //              op.to_asm(),
            //              self.cpu.reg_to_str());
            // }
            self.clock += (op.cycles as u32);
            self.count += 1;


            if self.count % 500 == 0 {
                let mut v = self.mmu.read8_IO(IORegister::LY);
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
                self.mmu.write8_IO(IORegister::LY,v2);
            }

        } else {
            println!("invalid op code: {:04x}",code);
            self.dump_current_state();
            panic!("invalid op code")
        }
    }
    pub fn execute_n(&mut self, n: usize) {
        for _ in 0..n {
            self.execute();
        }
    }
}

impl GBState {
    pub(crate) fn stuff(&mut self, op: &Op) {
        match &op.typ {
            OpType::Noop() => self.cpu.inc_pc(),
            Jump(typ) => {
                match typ {
                    Absolute(src) => {
                        let addr = src.get_addr(self);
                        self.set_pc(addr);
                    }
                    AbsoluteCond(cond,src) => {
                        let addr = src.get_addr(self);
                        self.set_pc(self.cpu.get_pc()+op.len);
                        if cond.get_value(self) {
                            self.set_pc(addr);
                        }
                    }
                    RelativeCond(cond,src) => {
                        let off = src.get_value(self);
                        let e = u8_as_i8(off);
                        self.set_pc(self.cpu.get_pc()+op.len);
                        if cond.get_value(self) {
                            let addr:u16 = (((self.cpu.get_pc()) as i32) + (e as i32)) as u16;
                            self.set_pc(addr);
                        }
                    }
                    Relative(src) => {
                        let e = u8_as_i8(src.get_value(self));
                        //update the pc before calculating the relative address
                        self.set_pc(self.cpu.get_pc()+op.len);
                        let addr:u16 = ((self.cpu.get_pc() as i32) + (e as i32)) as u16;
                        self.set_pc(addr);
                    }
                    Restart(n) => {
                        self.cpu.dec_sp();
                        self.cpu.dec_sp();
                        self.mmu.write16(self.cpu.get_sp(),self.cpu.get_pc()+1);
                        self.set_pc(*n as u16);
                    }
                }
            }
            Call(typ) => {
                match typ {
                    CallU16() => {
                        let addr = self.mmu.read16(self.cpu.get_pc()+1);
                        self.cpu.dec_sp();
                        self.cpu.dec_sp();
                        self.mmu.write16(self.cpu.get_sp(), self.cpu.get_pc()+3);
                        self.set_pc(addr);
                    }
                    CallCondU16(cond) => {
                        let addr = self.mmu.read16(self.cpu.get_pc()+1);
                        if cond.get_value(self) {
                            self.cpu.dec_sp();
                            self.cpu.dec_sp();
                            self.mmu.write16(self.cpu.get_sp(), self.cpu.get_pc()+3);
                            self.set_pc(addr);
                        } else {
                            self.set_pc(self.cpu.get_pc()+op.len);
                        }
                    }
                    Push(src) => {
                        self.cpu.dec_sp();
                        self.cpu.dec_sp();
                        let value = src.get_value(self);
                        self.mmu.write16(self.cpu.get_sp(), value);
                        self.set_pc(self.cpu.get_pc()+op.len);
                    }
                    Pop(src) => {
                        let value = self.mmu.read16(self.cpu.get_sp());
                        self.cpu.inc_sp();
                        self.cpu.inc_sp();
                        src.set_value(self,value);
                        // println!("PC:{:04x}  POP {:?}, now HL is, {}", self.cpu.get_pc(), src, self.cpu.reg_to_str());
                        self.set_pc(self.cpu.get_pc()+op.len);
                    }
                    Ret() => {
                        let addr = self.mmu.read16(self.cpu.get_sp());
                        self.cpu.inc_sp();
                        self.cpu.inc_sp();
                        self.set_pc(addr);
                    }
                    RetCond(cond) => {
                        if cond.get_value(self) {
                            let addr = self.mmu.read16(self.cpu.get_sp());
                            self.cpu.inc_sp();
                            self.cpu.inc_sp();
                            self.set_pc(addr);
                        } else {
                            self.set_pc(self.cpu.get_pc()+op.len);
                        }
                    }
                }
            }
            Load16(dst,src) => {
                let val = src.get_value(self);
                dst.set_value(self,val);
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            Load8(dst,src) => {
                // println!("executing Load8 dst = {:?} src = {:?}",dst,src);
                let val = src.get_value(self);
                // println!("value from src is {:02x}",val);
                dst.set_value(self,val);
                if let Src8::MemWithInc(r2) = src {
                    let v = r2.get_value(self);
                    let (v2,b) = v.overflowing_add(1);
                    r2.set_value(self,v2);
                }
                if let Src8::MemWithDec(r2) = src {
                    let v = r2.get_value(self);
                    let (v2,b) = v.overflowing_sub(1);
                    r2.set_value(self,v2);
                }
                if let Dst8::MemWithInc(r2) = dst {
                    let v = r2.get_value(self);
                    let (v2,b) = v.overflowing_add(1);
                    r2.set_value(self,v2);
                }
                if let Dst8::MemWithDec(r2) = dst {
                    let v = r2.get_value(self);
                    let (v2,b) = v.overflowing_sub(1);
                    r2.set_value(self,v2);
                }
                // println!("Len is {}",self.len);
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            OpType::DisableInterrupts() => {
                println!("pretending to disable interrupts");
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            OpType::EnableInterrupts() => {
                println!("pretending to enable interrupts");
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            OpType::Compare(dst, src) => {
                let src_v = src.get_value(self);
                let dst_v = dst.get_value(self);

                let result = src_v.wrapping_sub(dst_v);
                // println!("comparing {:02x} with {:02x}",dst_v, src_v);
                self.cpu.r.zero = result == 0;
                self.cpu.r.half = (dst_v & 0x0F) < (src_v & 0x0f);
                self.cpu.r.subn = true;
                self.cpu.r.carry = (dst_v as u16) < (src_v as u16);


                self.set_pc(self.cpu.get_pc()+op.len);
            }
            Inc16(dst) => {
                let v1 = dst.get_value(self);
                let v2 = v1.wrapping_add(1);
                dst.set_value(self, v2);
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            Inc8(dst) => {
                let v1 = dst.get_value(self);
                let result = v1.wrapping_add(1);
                dst.set_value(self, result);
                self.cpu.r.zero = result == 0;
                self.cpu.r.half = (result & 0x0F) + 1 > 0x0F;
                self.cpu.r.subn = false;
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            Dec16(dst) => {
                let v1 = dst.get_value(self);
                let v2 = v1.wrapping_sub(1);
                // println!(" ### DEC {:04x}",v2);
                dst.set_value(self, v2);
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            Dec8(dst) => {
                let v1 = dst.get_value(self);
                let result = v1.wrapping_sub(1);
                dst.set_value(self, result);
                self.cpu.r.zero = result == 0;
                self.cpu.r.subn = true;
                self.cpu.r.half = (result & 0x0F) == 0;
                // carry not modified
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            Math(binop, dst,src) => {
                let b = src.get_value(self);
                let a = dst.get_value(self);
                let (res,sub, half, carry) = match binop {
                    Or  => (a | b, false, false, self.cpu.r.carry),
                    Xor => (a ^ b, false, false, self.cpu.r.carry),
                    And => (a & b, false, true,  self.cpu.r.carry),
                    Add => {
                        let c = 0;
                        let r = a.wrapping_add(b).wrapping_add(c);
                        let half =     (a & 0x0F) + (b &0x0F) > 0xF;
                        let carry = (a as u16) + (b as u16) > 0xFF;
                        (r, false, half,carry)
                    },
                    BinOp::ADC => {
                        let c = if self.cpu.r.carry { 1 } else { 0 };
                        let r = a.wrapping_add(b).wrapping_add(c);
                        let half = (a & 0x0F) + (b & 0x0F) + c > 0xF;
                        let carry = (a as u16) + (b as u16) + (c as u16) > 0xFF;
                        (r,false,half,carry)
                    }
                    BinOp::SUB => {
                        let c =  0;
                        let r = a.wrapping_sub(b).wrapping_sub(c);
                        let half = ((a & 0x0F) < (b & 0x0F) + c);
                        let carry = (a as u16) < (b as u16) + (c as u16);
                        (r,true,half,carry)
                    },
                };
                self.cpu.r.zero = res == 0;
                self.cpu.r.subn = sub;
                self.cpu.r.half = half;
                self.cpu.r.carry = carry;
                dst.set_value(self,res);
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            Math16(binop, dst, src) => {
                let b = src.get_value(self);
                let a = dst.get_value(self);
                match binop {
                    Or => {}
                    Xor => {}
                    And => {}
                    SUB => {}
                    Add => {
                        // println!("adding {:?},{:?}",dst,src);
                        // println!("values are {:04x} {:04x}",a,b);
                        let r = a.wrapping_add(b);
                        self.cpu.r.subn = false;
                        self.cpu.r.half = (a & 0x07FF) + (b & 0x07FF) > 0x07FF;
                        self.cpu.r.carry = (a) > (0xFFFF - b);
                        // println!("final add value is {:04x}",r);
                        dst.set_value(self,r);
                    }
                    BinOp::ADC => {}
                };
                self.set_pc(self.cpu.get_pc()+op.len);
            }
            BitOp(bop) => {
                match bop {
                    BIT(n, r8) => {
                        let val = r8.get_value(self);
                        self.cpu.r.zero = !get_bit_as_bool(val, *n);
                        self.cpu.r.subn = false;
                        self.cpu.r.half = true;
                    }
                    RES(n, r8) => {
                        let val = r8.get_value(self);
                        let val2 = set_bit(val,*n,false);
                        r8.set_value(self,val2);
                    }
                    SET(n, r8) => {
                        let val = r8.get_value(self);
                        let val2 = set_bit(val,*n,true);
                        r8.set_value(self,val2);
                    }
                    BitOps::CPL() => {
                        let val = A.get_value(self);
                        let val2 = !val;
                        A.set_value(self,val2);
                        // Z not affected
                        self.cpu.r.subn = true;
                        self.cpu.r.half = true;
                        //C not affected
                    }
                    BitOps::RLC(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x80 == 0x80;
                        let r = (a << 1) | if c { 1 } else { 0 };
                        r8.set_value(self,r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                        // set_sr_flags(cpu, r,c);
                    }
                    BitOps::RRC(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = (a >> 1) | (if c {0x80} else { 0x00 });
                        r8.set_value(self,r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                        // set_sr_flags(cpu,r,c);
                    }
                    BitOps::RL(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x80 == 0x80;
                        let r = (a << 1) | (if self.cpu.r.carry { 1 } else { 0 });
                        r8.set_value(self,r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                        // set_sr_flags(cpu,r,c);
                    }
                    BitOps::RR(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = (a >> 1) | (if self.cpu.r.carry { 0x80 } else { 0x00 });
                        r8.set_value(self,r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                        // set_sr_flags(cpu, r, c);
                    }
                    BitOps::SLA(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x80 == 0x80;
                        let r = a << 1;
                        r8.set_value(self,r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                        // set_sr_flags(cpu,r,c);
                    }
                    BitOps::SRA(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = (a >> 1) | (a &0x80);
                        r8.set_value(self,r);
                        // set_sr_flags(cpu,r,c);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                    }
                    BitOps::SRL(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = a >> 1;
                        r8.set_value(self,r);
                        // set_sr_flags(cpu,r,c);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                    }
                    SWAP(r8) => {
                        let a = r8.get_value(self);
                        let r = ((a & 0x0f) << 4) | ((a & 0xf0) >> 4);
                        r8.set_value(self, r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = false;
                    }
                    BitOps::RLA() => {
                        let a:u8 = A.get_value(self);
                        let c = a & 0x80 == 0x80;
                        let r = (a << 1) | (if self.cpu.r.carry { 1 } else { 0 });
                        A.set_value(self,r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                    }
                    BitOps::RRA() => {
                        let a:u8 = A.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = (a >> 1) | (if self.cpu.r.carry { 0x80 } else { 0x00 });
                        A.set_value(self,r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                    }
                }
                self.set_pc(self.cpu.get_pc()+op.len);
            }
        }

    }
}




impl Op {
    pub(crate) fn to_asm(&self) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            Jump(typ) => {
                match typ {
                    Absolute(src) => format!("JP {}",src.name()),
                    RelativeCond(cond,src) => format!("JP {},{}",cond.name(),src.name()),
                    Relative(src) => format!("JR i{}",src.name()),
                    Restart(n) => format!("RST n{:02x}",n),
                    AbsoluteCond(cond,src) => format!("CALL {},{}",cond.name(),src.name()),
                }
            },
            Load16(dst,src) => format!("LD {} {}", dst.name(), src.name()),
            OpType::Load8(dst,src) => format!("LD {}, {}", dst.name(), src.name()),
            OpType::DisableInterrupts() => "DI".to_string(),
            OpType::EnableInterrupts() => "EI".to_string(),
            OpType::Compare(dst, src) => format!("CP {},{}", dst.name(), src.name()),
            Inc16(dst,) => format!("INC {}", dst.name()),
            Dec16(dst,) => format!("DEC {}", dst.name()),
            Inc8(dst) => format!("INC {}",dst.name()),
            Dec8(dst) => format!("DEC {}",dst.name()),
            Math(binop, dst, src) => format!("{} {},{}",binop.name(),dst.name(),src.name()),
            Math16(binop, dst, src) => format!("{} {},{}",binop.name(),dst.name(),src.name()),
            BitOp(BIT(n, src)) => format!("BIT {}, {}", n, src.name()),
            BitOp(RES(n, src)) => format!("RES {}, {}", n, src.name()),
            BitOp(SET(n, src)) => format!("SET {}, {}", n, src.name()),
            BitOp(RL(src)) => format!("RL {}", src.name()),
            BitOp(RLC(src)) => format!("RLC {}", src.name()),
            BitOp(RRC(src)) => format!("RLC {}", src.name()),
            BitOp(RR(src)) => format!("RLC {}", src.name()),
            BitOp(SLA(src)) => format!("RLC {}", src.name()),
            BitOp(SRA(src)) => format!("RLC {}", src.name()),
            BitOp(SRL(src)) => format!("RLC {}", src.name()),
            BitOp(SWAP(src)) => format!("RLC {}", src.name()),
            BitOp(BitOps::RLA()) => format!("RLA"),
            BitOp(BitOps::RRA()) => format!("RRA"),
            BitOp(BitOps::CPL()) => format!("CPL"),
            Call(ct) => {
                match ct {
                    CallU16() => format!("CALL u16"),
                    Push(src) => format!("PUSH {}", src.name()),
                    Pop(src) => format!("POP {}", src.name()),
                    Ret() => format!("RET"),
                    CallCondU16(cond) => format!("CALL {},u16",cond.name()),
                    RetCond(cond) => format!("RET {}",cond.name()),
                }
            }
        }
    }
    pub(crate) fn real(&self, gb:&GBState) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            Jump(typ) => {
                match typ {
                    Absolute(src) => format!("JP {:04x}",src.get_addr(gb)),
                    RelativeCond(cond, src) =>  format!("JR {}, {}", cond.real(gb), src.real(gb)),
                    Relative(src) => format!("JR +/-{}", src.real(gb)),
                    Restart(n) => format!("RST {:02x}",n),
                    AbsoluteCond(cond, src) => format!("CALL {}, {}", cond.real(gb), src.real(gb)),
                }
            },
            Load8(dst,src) => format!("LD {} <- {}",dst.real(gb), src.real(gb)),
            Load16(dst,src) => format!("LD {} <- {}",dst.real(gb), src.real(gb)),
            OpType::DisableInterrupts() => format!("DI"),
            OpType::EnableInterrupts() => format!("EI"),
            OpType::Compare(dst, src) => format!("CP {}, {}",dst.real(gb), src.real(gb)),
            Inc16(dst) => format!("INC {}", dst.get_value(gb)),
            Dec16(dst) => format!("DEC {}", dst.get_value(gb)),
            Inc8(dst) => format!("INC {}",dst.get_value(gb)),
            Dec8(dst) => format!("DEC {}",dst.get_value(gb)),
            Math(binop, dst, src) => format!("{} {},{}",binop.name(),dst.real(gb),src.real(gb)),
            Math16(binop, dst,src) => format!("{} {},{}",binop.name(),dst.real(gb),src.real(gb)),
            BitOp(BIT(n, src)) => format!("BIT {}, {}", n, src.get_value(gb)),
            BitOp(RES(n, src)) => format!("RES {}, {}", n, src.get_value(gb)),
            BitOp(SET(n, src)) => format!("SET {}, {}", n, src.get_value(gb)),
            BitOp(RLC(src)) => format!("RLC {}", src.get_value(gb)),
            BitOp(RRC(src)) => format!("RRC {}", src.get_value(gb)),
            BitOp(RL(src))  => format!("RL {}", src.get_value(gb)),
            BitOp(RR(src))  => format!("RR {}", src.get_value(gb)),
            BitOp(SLA(src)) => format!("SLA  {}", src.get_value(gb)),
            BitOp(SRA(src)) => format!("SRA  {}", src.get_value(gb)),
            BitOp(SRL(src)) => format!("SRL  {}", src.get_value(gb)),
            BitOp(SWAP(src)) => format!("SWAP {}",src.get_value(gb)),
            BitOp(BitOps::RLA()) => format!("RLA {}", A.get_value(gb)),
            BitOp(BitOps::RRA()) => format!("RRA {}", A.get_value(gb)),
            BitOp(BitOps::CPL()) => format!("CPL {}", A.get_value(gb)),
            Call(ct) => {
                match ct {
                    CallU16() => format!("CALL {}",gb.mmu.read16(gb.cpu.get_pc())),
                    CallCondU16(cond) => format!("CALL {}, {}",cond.real(gb),gb.mmu.read16(gb.cpu.get_pc())),
                    Push(src) => format!("PUSH {}", src.get_value(gb)),
                    Pop(src) => format!("POP {}", src.get_value(gb)),
                    Ret() => format!("RET  back to {:04X}", gb.mmu.read16(gb.cpu.get_sp())),
                    RetCond(cond) => format!("RET Z: if {} then back to {:04x}",cond.get_value(gb),gb.mmu.read16(gb.cpu.get_sp())),
                }
            }
        }
    }
}

fn fetch_opcode_from_memory(gb:&GBState) -> u16 {
    let pc = gb.cpu.get_pc();
    let fb:u8 = gb.mmu.read8(pc);
    if fb == 0xcb {
        let sb:u8 = gb.mmu.read8(pc+1);
        0xcb00 | sb as u16
    } else {
        fb as u16
    }
}


struct OpTable {
    ops:HashMap<u16,Op>
}

impl OpTable {
    fn new() -> OpTable {
        OpTable {
            ops: Default::default()
        }
    }
    fn inserto(&mut self, code: u16, op: Op) {
        if self.ops.contains_key(&code) {
            println!("ERROR: table already contains {:04x}",code);
            panic!("ERROR table already contains opcode");
        }
        self.ops.insert(code,op);
    }
    fn add(&mut self, op:Op) {
        self.inserto(op.code,op);
    }
    fn load8(&mut self, code: u16, dst: Dst8, src: Src8) {
        let (mut len, mut cycles) = match src {
            SrcR8(_) => (1, 4),
            Mem(_) => (1, 8),
            Im8() => (2, 8),
            Src8::HiMemIm8() => (2,12),
            Src8::MemIm16() => (3,16),
            Src8::MemWithInc(_) => (1,8),
            Src8::MemWithDec(_) => (1,8),
        };
        match dst {
            DstR8(_) => {}
            AddrDst(_) => cycles += 4,
            Dst8::HiMemIm8() => len += 1,
            Dst8::HiMemR8(_) => {},
            Dst8::MemIm16() => len += 2,
            Dst8::MemWithInc(_) => {}
            Dst8::MemWithDec(_) => {}
        }
        self.add(Op { code, len, cycles,  typ:OpType::Load8(dst, src)});
    }
    fn load16(&mut self, code: u16, dst: Dst16, src: Src16) {
        let (len, cycles) = match src {
            Im16() => (3,12),
            SrcR16(_) => (1,8),
        };
        self.add( Op { code, len, cycles, typ: Load16(dst, src), });
    }
    fn lookup(&self, code: &u16) -> Option<&Op> {
        self.ops.get(code)
    }
    fn bitop(&mut self, code: u16, bop: BitOps) {
        self.add(Op{code, len:2, cycles:8, typ:  BitOp(bop)});
    }
}

fn make_op_table() -> OpTable {
    let mut op_table = OpTable::new();//:HashMap<u16,Op> = HashMap::new();
    op_table.add(Op { code:0x00, len:1, cycles:4,  typ:OpType::Noop() });


    op_table.load16(0x01, DstR16(BC), Im16()); // LD DE, nn
    op_table.load8( 0x02, AddrDst(BC), SrcR8(A)); // LD L,A
    op_table.load8( 0x06, DstR8(B), Im8()); // LD B,n
    op_table.load8( 0x0E, DstR8(C), Im8()); // LD C,n
    op_table.load8( 0x1A,DstR8(A), Mem(DE));

    op_table.load8( 0x40, DstR8(B), SrcR8(B));
    op_table.load8( 0x41, DstR8(B), SrcR8(C));
    op_table.load8( 0x42, DstR8(B), SrcR8(D));
    op_table.load8( 0x43, DstR8(B), SrcR8(E));
    op_table.load8( 0x44, DstR8(B), SrcR8(H));
    op_table.load8( 0x45, DstR8(B), SrcR8(L));
    op_table.load8( 0x46, DstR8(B), Mem(HL));
    op_table.load8( 0x47, DstR8(B), SrcR8(A));
    op_table.load8( 0x48, DstR8(C), SrcR8(B));
    op_table.load8( 0x49, DstR8(C), SrcR8(C));
    op_table.load8( 0x4A, DstR8(C), SrcR8(D));
    op_table.load8( 0x4B, DstR8(C), SrcR8(E));
    op_table.load8( 0x4C, DstR8(C), SrcR8(H));
    op_table.load8( 0x4D, DstR8(C), SrcR8(L));
    op_table.load8( 0x4E, DstR8(C), Mem(HL));
    op_table.load8( 0x4F, DstR8(C), SrcR8(A));

    op_table.load8( 0x50, DstR8(D), SrcR8(B));
    op_table.load8( 0x51, DstR8(D), SrcR8(C));
    op_table.load8( 0x52, DstR8(D), SrcR8(D));
    op_table.load8( 0x53, DstR8(D), SrcR8(E));
    op_table.load8( 0x54, DstR8(D), SrcR8(H));
    op_table.load8( 0x55, DstR8(D), SrcR8(L));
    op_table.load8( 0x56, DstR8(D), Mem(HL));
    op_table.load8( 0x57, DstR8(D), SrcR8(A));
    op_table.load8( 0x58, DstR8(E), SrcR8(B));
    op_table.load8( 0x59, DstR8(E), SrcR8(C));
    op_table.load8( 0x5A, DstR8(E), SrcR8(D));
    op_table.load8( 0x5B, DstR8(E), SrcR8(E));
    op_table.load8( 0x5C, DstR8(E), SrcR8(H));
    op_table.load8( 0x5D, DstR8(E), SrcR8(L));
    op_table.load8( 0x5E, DstR8(E), Mem(HL));
    op_table.load8( 0x5F, DstR8(E), SrcR8(A));

    op_table.load8( 0x60, DstR8(H), SrcR8(B));
    op_table.load8( 0x61, DstR8(H), SrcR8(C));
    op_table.load8( 0x62, DstR8(H), SrcR8(D));
    op_table.load8( 0x63, DstR8(H), SrcR8(E));
    op_table.load8( 0x64, DstR8(H), SrcR8(H));
    op_table.load8( 0x65, DstR8(H), SrcR8(L));
    op_table.load8( 0x66, DstR8(H), Mem(HL));
    op_table.load8( 0x67, DstR8(H), SrcR8(A));
    op_table.load8( 0x68, DstR8(L), SrcR8(B));
    op_table.load8( 0x69, DstR8(L), SrcR8(C));
    op_table.load8( 0x6A, DstR8(L), SrcR8(D));
    op_table.load8( 0x6B, DstR8(L), SrcR8(E));
    op_table.load8( 0x6C, DstR8(L), SrcR8(H));
    op_table.load8( 0x6D, DstR8(L), SrcR8(L));
    op_table.load8( 0x6E, DstR8(L), Mem(HL));
    op_table.load8( 0x6F, DstR8(L), SrcR8(A));

    op_table.load8( 0x77, AddrDst(HL), SrcR8(A)); // LD L,A
    op_table.load8( 0x78, DstR8(A), SrcR8(B));
    op_table.load8( 0x79, DstR8(A), SrcR8(C));
    op_table.load8( 0x7A, DstR8(A), SrcR8(D));
    op_table.load8( 0x7B, DstR8(A), SrcR8(E));
    op_table.load8( 0x7C, DstR8(A), SrcR8(H));
    op_table.load8( 0x7D, DstR8(A), SrcR8(L));
    op_table.load8( 0x7E, DstR8(A), Mem(HL));
    op_table.load8( 0x7F, DstR8(A), SrcR8(A));


    op_table.load16(0x11, DstR16(DE), Im16()); // LD DE, nn
    op_table.load16(0x21, DstR16(HL), Im16()); // LD HL, nn
    op_table.load16(0x31, DstR16(SP), Im16()); // LD HL, nn


    op_table.load8( 0x16, DstR8(D), Im8()); // LD D,n
    op_table.load8( 0x1E, DstR8(E), Im8()); // LD E,n
    op_table.load8( 0x26, DstR8(H), Im8()); // LD H,n
    op_table.load8( 0x2E, DstR8(L), Im8()); // LD L,n
    // 0x36 => Some(LoadInstr(Load_addr_R2_u8(HL))),
    op_table.load8( 0x36, AddrDst(HL), Im8()); // LD (HL),u8


    op_table.load8(0x12,AddrDst(DE), SrcR8(A)); // LD L,A
    op_table.load8(0xEA,Dst8::MemIm16(), SrcR8(A)); // LD L,A
    op_table.load8(0xF0,DstR8(A), Src8::HiMemIm8());
    op_table.load8(0xFA,DstR8(A),Src8::MemIm16());
    op_table.load8(0xE0,Dst8::HiMemIm8(), SrcR8(A));
    op_table.load8(0xE2,Dst8::HiMemR8(C), SrcR8(A));
    op_table.load8(0x3E,DstR8(A), Im8());
    op_table.load8(0x2A,DstR8(A), Src8::MemWithInc(HL));
    op_table.load8(0x3A,DstR8(A), Src8::MemWithDec(HL));
    op_table.load8(0x22,Dst8::MemWithInc(HL), SrcR8(A));
    op_table.load8(0x32,Dst8::MemWithDec(HL), SrcR8(A));


    op_table.add(Op { code:0xF3, len: 1, cycles:4, typ:OpType::DisableInterrupts(), });
    op_table.add(Op { code: 0xFE, len: 2,  cycles: 8, typ: OpType::Compare(DstR8(A),Im8()), });
    op_table.add(Op { code: 0xBE, len: 1,  cycles: 8, typ: OpType::Compare(DstR8(A), Mem(HL))});
    op_table.add(Op { code:0xFB, len: 1, cycles:4, typ:OpType::EnableInterrupts(), });




    op_table.add(Op { code:0x03, len: 1, cycles: 8, typ: Inc16(BC)  });
    op_table.add(Op { code:0x13, len: 1, cycles: 8, typ: Inc16(DE)  });
    op_table.add(Op { code:0x23, len: 1, cycles: 8, typ: Inc16(HL)  });
    op_table.add(Op { code:0x33, len: 1, cycles: 8, typ: Inc16(SP)  });

    op_table.add(Op { code:0x0B, len: 1, cycles: 8, typ: Dec16(BC)  });
    op_table.add(Op { code:0x1B, len: 1, cycles: 8, typ: Dec16(DE)  });
    op_table.add(Op { code:0x2B, len: 1, cycles: 8, typ: Dec16(HL)  });
    op_table.add(Op { code:0x3B, len: 1, cycles: 8, typ: Dec16(SP)  });

    op_table.add(Op { code:0x04, len: 1, cycles: 4, typ: Inc8(DstR8(B))   });
    op_table.add(Op { code:0x05, len: 1, cycles: 4, typ: Dec8(DstR8(B))   });
    op_table.add(Op { code:0x0C, len: 1, cycles: 4, typ: Inc8(DstR8(C))   });
    op_table.add(Op { code:0x0D, len: 1, cycles: 4, typ: Dec8(DstR8(C))   });
    op_table.add(Op { code:0x14, len: 1, cycles: 4, typ: Inc8(DstR8(D))   });
    op_table.add(Op { code:0x15, len: 1, cycles: 4, typ: Dec8(DstR8(D))   });
    op_table.add(Op { code:0x1C, len: 1, cycles: 4, typ: Inc8(DstR8(E))   });
    op_table.add(Op { code:0x1D, len: 1, cycles: 4, typ: Dec8(DstR8(E))   });

    op_table.add(Op { code:0x24, len: 1, cycles: 4, typ: Inc8(DstR8(H))   });
    op_table.add(Op { code:0x25, len: 1, cycles: 4, typ: Dec8(DstR8(H))   });
    op_table.add(Op { code:0x34, len: 1, cycles: 12,typ: Inc8(AddrDst(HL))   });
    op_table.add(Op { code:0x35, len: 1, cycles: 12,typ: Dec8(AddrDst(HL))   });
    op_table.add(Op { code:0x2C, len: 1, cycles: 4, typ: Inc8(DstR8(L))   });
    op_table.add(Op { code:0x2D, len: 1, cycles: 4, typ: Dec8(DstR8(L))   });

    op_table.add(Op { code:0x3C, len: 1, cycles: 4, typ: Inc8(DstR8(A))   });
    op_table.add(Op { code:0x3D, len: 1, cycles: 4, typ: Dec8(DstR8(A))   });

    op_table.add(Op { code: 0x80, len: 1, cycles: 4, typ: Math(Add, DstR8(A), SrcR8(B))});
    op_table.add(Op { code: 0x81, len: 1, cycles: 4, typ: Math(Add, DstR8(A), SrcR8(C))});
    op_table.add(Op { code: 0x82, len: 1, cycles: 4, typ: Math(Add, DstR8(A), SrcR8(D))});
    op_table.add(Op { code: 0x83, len: 1, cycles: 4, typ: Math(Add, DstR8(A), SrcR8(E))});
    op_table.add(Op { code: 0x84, len: 1, cycles: 4, typ: Math(Add, DstR8(A), SrcR8(H))});
    op_table.add(Op { code: 0x85, len: 1, cycles: 4, typ: Math(Add, DstR8(A), SrcR8(L))});
    op_table.add(Op { code: 0x86, len: 1, cycles: 8, typ: Math(Add, DstR8(A), Mem(HL))});
    op_table.add(Op { code: 0x87, len: 1, cycles: 4, typ: Math(Add, DstR8(A), SrcR8(A))});


    op_table.add(Op { code: 0x90, len: 1, cycles: 4, typ: Math(SUB, DstR8(A), SrcR8(B))});
    op_table.add(Op { code: 0x91, len: 1, cycles: 4, typ: Math(SUB, DstR8(A), SrcR8(C))});
    op_table.add(Op { code: 0x92, len: 1, cycles: 4, typ: Math(SUB, DstR8(A), SrcR8(D))});
    op_table.add(Op { code: 0x93, len: 1, cycles: 4, typ: Math(SUB, DstR8(A), SrcR8(E))});
    op_table.add(Op { code: 0x94, len: 1, cycles: 4, typ: Math(SUB, DstR8(A), SrcR8(H))});
    op_table.add(Op { code: 0x95, len: 1, cycles: 4, typ: Math(SUB, DstR8(A), SrcR8(L))});
    op_table.add(Op { code: 0x96, len: 1, cycles: 8, typ: Math(SUB, DstR8(A), Mem(HL))});
    op_table.add(Op { code: 0x97, len: 1, cycles: 4, typ: Math(SUB, DstR8(A), SrcR8(A))});

    op_table.add(Op { code: 0xA0, len: 1, cycles: 4, typ: Math(And,DstR8(A),SrcR8(B)) });
    op_table.add(Op { code: 0xA1, len: 1, cycles: 4, typ: Math(And,DstR8(A),SrcR8(C)) });
    op_table.add(Op { code: 0xA2, len: 1, cycles: 4, typ: Math(And,DstR8(A),SrcR8(D)) });
    op_table.add(Op { code: 0xA3, len: 1, cycles: 4, typ: Math(And,DstR8(A),SrcR8(E)) });
    op_table.add(Op { code: 0xA4, len: 1, cycles: 4, typ: Math(And,DstR8(A),SrcR8(H)) });
    op_table.add(Op { code: 0xA5, len: 1, cycles: 4, typ: Math(And,DstR8(A),SrcR8(L)) });
    op_table.add(Op { code: 0xA6, len: 1, cycles: 8, typ: Math(And, DstR8(A),Src8::Mem(HL)) });
    op_table.add(Op { code: 0xA7, len: 1, cycles: 4, typ: Math(And,DstR8(A),SrcR8(A)) });
    op_table.add(Op { code: 0xA9, len: 1, cycles: 4, typ: Math(Xor,DstR8(A),SrcR8(C)) });
    op_table.add(Op { code: 0xAA, len: 1, cycles: 4, typ: Math(Xor,DstR8(A),SrcR8(D)) });
    op_table.add(Op { code: 0xAB, len: 1, cycles: 4, typ: Math(Xor,DstR8(A),SrcR8(E)) });
    op_table.add(Op { code: 0xAC, len: 1, cycles: 4, typ: Math(Xor,DstR8(A),SrcR8(H)) });
    op_table.add(Op { code: 0xAD, len: 1, cycles: 4, typ: Math(Xor,DstR8(A),SrcR8(L)) });
    op_table.add(Op { code: 0xAE, len: 1, cycles: 8, typ: Math(Xor,DstR8(A),Src8::Mem(HL)) });
    op_table.add(Op { code: 0xAF, len: 1, cycles: 4, typ: Math(Xor,DstR8(A),SrcR8(A)) });

    op_table.add(Op { code: 0xB0, len: 1, cycles: 4, typ: Math(Or, DstR8(A),SrcR8(B)) });
    op_table.add(Op { code: 0xB1, len: 1, cycles: 4, typ: Math(Or, DstR8(A),SrcR8(C)) });
    op_table.add(Op { code: 0xB2, len: 1, cycles: 4, typ: Math(Or, DstR8(A),SrcR8(D)) });
    op_table.add(Op { code: 0xB3, len: 1, cycles: 4, typ: Math(Or, DstR8(A),SrcR8(E)) });
    op_table.add(Op { code: 0xB4, len: 1, cycles: 4, typ: Math(Or, DstR8(A),SrcR8(H)) });
    op_table.add(Op { code: 0xB5, len: 1, cycles: 4, typ: Math(Or, DstR8(A),SrcR8(L)) });
    op_table.add(Op { code: 0xB6, len: 1, cycles: 8, typ: Math(Or, DstR8(A),Src8::Mem(HL)) });
    op_table.add(Op { code: 0xB7, len: 1, cycles: 4, typ: Math(Or, DstR8(A),SrcR8(A)) });


    op_table.add(Op { code: 0x09, len: 1, cycles: 8, typ:Math16(Add,DstR16(HL), SrcR16(BC))});
    op_table.add(Op { code: 0x19, len: 1, cycles: 8, typ:Math16(Add,DstR16(HL), SrcR16(DE))});
    op_table.add(Op { code: 0x29, len: 1, cycles: 8, typ:Math16(Add,DstR16(HL), SrcR16(HL))});
    op_table.add(Op { code: 0x39, len: 1, cycles: 8, typ:Math16(Add,DstR16(HL), SrcR16(SP))});


    op_table.add(Op { code: 0xC6, len: 2, cycles: 8, typ: Math(Add, DstR8(A), Src8::Im8())});
    op_table.add(Op { code: 0xD6, len: 2, cycles: 8, typ: Math(SUB, DstR8(A), Src8::Im8())});


    op_table.add(Op { code: 0xE6, len: 2, cycles: 8, typ: Math(And, DstR8(A), Im8())});
    op_table.add(Op { code: 0xEE, len: 1, cycles: 8, typ: Math(Xor, DstR8(A),Src8::Im8()) });


    op_table.add(Op { code: 0xC3, len: 3, cycles: 12, typ: Jump(Absolute(AddrSrc::Imu16()) )});
    op_table.add(Op { code: 0x20, len: 2, cycles: 12, typ: Jump(RelativeCond(NotZero(), Im8())) });
    op_table.add(Op { code: 0x30, len: 2, cycles: 12, typ: Jump(RelativeCond(NotCarry(),Im8())) });
    op_table.add(Op { code: 0x38, len: 2, cycles:  8, typ: Jump(RelativeCond(Carry(),   Im8())) });
    op_table.add(Op { code: 0x28, len: 2, cycles: 12, typ: Jump(RelativeCond(Zero(),    Im8())) });
    op_table.add(Op { code: 0x18, len: 2, cycles: 12, typ: Jump(Relative(Im8())) });
    op_table.add(Op { code: 0xE9, len: 1, cycles:  4, typ: Jump(Absolute(AddrSrc::Src16(HL)))});
    op_table.add(Op { code: 0xCA, len: 3, cycles: 12, typ: Jump(AbsoluteCond(Zero(),AddrSrc::Imu16()) )});


    op_table.add(Op{code:0x17, len:1, cycles:4, typ: BitOp(BitOps::RLA())});
    op_table.add(Op{code:0x1F, len:1, cycles:4, typ: BitOp(BitOps::RRA())});
    op_table.add(Op{code:0x2F, len:1, cycles:4, typ: BitOp(BitOps::CPL())});

    // almost the entire lower CB chart
    let r8list = [B,C,D,E,H,L];
    for (i, r8) in r8list.iter().enumerate() {
        let col = (i as u16);
        op_table.bitop(0xCB_00 + col,RLC(*r8));
        op_table.bitop(0xCB_08 + col,RRC(*r8));
        op_table.bitop(0xCB_10 + col,RL(*r8));
        op_table.bitop(0xCB_18 + col,RR(*r8));
        op_table.bitop(0xCB_20 + col,SLA(*r8));
        op_table.bitop(0xCB_28 + col,SRA(*r8));
        op_table.bitop(0xCB_30 + col,SWAP(*r8));
        op_table.bitop(0xCB_38 + col,SRL(*r8));
        op_table.bitop(0xCB_40 + col,BIT(0,*r8));
        op_table.bitop(0xCB_48 + col,BIT(1,*r8));
        op_table.bitop(0xCB_50 + col,BIT(2,*r8));
        op_table.bitop(0xCB_58 + col,BIT(3,*r8));
        op_table.bitop(0xCB_60 + col,BIT(4,*r8));
        op_table.bitop(0xCB_68 + col,BIT(5,*r8));
        op_table.bitop(0xCB_70 + col,BIT(6,*r8));
        op_table.bitop(0xCB_78 + col,BIT(7,*r8));

        op_table.bitop(0xCB_80 + col,RES(0, *r8));
        op_table.bitop(0xCB_88 + col,RES(1, *r8));
        op_table.bitop(0xCB_90 + col,RES(2, *r8));
        op_table.bitop(0xCB_98 + col,RES(3, *r8));
        op_table.bitop(0xCB_A0 + col,RES(4, *r8));
        op_table.bitop(0xCB_A8 + col,RES(5, *r8));
        op_table.bitop(0xCB_B0 + col,RES(6, *r8));
        op_table.bitop(0xCB_B8 + col,RES(7, *r8));

        op_table.bitop(0xCB_C0 + col,SET(0, *r8));
        op_table.bitop(0xCB_C8 + col,SET(1, *r8));
        op_table.bitop(0xCB_D0 + col,SET(2, *r8));
        op_table.bitop(0xCB_D8 + col,SET(3, *r8));
        op_table.bitop(0xCB_E0 + col,SET(4, *r8));
        op_table.bitop(0xCB_E8 + col,SET(5, *r8));
        op_table.bitop(0xCB_F0 + col,SET(6, *r8));
        op_table.bitop(0xCB_F8 + col,SET(7, *r8));
    }

    op_table.add(Op{ code: 0xCB_37,len:2, cycles:8,   typ: BitOp(SWAP(A))});
    op_table.add(Op{ code: 0xCB_87,len:2, cycles:8,   typ: BitOp(RES(0,A))});

    op_table.add(Op{ code: 0x00C0, len:1, cycles: 20, typ: Call(RetCond(NotZero()))});
    op_table.add(Op{ code: 0x00C4, len:3, cycles: 24, typ: Call(CallCondU16(NotZero()))});
    op_table.add(Op{ code: 0x00C8, len:1, cycles: 20, typ: Call(RetCond(Zero()))});
    op_table.add(Op{ code: 0x00C9, len:1, cycles: 16, typ: Call(Ret())});
    op_table.add(Op{ code: 0x00CC, len:3, cycles: 24, typ: Call(CallCondU16(Zero()))});
    op_table.add(Op{ code: 0x00CD, len:3, cycles: 24, typ: Call(CallU16())});

    op_table.add(Op{ code: 0x00CF, len:1, cycles: 16, typ: Jump(Restart(0x08))});
    op_table.add(Op{ code: 0x00DF, len:1, cycles: 16, typ: Jump(Restart(0x18))});
    op_table.add(Op{ code: 0x00EF, len:1, cycles: 16, typ: Jump(Restart(0x28))});
    op_table.add(Op{ code: 0x00FF, len:1, cycles: 16, typ: Jump(Restart(0x38))});


    op_table.add(Op{ code: 0x00C1, len:1, cycles: 12, typ: Call(Pop(BC))});
    op_table.add(Op{ code: 0x00C5, len:1, cycles: 16, typ: Call(Push(BC))});
    op_table.add(Op{ code: 0x00D1, len:1, cycles: 12, typ: Call(Pop(DE))});
    op_table.add(Op{ code: 0x00D5, len:1, cycles: 16, typ: Call(Push(DE))});
    op_table.add(Op{ code: 0x00E1, len:1, cycles: 12, typ: Call(Pop(HL))});
    op_table.add(Op{ code: 0x00E5, len:1, cycles: 16, typ: Call(Push(HL))});
    op_table.add(Op{ code: 0x00F1, len:1, cycles: 16, typ: Call(Pop(AF))});
    op_table.add(Op{ code: 0x00F5, len:1, cycles: 16, typ: Call(Push(AF))});

    // op_table.add(Op{ code: 0x00F1, len:1, cycles: 12, typ: OpType::Call(CallType::Pop(AF))});
    // op_table.add(Op{ code: 0x00F5, len:1, cycles: 16, typ: OpType::Call(CallType::Push(AF))});

    op_table
}

pub fn setup_test_rom(fname: &str) -> Option<GBState> {
    match load_romfile(&PathBuf::from(fname)) {
        Ok(cart) => {
            let mut gb = GBState {
                cpu: CPU::init(),
                mmu: MMU2::init(&cart.data),
                ppu: PPU2::init(),
                clock: 0,
                count: 0,
                ops: make_op_table(),
                debug: false
            };
            gb.set_pc(0x100);
            return Some(gb);
        }
        Err(_) => return None
    }
}

#[test]
fn test_cpuins() {
    let mut gb = setup_test_rom("./resources/testroms/cpu_instrs/individual/10-bit ops.gb").unwrap();

    loop {
        println!("PC {:04x}",gb.cpu.get_pc());
        let prev_pc = gb.cpu.get_pc();
        gb.execute();
        // op.execute(&mut gb);
        if gb.cpu.get_pc() == prev_pc {
            println!("stuck in an infinite loop. pc = {:04x}",gb.cpu.get_pc());
            gb.dump_current_state();
            panic!("stuck in an infinite loop");
        }
    }
    let goal = 7;
    println!("hopefully we reached count = {}  really = {} ", goal,gb.count);
    assert_eq!(gb.count>=goal,true);
}

#[test]
fn test_hellogithub() {
    let mut gb = setup_test_rom("./resources/testroms/hello-world.gb").unwrap();
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
            println!("PC {:04x}    clock = {}   count = {}", gb.cpu.get_pc(), gb.clock, gb.count);
        }
        let prev_pc = gb.cpu.get_pc();
        gb.execute();
        if debug {
            println!("after A:{:02x} B:{:02x} C:{:02x}     BC:{:04x} DE:{:04x} HL:{:04x}  Z={}  C={}",
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
            println!("stuck in an infinite loop. pc = {:04x}",gb.cpu.get_pc());
            gb.dump_current_state();
            panic!("stuck in an infinite loop");
            break;
        }

        if gb.count % 20 == 0 {
            let mut v = gb.mmu.read8_IO(IORegister::LY);
            if v >= 154 {
                v = 0;
            }
            gb.mmu.write8_IO(IORegister::LY,v+1);
        }

        if gb.count > goal {
            break;
        }
    }
    println!("hopefully we reached count = {}  really = {} ", goal,gb.count);
    gb.dump_current_state();
    assert_eq!(gb.cpu.get_pc(),0x35);
}

#[test]
fn test_bootrom() {
    let op_table = make_op_table();
    let pth = Path::new("./resources/testroms/hello-world.gb");
    let data:Vec<u8> = fs::read(pth).unwrap();

    let mut gb = GBState {
        cpu: CPU::init(),
        mmu: MMU2::init(&data),
        ppu: PPU2::init(),
        clock: 0,
        count: 0,
        ops:make_op_table(),
        debug: false
    };
    gb.mmu.enable_bootrom();

    let goal = 3_250_000;
    gb.set_pc(0);

    let mut debug = false;
    loop {
        if debug {
            println!("==========");
            println!("PC {:04x}    clock = {}   count = {}", gb.cpu.get_pc(), gb.clock, gb.count);
            println!("LY = {:02x}",gb.mmu.read8(0xFF44))
        }
        let opcode = fetch_opcode_from_memory(&gb);
        if let None = op_table.lookup(&opcode) {
            println!("failed to lookup op for code {:04x}",opcode);
            break;
        }
        let op = op_table.lookup(&opcode).unwrap();
        if debug {
            println!("PC {:04x} {:04x}  =  {}      ({},{})", gb.cpu.get_pc(), op.code, op.to_asm(), op.len, op.cycles);
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
            println!("stuck in an infinite loop. pc = {:04x}",gb.cpu.get_pc());
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
            0x0055 => println!("setting scroll count to {:02x}",gb.cpu.get_r8(CPUR8::R8H)),
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
            },
            _ => {
                // println!("PC {:04x}",pc);
            }
        }


        if gb.count % 500 == 0 {
            let mut v = gb.mmu.read8_IO(IORegister::LY);
            if v >= 154 {
                v = 0;
            }
            gb.mmu.write8_IO(IORegister::LY,v+1);
        }

        if gb.count > goal {
            break;
        }
    }
    println!("PC {:04x}",gb.cpu.get_pc());
    println!("hopefully we reached count = {}  really = {} ", goal,gb.count);
    assert_eq!(gb.mmu.read8(0x0100),0xF3);
    // assert_eq!(gb.count>=goal,true);
}

#[test]
fn test_tetris() {
    let op_table = make_op_table();
    let pth = Path::new("./resources/testroms/tetris.gb");
    let data:Vec<u8> = fs::read(pth).unwrap();

    let mut gb = GBState {
        cpu: CPU::init(),
        mmu: MMU2::init(&data),
        ppu: PPU2::init(),
        clock: 0,
        count: 0,
        ops:make_op_table(),
        debug: false
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
            },
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

            0x2828 => println!("checking ly {:04x}",gb.mmu.read8_IO(IORegister::LY)),
            0x282e => println!("end of vblank wait"),
            // 0x0217 => println!("made it past the loop"),
            _ => {}
        }
        if gb.cpu.get_pc() == 0x33 {
            gb.dump_current_state();
            println!("got to 33, jump to contents of HL {:04x}",gb.cpu.get_r16(CPUR16::HL));
            debug = true;
        }
        if debug {
            println!("==========");
            println!("PC {:04x}    clock = {}   count = {}", gb.cpu.get_pc(), gb.clock, gb.count);
            println!("LY = {:02x}",gb.mmu.read8(0xFF44))
        }
        let opcode = fetch_opcode_from_memory(&gb);
        if let None = op_table.lookup(&opcode) {
            println!("failed to lookup op for code {:04x}",opcode);
            println!("current PC is {:04x}",gb.cpu.get_pc());
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
            println!("stuck in an infinite loop. pc = {:04x}",gb.cpu.get_pc());
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
            let mut v = gb.mmu.read8_IO(IORegister::LY);
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
            gb.mmu.write8_IO(IORegister::LY,v2);
        }

        if gb.count > goal {
            break;
        }
    }
    println!("PC {:04x}",gb.cpu.get_pc());
    println!("hopefully we reached count = {}  really = {} ", goal,gb.count);
    assert_eq!(gb.mmu.read8(0x0100),0xF3);
    // assert_eq!(gb.count>=goal,true);
}

#[test]
fn read_n_right_test() {
    let mut mmu:MMU2 = MMU2::init_empty(0x66);
    assert_eq!(mmu.read8(0x142),0x66);
    mmu.write8(0x142,0x42);
    assert_eq!(mmu.read8(0x142),0x42);
    mmu.write8_IO(IORegister::LY,0x85);
    assert_eq!(mmu.read8_IO(IORegister::LY),0x85);
}

#[test]
fn op_tests() {
    let mut gb = GBState {
        cpu: CPU::init(),
        mmu: MMU2::init_empty(0xDE),
        ppu: PPU2::init(),
        ops:make_op_table(),
        clock: 0,
        count: 0,
        debug: false
    };
    let op_table = make_op_table();
    if let Some(op)= op_table.lookup(&0x36) {
        println!("op is {:?}", op);
        assert_eq!(op.len,2);
        assert_eq!(op.cycles,12);

        let addr = 0x8000;
        let payload = 0x99;
        let pc = 0x200;

        //set pc to x200
        gb.set_pc(pc);

        //put x99 as the i8
        gb.mmu.write8(pc+0,0x36);
        gb.mmu.write8(pc+1,payload);

        //set HL to x8000
        gb.cpu.set_r16(CPUR16::HL,addr);

        //confirm x8000 has xDE in it
        assert_eq!(gb.mmu.read8(addr),0xDE);

        //execute instruction
        gb.execute();

        //confirm x8000 now has x99 in it
        assert_eq!(gb.mmu.read8(addr),payload);

        //confirm PC is now x202
        assert_eq!(gb.cpu.get_pc(),pc+2);
        println!("HL loaded to 0x8000 {:04x} {:02x}",addr,gb.mmu.read8(addr));
    } else {
        gb.dump_current_state();
        panic!("op not found");
    }

}

#[test]
fn test_write_register_A() {
    let rom:[u8;2] = [0x3e,0x05]; // LD A, 0x05
    let mut gb:GBState = GBState::make_test_context(&rom.to_vec());
    gb.execute();
    // println!("op is {:?}", op);
    assert_eq!(gb.cpu.get_r8(CPUR8::R8A),0x05);
}

#[test]
fn test_write_register_D() {
    let rom:[u8;2] = [0x16,0x05]; // LD D, 0x05
    let mut gb:GBState = GBState::make_test_context(&rom.to_vec());
    gb.execute();
    // let code = gb.fetch_next_opcode();
    // let op = gb.ops.lookup(&code).unwrap();
    assert_eq!(gb.cpu.get_r8(CPUR8::R8D),0x05);
}

#[test]
fn test_write_register_DE() {
    let rom:[u8;2] = [0x16,0x05]; // LD D, 0x05
    let mut gb:GBState = GBState::make_test_context(&rom.to_vec());
    gb.execute();
    assert_eq!(gb.cpu.get_r8(CPUR8::R8D),0x05);
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

