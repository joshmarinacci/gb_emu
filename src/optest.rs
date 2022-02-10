use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use BinOp::{ADC, And, Or, SUB, Xor};
use BitOps::{BIT, CCF, CPL, DAA, RES, RLA, RLCA, RRA, RRCA, SCF, SET};
use CallType::{CallU16, Pop, Push, Ret, RetCond};
use Cond::{Carry, NotCarry, NotZero, Zero};
use Dst16::DstR16;
use Dst8::AddrDst;
use JumpType::{Absolute, Restart};
use OpType::{BitOp, Call, Dec8, DisableInterrupts, Inc8, Jump, Load16, Math, Noop};
use Src16::{SrcR16};
use CPUR8::{R8A, R8B, R8C};
use crate::common::{get_bit_as_bool, load_romfile, MBC, set_bit, u8_as_i8};
use crate::cpu2::CPUR8::{R8D, R8E, R8H, R8L};
use crate::cpu2::{CPU, CPUR16, CPUR8};
use crate::mmu2::{IORegister, MMU2};
use crate::optest::BinOp::{Add, SBC};
use crate::optest::BitOps::{RL, RLC, RR, RRC, SLA, SRA, SRL, SWAP};
use crate::optest::CallType::{CallCondU16, RetI};
use crate::optest::Dst8::DstR8;
use crate::optest::JumpType::{AbsoluteCond, Relative, RelativeCond};
use crate::optest::BinOp16::Add16;
use crate::optest::OpType::{AddSP, Compare, EnableInterrupts, Halt, Load8, Math16, Math16u};
use crate::optest::Src8::{HiMemIm8, Im8, Mem, SrcR8};
use crate::optest::R16::{AF, BC, DE, HL, SP};
use crate::optest::R8::{A, B, C, D, E, H, L};
use crate::optest::UnOp16::{Dec16, Inc16};
use crate::ppu2::PPU2;

#[derive(Debug, Copy, Clone)]
pub enum R8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Debug, Copy, Clone)]
pub enum R16 {
    BC,
    HL,
    DE,
    SP,
    AF,
}

#[derive(Debug, Copy, Clone)]
pub enum Src8 {
    SrcR8(R8),
    Mem(R16),
    MemWithInc(R16),
    MemWithDec(R16),
    Im8(),
    HiMemIm8(),
    HiMemR8(R8),
    MemIm16(),
}

#[derive(Debug, Copy, Clone)]
pub enum Src16 {
    SrcR16(R16),
    SrcR16WithOffset(R16),
    Im16(),
}

#[derive(Debug, Copy, Clone)]
pub enum Dst8 {
    DstR8(R8),
    AddrDst(R16),
    HiMemIm8(),
    HiMemR8(R8),
    MemIm16(),
    MemWithInc(R16),
    MemWithDec(R16),
}

#[derive(Debug, Copy, Clone)]
pub enum Dst16 {
    DstR16(R16),
    MemIm16(),
}

#[derive(Debug, Copy, Clone)]
enum AddrSrc {
    Imu16(),
    Src16(R16),
}

#[derive(Debug, Copy, Clone)]
enum BinOp {
    Or,
    Xor,
    And,
    SUB,
    Add,
    ADC,
    SBC,
}

#[derive(Debug, Copy, Clone)]
enum BinOp16 {
    Add16,
}
#[derive(Debug, Copy, Clone)]
enum UnOp16 {
    Inc16(R16),
    Dec16(R16),
}

#[derive(Debug, Copy, Clone)]
enum BitOps {
    BIT(u8, Dst8),
    RES(u8, Dst8),
    SET(u8, Dst8),
    RLC(Dst8),
    RRC(Dst8),
    RL(Dst8),
    RR(Dst8),
    SLA(Dst8),
    SRA(Dst8),
    SRL(Dst8),
    SWAP(Dst8),
    RLA(),
    RLCA(),
    RRA(),
    RRCA(),
    CPL(),
    DAA(),
    SCF(),
    CCF(),
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
    AbsoluteCond(Cond, AddrSrc),
    RelativeCond(Cond, Src8),
    Relative(Src8),
    Restart(u8),
}

#[derive(Debug, Copy, Clone)]
enum CallType {
    CallU16(),
    CallCondU16(Cond),
    Ret(),
    RetI(),
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
    Halt(),
    Compare(Dst8, Src8),
    Math(BinOp, Dst8, Src8),
    AddSP(Dst16,Src8),
    Math16(BinOp16, Dst16, Src16),
    Math16u(UnOp16),
    Inc8(Dst8),
    Dec8(Dst8),
    BitOp(BitOps),
}

#[derive(Debug, Clone)]
pub struct Op {
    code: u16,
    len: u16,
    cycles: u16,
    typ: OpType,
}

pub struct GBState {
    pub cpu: CPU,
    pub mmu: MMU2,
    pub ppu: PPU2,
    pub clock: u32,
    pub count: u32,
    ops: OpTable,
    pub debug: bool,
}

impl GBState {
    pub fn lookup_op(&self, code: &u16) -> Option<&Op> {
        self.ops.ops.get(code)
    }
}

impl GBState {
    pub fn draw_full_screen(&mut self) {
        self.ppu.draw_full_screen(&self.mmu);
    }
    pub fn get_pc(&self) -> u16 {
        self.cpu.get_pc()
    }
    pub fn set_pc(&mut self, pc: u16) {
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
}

impl GBState {
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
    fn set_value(&self, gb: &mut GBState, value: u8) {
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
    fn name(&self) -> &'static str {
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
    fn set_value(&self, gb: &mut GBState, val: u16) {
        match self {
            BC => gb.cpu.set_r16(CPUR16::BC, val),
            HL => gb.cpu.set_r16(CPUR16::HL, val),
            DE => gb.cpu.set_r16(CPUR16::DE, val),
            SP => gb.cpu.set_r16(CPUR16::SP, val),
            AF => gb.cpu.set_r16(CPUR16::AF, val),
        }
    }
    fn name(&self) -> &'static str {
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
    fn real(&self, gb: &GBState) -> String {
        match self {
            AddrSrc::Imu16() => format!("{:04x}", gb.mmu.read16(gb.cpu.get_pc() + 1)),
            AddrSrc::Src16(r) => format!("{:04x}", r.get_value(gb)),
        }
    }
    pub(crate) fn get_addr(&self, gb: &GBState) -> u16 {
        match self {
            AddrSrc::Imu16() => gb.mmu.read16(gb.cpu.get_pc() + 1),
            AddrSrc::Src16(r) => r.get_value(gb),
        }
    }
}

impl Src16 {
    fn name(&self) -> String {
        match self {
            Src16::Im16() => "nn".to_string(),
            SrcR16(r) => r.name().to_string(),
            Src16::SrcR16WithOffset(r) => r.name().to_string(),
        }
    }
    fn get_value(&self, gb: &GBState) -> u16 {
        match self {
            Src16::Im16() => gb.mmu.read16(gb.cpu.get_pc() + 1),
            SrcR16(r) => r.get_value(gb),
            Src16::SrcR16WithOffset(r) => {
                let e = u8_as_i8(gb.mmu.read8(gb.cpu.get_pc()+1));
                ((r.get_value(gb) as i32) + e as i32) as u16
            }
        }
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            Src16::Im16() => format!("{:04x}", gb.mmu.read16(gb.cpu.get_pc() + 1)),
            SrcR16(r) => format!("{:04x}", r.get_value(gb)),
            Src16::SrcR16WithOffset(r) => format!("{:04x} + {}",r.get_value(gb),gb.mmu.read8(gb.cpu.get_pc()+1)),
        }
    }
}

impl Dst16 {
    fn name(&self) -> String {
        match self {
            DstR16(r2) => r2.name().to_string(),
            Dst16::MemIm16() => "(nn)".to_string(),
        }
    }
    fn set_value(&self, gb: &mut GBState, val: u16) {
        match self {
            DstR16(r2) => r2.set_value(gb, val),
            Dst16::MemIm16() => {
                let addr = gb.mmu.read16(gb.cpu.get_pc()+1);
                gb.mmu.write16(addr,val);
            }
        }
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            DstR16(rr) => format!("{:04x}", rr.get_value(gb)),
            Dst16::MemIm16() => format!("({:04x})", gb.mmu.read16(gb.cpu.get_pc()+1)),
        }
    }
    fn get_value(&self, gb: &GBState) -> u16 {
        match self {
            DstR16(rr) => rr.get_value(gb),
            Dst16::MemIm16() => {
                let addr = gb.mmu.read16(gb.cpu.get_pc()+1);
                gb.mmu.read16(addr)
            }
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
            ADC => "ADC",
            SBC => "SBC",
        }
    }
}
impl BinOp16 {
    fn name(&self) -> &str {
        match self {
            Add16 => "ADD",
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
            NotZero() => !gb.cpu.r.zero,
        }
    }
}

impl Src8 {
    fn name(&self) -> String {
        match self {
            SrcR8(r1) => r1.name().to_string(),
            Mem(r2) => format!("({})", r2.name()),
            Im8() => "u8".to_string(),
            Src8::HiMemIm8() => "($FF00+n)".to_string(),
            Src8::MemIm16() => "(nn)".to_string(),
            Src8::MemWithInc(r2) => format!("({}+)", r2.name()),
            Src8::MemWithDec(r2) => format!("({}-)", r2.name()),
            Src8::HiMemR8(r) => format!("($FF00+{}",r.name()).to_string(),
        }
    }
    fn get_value(&self, gb: &GBState) -> u8 {
        match self {
            SrcR8(r1) => r1.get_value(gb),
            Mem(r2) => {
                // println!("reading memory at location {:04x}",r2.get_value(&gb.cpu));
                gb.mmu.read8(r2.get_value(gb))
            }
            Src8::MemWithInc(r2) => gb.mmu.read8(r2.get_value(gb)),
            Src8::MemWithDec(r2) => gb.mmu.read8(r2.get_value(gb)),
            Im8() => gb.mmu.read8(gb.cpu.get_pc() + 1),
            Src8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc() + 1);
                gb.mmu.read8(0xFF00 + im as u16)
            }
            Src8::MemIm16() => {
                let addr = gb.mmu.read16(gb.cpu.get_pc() + 1);
                gb.mmu.read8(addr)
            }
            Src8::HiMemR8(r) => {
                let im = r.get_value(gb);
                gb.mmu.read8(0xFF00 + im as u16)
            }
        }
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc() + 1);
                let addr = 0xFF00 + im as u16;
                let name = named_addr(addr, gb);
                return format!("{} ({:02x})", name, self.get_value(gb));
            }
            _ => {}
        }
        format!("{:02x}", self.get_value(gb))
    }
}

impl Dst8 {
    fn name(&self) -> String {
        match self {
            DstR8(r8) => r8.name().to_string(),
            AddrDst(r16) => r16.name().to_string(),
            Dst8::MemIm16() => "(nn)".to_string(),
            Dst8::HiMemIm8() => "($FF00+n)".to_string(),
            Dst8::HiMemR8(r8) => format!("($FF00+{})", r8.name()).to_string(),
            Dst8::MemWithInc(r16) => format!("({}+)", r16.name()).to_string(),
            Dst8::MemWithDec(r16) => format!("({}-)", r16.name()).to_string(),
        }
    }
    fn set_value(&self, gb: &mut GBState, val: u8) {
        match self {
            DstR8(r8) => r8.set_value(gb, val),
            AddrDst(r16) => gb.mmu.write8(r16.get_value(gb), val),
            Dst8::MemIm16() => gb.mmu.write8(gb.mmu.read16(gb.cpu.get_pc() + 1), val),
            Dst8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc() + 1);
                gb.mmu.write8(0xFF00 + im as u16, val)
            }
            Dst8::HiMemR8(r8) => {
                let addr = 0xFF00 + (r8.get_value(gb) as u16);
                gb.mmu.write8(addr, val)
            }
            Dst8::MemWithInc(r16) => gb.mmu.write8(r16.get_value(gb), val),
            Dst8::MemWithDec(r16) => gb.mmu.write8(r16.get_value(gb), val),
        }
    }
    fn get_value(&self, gb: &GBState) -> u8 {
        match self {
            DstR8(r1) => r1.get_value(gb),
            AddrDst(r2) => gb.mmu.read8(r2.get_value(gb)),
            Dst8::MemIm16() => {
                let addr = gb.mmu.read16(gb.cpu.get_pc() + 1);
                gb.mmu.read8(addr)
            }
            Dst8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc() + 1);
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
            DstR8(r8) => format!("{}", r8.name()),
            AddrDst(addr) => format!("{:02x}", gb.mmu.read8(addr.get_value(gb))),
            Dst8::MemIm16() => {
                let addr = gb.mmu.read16(gb.cpu.get_pc() + 1);
                named_addr(addr, gb)
            }
            Dst8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc() + 1);
                let addr = 0xFF00 + im as u16;
                named_addr(addr, gb)
            }
            Dst8::HiMemR8(r8) => {
                let addr = 0xFF00 + (r8.get_value(gb) as u16);
                named_addr(addr, gb)
            }
            Dst8::MemWithInc(r16) => format!("{:02x}", gb.mmu.read8(r16.get_value(gb))),
            Dst8::MemWithDec(r16) => format!("{:02x}", gb.mmu.read8(r16.get_value(gb))),
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
                self.execute_op(&op);
            }
            self.ppu.update(&mut self.mmu, self.clock);
            {
                //update DIV
                let mut div = self.mmu.read8_IO(&IORegister::DIV);
                if self.count % 5 == 0 {
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
                    let mut TIMA = self.mmu.read8_IO(&IORegister::TIMA);
                    let mut TMA = self.mmu.read8_IO(&IORegister::TMA);
                    if self.count % factor == 0 {
                        let (t2,over) = TIMA.overflowing_add(1);
                        if over {
                            TIMA = TMA;
                            self.mmu.set_IO_bit(&IORegister::IF, 2, true);
                        } else {
                            TIMA = t2;
                        }
                        self.mmu.write8_IO_raw(IORegister::TIMA,TIMA);
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

    fn trigger_interrupt(&mut self, handler_address:u16, bit:u8) {
        println!("doing interrupt for bit {}",bit);
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
        println!("some other interrupt requested but not handled");
        println!("IE ={:08b}",self.mmu.read8_IO(&IORegister::IE));
        println!("IF ={:08b}",self.mmu.read8_IO(&IORegister::IF));
    }
}


const VBLANK_HANDLER_ADDRESS:u16 = 0x40;
const STAT_HANDLER_ADDRESS:u16 = 0x48;
const TIMER_HANDLER_ADDRESS:u16 = 0x50;
const SERIAL_HANDLER_ADDRESS:u16 = 0x58;
const JOYPAD_HANDLER_ADDRESS:u16 = 0x60;



impl GBState {
    pub(crate) fn execute_op(&mut self, op: &Op) {
        match &op.typ {
            Noop() => self.set_pc(self.cpu.get_pc() + op.len),
            Jump(typ) => {
                match typ {
                    Absolute(src) => {
                        let addr = src.get_addr(self);
                        self.set_pc(addr);
                    }
                    AbsoluteCond(cond, src) => {
                        let addr = src.get_addr(self);
                        self.set_pc(self.cpu.get_pc() + op.len);
                        if cond.get_value(self) {
                            self.set_pc(addr);
                        }
                    }
                    RelativeCond(cond, src) => {
                        let off = src.get_value(self);
                        let e = u8_as_i8(off);
                        self.set_pc(self.cpu.get_pc() + op.len);
                        if cond.get_value(self) {
                            let addr: u16 = (((self.cpu.get_pc()) as i32) + (e as i32)) as u16;
                            self.set_pc(addr);
                        }
                    }
                    Relative(src) => {
                        let e = u8_as_i8(src.get_value(self));
                        //update the pc before calculating the relative address
                        self.set_pc(self.cpu.get_pc() + op.len);
                        let addr: u16 = ((self.cpu.get_pc() as i32) + (e as i32)) as u16;
                        self.set_pc(addr);
                    }
                    Restart(n) => {
                        self.cpu.dec_sp();
                        self.cpu.dec_sp();
                        self.mmu.write16(self.cpu.get_sp(), self.cpu.get_pc() + 1);
                        self.set_pc(*n as u16);
                    }
                }
            }
            Call(typ) => {
                match typ {
                    CallU16() => {
                        let addr = self.mmu.read16(self.cpu.get_pc() + 1);
                        self.cpu.dec_sp();
                        self.cpu.dec_sp();
                        self.mmu.write16(self.cpu.get_sp(), self.cpu.get_pc() + 3);
                        self.set_pc(addr);
                    }
                    CallCondU16(cond) => {
                        let addr = self.mmu.read16(self.cpu.get_pc() + 1);
                        if cond.get_value(self) {
                            self.cpu.dec_sp();
                            self.cpu.dec_sp();
                            self.mmu.write16(self.cpu.get_sp(), self.cpu.get_pc() + 3);
                            self.set_pc(addr);
                        } else {
                            self.set_pc(self.cpu.get_pc() + op.len);
                        }
                    }
                    Push(src) => {
                        self.cpu.dec_sp();
                        self.cpu.dec_sp();
                        let value = src.get_value(self);
                        self.mmu.write16(self.cpu.get_sp(), value);
                        self.set_pc(self.cpu.get_pc() + op.len);
                    }
                    Pop(src) => {
                        let value = self.mmu.read16(self.cpu.get_sp());
                        self.cpu.inc_sp();
                        self.cpu.inc_sp();
                        src.set_value(self, value);
                        // println!("PC:{:04x}  POP {:?}, now HL is, {}", self.cpu.get_pc(), src, self.cpu.reg_to_str());
                        self.set_pc(self.cpu.get_pc() + op.len);
                    }
                    Ret() => {
                        let addr = self.mmu.read16(self.cpu.get_sp());
                        self.cpu.inc_sp();
                        self.cpu.inc_sp();
                        self.set_pc(addr);
                    }
                    RetI() => {
                        // println!("reti   enabling interrupts again");
                        let addr = self.mmu.read16(self.cpu.get_sp());
                        self.cpu.inc_sp();
                        self.cpu.inc_sp();
                        self.set_pc(addr);
                        // println!("ending interrupt");
                        self.cpu.IME = true;
                    }
                    RetCond(cond) => {
                        if cond.get_value(self) {
                            let addr = self.mmu.read16(self.cpu.get_sp());
                            self.cpu.inc_sp();
                            self.cpu.inc_sp();
                            self.set_pc(addr);
                        } else {
                            self.set_pc(self.cpu.get_pc() + op.len);
                        }
                    }
                }
            }
            Load16(dst, src) => {
                let val = src.get_value(self);
                dst.set_value(self, val);
                if let Src16::SrcR16WithOffset(r2) = src {
                    let a = r2.get_value(self);
                    let b = u8_as_i8(self.mmu.read8(self.cpu.get_pc()+1)) as u16;
                    self.cpu.r.zero = false;
                    self.cpu.r.subn = false;
                    self.cpu.r.half  = ((a & 0x000F) + (b & 0x000F) > 0x000F);
                    self.cpu.r.carry = ((a & 0x00FF) + (b & 0x00FF) > 0x00FF);
                }
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            Load8(dst, src) => {
                // println!("executing Load8 dst = {:?} src = {:?}",dst,src);
                let val = src.get_value(self);
                // println!("value from src is {:02x}",val);
                dst.set_value(self, val);
                if let Src8::MemWithInc(r2) = src {
                    let v = r2.get_value(self);
                    let (v2, b) = v.overflowing_add(1);
                    r2.set_value(self, v2);
                }
                if let Src8::MemWithDec(r2) = src {
                    let v = r2.get_value(self);
                    let (v2, b) = v.overflowing_sub(1);
                    r2.set_value(self, v2);
                }
                if let Dst8::MemWithInc(r2) = dst {
                    let v = r2.get_value(self);
                    let (v2, b) = v.overflowing_add(1);
                    r2.set_value(self, v2);
                }
                if let Dst8::MemWithDec(r2) = dst {
                    let v = r2.get_value(self);
                    let (v2, b) = v.overflowing_sub(1);
                    r2.set_value(self, v2);
                }
                // println!("Len is {}",self.len);
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            DisableInterrupts() => {
                // println!("disabling interrupts by DI");
                self.cpu.IME = false;
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            EnableInterrupts() => {
                // println!("enabling interrupts by EI");
                self.cpu.IME = true;
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            Halt() => {
                self.cpu.halt = true;
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            Compare(dst, src) => {
                let src_v = src.get_value(self);
                let dst_v = dst.get_value(self);

                let result = src_v.wrapping_sub(dst_v);
                // println!("comparing {:02x} with {:02x}",dst_v, src_v);
                self.cpu.r.zero = result == 0;
                self.cpu.r.half = (dst_v & 0x0F) < (src_v & 0x0f);
                self.cpu.r.subn = true;
                self.cpu.r.carry = (dst_v as u16) < (src_v as u16);

                self.set_pc(self.cpu.get_pc() + op.len);
            }
            Inc8(dst) => {
                let a = dst.get_value(self);
                let result = a.wrapping_add(1);
                dst.set_value(self, result);
                self.cpu.r.zero = result == 0;
                self.cpu.r.subn = false;
                self.cpu.r.half = (a & 0x0F) + 1 > 0x0F;
                //carry not modified
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            Dec8(dst) => {
                let a = dst.get_value(self);
                let result = a.wrapping_sub(1);
                dst.set_value(self, result);
                self.cpu.r.zero = result == 0;
                self.cpu.r.subn = true;
                self.cpu.r.half = (a & 0x0F) == 0;
                // carry not modified
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            Math(binop, dst, src) => {
                let b = src.get_value(self);
                let a = dst.get_value(self);
                let (res, sub, half, carry) = match binop {
                    Or  => (a | b, false, false, false),
                    Xor => (a ^ b, false, false, false),
                    And => (a & b, false, true,  false),
                    Add => {
                        let c = 0;
                        let r = a.wrapping_add(b).wrapping_add(c);
                        let half = (a & 0x0F) + (b & 0x0F) > 0xF;
                        let carry = (a as u16) + (b as u16) > 0xFF;
                        (r, false, half, carry)
                    }
                    ADC => {
                        let c = if self.cpu.r.carry { 1 } else { 0 };
                        let r = a.wrapping_add(b).wrapping_add(c);
                        let half = (a & 0x0F) + (b & 0x0F) + c > 0xF;
                        let carry = (a as u16) + (b as u16) + (c as u16) > 0xFF;
                        (r, false, half, carry)
                    }
                    SUB => {
                        let c = 0;
                        let r = a.wrapping_sub(b).wrapping_sub(c);
                        let half = (a & 0x0F) < (b & 0x0F) + c;
                        let carry = (a as u16) < (b as u16) + (c as u16);
                        (r, true, half, carry)
                    }
                    SBC => {
                        let c = if self.cpu.r.carry { 1 } else { 0 };
                        let r = a.wrapping_sub(b).wrapping_sub(c);
                        let half = (a & 0x0F) < (b & 0x0F) + c;
                        let carry = (a as u16) < (b as u16) + (c as u16);
                        (r, true, half, carry)
                    }
                };
                self.cpu.r.zero = res == 0;
                self.cpu.r.subn = sub;
                self.cpu.r.half = half;
                self.cpu.r.carry = carry;
                dst.set_value(self, res);
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            Math16(binop, dst, src) => {
                let b = src.get_value(self);
                let a = dst.get_value(self);
                match binop {
                    Add16 => {
                        // println!("adding {:?},{:?}",dst,src);
                        // println!("values are {:04x} {:04x}",a,b);
                        let r = a.wrapping_add(b);
                        self.cpu.r.subn = false;
                        self.cpu.r.half = (a & 0x07FF) + (b & 0x07FF) > 0x07FF;
                        self.cpu.r.carry = (a) > (0xFFFF - b);
                        // println!("final add value is {:04x}",r);
                        dst.set_value(self, r);
                    }
                };
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            Math16u(unop) => {
                match unop {
                    Inc16(dst) => {
                        let v1 = dst.get_value(self);
                        let v2 = v1.wrapping_add(1);
                        dst.set_value(self, v2);
                        self.set_pc(self.cpu.get_pc() + op.len);
                    }
                    Dec16(dst) => {
                        let v1 = dst.get_value(self);
                        let v2 = v1.wrapping_sub(1);
                        // println!(" ### DEC {:04x}",v2);
                        dst.set_value(self, v2);
                        self.set_pc(self.cpu.get_pc() + op.len);
                    }
                }
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
                        let val2 = set_bit(val, *n, false);
                        r8.set_value(self, val2);
                    }
                    SET(n, r8) => {
                        let val = r8.get_value(self);
                        let val2 = set_bit(val, *n, true);
                        r8.set_value(self, val2);
                    }
                    CPL() => {
                        let val = A.get_value(self);
                        let val2 = !val;
                        A.set_value(self, val2);
                        // Z not affected
                        self.cpu.r.subn = true;
                        self.cpu.r.half = true;
                        //C not affected
                    }
                    BitOps::RLC(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x80 == 0x80;
                        let r = (a << 1) | if c { 1 } else { 0 };
                        r8.set_value(self, r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                        // set_sr_flags(cpu, r,c);
                    }
                    BitOps::RRC(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = (a >> 1) | (if c { 0x80 } else { 0x00 });
                        r8.set_value(self, r);
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
                        r8.set_value(self, r);
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
                        r8.set_value(self, r);
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
                        r8.set_value(self, r);
                        self.cpu.r.zero = r == 0;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                        // set_sr_flags(cpu,r,c);
                    }
                    BitOps::SRA(r8) => {
                        let a = r8.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = (a >> 1) | (a & 0x80);
                        r8.set_value(self, r);
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
                        r8.set_value(self, r);
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
                    RLA() => {
                        let a: u8 = A.get_value(self);
                        let c = a & 0x80 == 0x80;
                        let r = (a << 1) | (if self.cpu.r.carry { 1 } else { 0 });
                        A.set_value(self, r);
                        self.cpu.r.zero = false;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                    }
                    RLCA() => {
                        let a:u8 = A.get_value(self);
                        let c = a & 0x80 == 0x80;
                        let r = (a << 1) | if c { 1 } else { 0 };
                        A.set_value(self, r);
                        self.cpu.r.zero = false;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                        // set_sr_flags(cpu, r,c);
                    }
                    RRA() => {
                        let a: u8 = A.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = (a >> 1) | (if self.cpu.r.carry { 0x80 } else { 0x00 });
                        A.set_value(self, r);
                        self.cpu.r.zero = false;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                    }
                    RRCA() => {
                        let a: u8 = A.get_value(self);
                        let c = a & 0x01 == 0x01;
                        let r = (a >> 1) | (if c { 0x80 } else { 0x00 });
                        A.set_value(self, r);
                        self.cpu.r.zero = false;
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = c;
                    }
                    DAA() => {
                        let mut adj = 0;
                        let mut v: u8 = A.get_value(self);

                        if self.cpu.r.half || (!self.cpu.r.subn && (v & 0xf) > 9) {
                            adj |= 0x6;
                        }

                        let c = if self.cpu.r.carry || (!self.cpu.r.subn && v > 0x99) {
                            adj |= 0x60;
                            true
                        } else {
                            false
                        };

                        let (v,over) = if self.cpu.r.subn { v.overflowing_sub(adj) } else { v.overflowing_add(adj) };
                        let v = (v & 0xff) as u8;

                        self.cpu.r.zero = v == 0;
                        self.cpu.r.carry = c;
                        self.cpu.r.half = false;
                        A.set_value(self, v);
                    }
                    SCF() => {
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = true;
                    }
                    CCF() => {
                        self.cpu.r.subn = false;
                        self.cpu.r.half = false;
                        self.cpu.r.carry = !self.cpu.r.carry;
                    }
                }
                self.set_pc(self.cpu.get_pc() + op.len);
            }
            AddSP(dst, src) => {
                let b = u8_as_i8(src.get_value(self)) as i16 as u16;
                let a = dst.get_value(self);

                let r: u16 = a.wrapping_add(b);
                dst.set_value(self,r);
                self.set_pc(self.cpu.get_pc() + op.len);
                self.cpu.r.zero = false;
                self.cpu.r.subn = false;
                self.cpu.r.half  = ((a & 0x000F) + (b & 0x000F) > 0x000F);
                self.cpu.r.carry = ((a & 0x00FF) + (b & 0x00FF) > 0x00FF);
            }
        }
    }
}

impl Op {
    pub fn to_asm(&self) -> String {
        match &self.typ {
            Noop() => "NOOP".to_string(),
            Jump(typ) => match typ {
                Absolute(src) => format!("JP {}", src.name()),
                RelativeCond(cond, src) => format!("JP {},{}", cond.name(), src.name()),
                Relative(src) => format!("JR i{}", src.name()),
                Restart(n) => format!("RST n{:02x}", n),
                AbsoluteCond(cond, src) => format!("CALL {},{}", cond.name(), src.name()),
            },
            Load16(dst, src) => format!("LD {} {}", dst.name(), src.name()),
            Load8(dst, src) => format!("LD {}, {}", dst.name(), src.name()),
            DisableInterrupts() => "DI".to_string(),
            EnableInterrupts() => "EI".to_string(),
            Halt() => "HALT".to_string(),
            Compare(dst, src) => format!("CP {},{}", dst.name(), src.name()),
            Inc8(dst) => format!("INC {}", dst.name()),
            Dec8(dst) => format!("DEC {}", dst.name()),
            Math(binop, dst, src) => format!("{} {},{}", binop.name(), dst.name(), src.name()),
            Math16(binop, dst, src) => format!("{} {},{}", binop.name(), dst.name(), src.name()),
            Math16u(Inc16(dst)) => format!("INC {}",dst.name()),
            Math16u(Dec16(dst)) => format!("DEC {}",dst.name()),
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
            BitOp(RLA()) => format!("RLA"),
            BitOp(RLCA()) => format!("RLCA"),
            BitOp(RRA()) => format!("RRA"),
            BitOp(RRCA()) => format!("RRCA"),
            BitOp(CPL()) => format!("CPL"),
            BitOp(DAA()) => format!("DDA"),
            BitOp(SCF()) => format!("SCF"),
            BitOp(CCF()) => format!("CCF"),
            Call(ct) => match ct {
                CallU16() => format!("CALL u16"),
                Push(src) => format!("PUSH {}", src.name()),
                Pop(src) => format!("POP {}", src.name()),
                Ret() => format!("RET"),
                CallCondU16(cond) => format!("CALL {},u16", cond.name()),
                RetCond(cond) => format!("RET {}", cond.name()),
                RetI() => format!("RETI"),
            },
            AddSP(dst, src) => format!("{} {}",dst.name(),src.name()),
        }
    }
    pub fn real(&self, gb: &GBState) -> String {
        match &self.typ {
            Noop() => "NOOP".to_string(),
            Jump(typ) => match typ {
                Absolute(src) => format!("JP {:04x}", src.get_addr(gb)),
                RelativeCond(cond, src) => format!("JR {}, {}", cond.real(gb), src.real(gb)),
                Relative(src) => format!("JR +/-{}", src.real(gb)),
                Restart(n) => format!("RST {:02x}", n),
                AbsoluteCond(cond, src) => format!("CALL {}, {}", cond.real(gb), src.real(gb)),
            },
            Load8(dst, src) => format!("LD {} <- {}", dst.real(gb), src.real(gb)),
            Load16(dst, src) => format!("LD {} <- {}", dst.real(gb), src.real(gb)),
            DisableInterrupts() => format!("DI"),
            EnableInterrupts() => format!("EI"),
            Halt() => format!("HALT"),
            Compare(dst, src) => format!("CP {}, {}", dst.real(gb), src.real(gb)),
            Inc8(dst) => format!("INC {}", dst.get_value(gb)),
            Dec8(dst) => format!("DEC {}", dst.get_value(gb)),
            Math(binop, dst, src) => format!("{} {},{}", binop.name(), dst.real(gb), src.real(gb)),
            Math16(binop, dst, src) => {
                format!("{} {},{}", binop.name(), dst.real(gb), src.real(gb))
            }
            Math16u(unop) => {
                match unop {
                    Inc16(dst) => format!("INC {}", dst.get_value(gb)),
                    Dec16(dst) => format!("DEC {}", dst.get_value(gb)),
                }
            }
            BitOp(BIT(n, src)) => format!("BIT {}, {}", n, src.get_value(gb)),
            BitOp(RES(n, src)) => format!("RES {}, {}", n, src.get_value(gb)),
            BitOp(SET(n, src)) => format!("SET {}, {}", n, src.get_value(gb)),
            BitOp(RLC(src)) => format!("RLC {}", src.get_value(gb)),
            BitOp(RRC(src)) => format!("RRC {}", src.get_value(gb)),
            BitOp(RL(src)) => format!("RL {}", src.get_value(gb)),
            BitOp(RR(src)) => format!("RR {}", src.get_value(gb)),
            BitOp(SLA(src)) => format!("SLA  {}", src.get_value(gb)),
            BitOp(SRA(src)) => format!("SRA  {}", src.get_value(gb)),
            BitOp(SRL(src)) => format!("SRL  {}", src.get_value(gb)),
            BitOp(SWAP(src)) => format!("SWAP {}", src.get_value(gb)),
            BitOp(RLA()) => format!("RLA {}", A.get_value(gb)),
            BitOp(RLCA()) => format!("RLCA {}", A.get_value(gb)),
            BitOp(RRA()) => format!("RRA {}", A.get_value(gb)),
            BitOp(RRCA()) => format!("RRCA {}", A.get_value(gb)),
            BitOp(CPL()) => format!("CPL {}", A.get_value(gb)),
            BitOp(DAA()) => format!("DDA {}", A.get_value(gb)),
            BitOp(SCF()) => format!("SCF {}", A.get_value(gb)),
            BitOp(CCF()) => format!("CCF {}", A.get_value(gb)),
            Call(ct) => match ct {
                CallU16() => format!("CALL {}", gb.mmu.read16(gb.cpu.get_pc())),
                CallCondU16(cond) => {
                    format!("CALL {}, {}", cond.real(gb), gb.mmu.read16(gb.cpu.get_pc()))
                }
                Push(src) => format!("PUSH {}", src.get_value(gb)),
                Pop(src) => format!("POP {}", src.get_value(gb)),
                Ret() => format!("RET  back to {:04X}", gb.mmu.read16(gb.cpu.get_sp())),
                RetCond(cond) => format!(
                    "RET Z: if {} then back to {:04x}",
                    cond.get_value(gb),
                    gb.mmu.read16(gb.cpu.get_sp())
                ),
                RetI() => format!("RET back to {:04X}", gb.mmu.read16(gb.cpu.get_sp())),
            },
            AddSP(dst, src) => format!("ADD {}, {}", dst.get_value(gb), src.get_value(gb)),
        }
    }
}

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

struct OpTable {
    ops: HashMap<u16, Op>,
}

impl OpTable {
    fn new() -> OpTable {
        OpTable {
            ops: Default::default(),
        }
    }
    fn inserto(&mut self, code: u16, op: Op) {
        if self.ops.contains_key(&code) {
            println!("ERROR: table already contains {:04x}", code);
            panic!("ERROR table already contains opcode");
        }
        self.ops.insert(code, op);
    }
    fn add(&mut self, op: Op) {
        self.inserto(op.code, op);
    }
    pub(crate) fn add_op(&mut self, code: u16, len: u16, cycles: u16, typ: OpType) {
        self.inserto(code,Op{ code, len, cycles, typ });
    }

    fn load8(&mut self, code: u16, dst: Dst8, src: Src8) {
        let (mut len, mut cycles) = match src {
            SrcR8(_) => (1, 4),
            Mem(_) => (1, 8),
            Im8() => (2, 8),
            Src8::HiMemIm8() => (2, 12),
            Src8::MemIm16() => (3, 16),
            Src8::MemWithInc(_) => (1, 8),
            Src8::MemWithDec(_) => (1, 8),
            Src8::HiMemR8(_) => (1,8),
        };
        match dst {
            DstR8(_) => {}
            AddrDst(_) => cycles += 4,
            Dst8::HiMemIm8() => len += 1,
            Dst8::HiMemR8(_) => {}
            Dst8::MemIm16() => len += 2,
            Dst8::MemWithInc(_) => {}
            Dst8::MemWithDec(_) => {}
        }
        self.add(Op {
            code,
            len,
            cycles,
            typ: Load8(dst, src),
        });
    }
    fn load16(&mut self, code: u16, dst: Dst16, src: Src16) {
        let (len, cycles) = match src {
            Src16::Im16() => (3, 12),
            SrcR16(r) => (1, 8),
            Src16::SrcR16WithOffset(r) => (2,12),
        };
        self.add(Op {
            code,
            len,
            cycles,
            typ: Load16(dst, src),
        });
    }
    fn lookup(&self, code: &u16) -> Option<&Op> {
        self.ops.get(code)
    }
    fn bitop(&mut self, code: u16, bop: BitOps) {
        self.add(Op {
            code,
            len: 2,
            cycles: 8,
            typ: BitOp(bop),
        });
    }
    fn math(&mut self, code: u16, binop: BinOp, dst8: Dst8, src8: Src8) {
        self.add(Op {
            code,
            len: 1,
            cycles: 4,
            typ: Math(binop,dst8,src8),
        });
    }
}

fn make_op_table() -> OpTable {
    let mut op_table = OpTable::new(); //:HashMap<u16,Op> = HashMap::new();
    op_table.add_op(0x00,1,4,Noop());

    op_table.load8(0x02, AddrDst(BC), SrcR8(A)); // LD L,A
    op_table.load8(0x06, DstR8(B), Im8()); // LD B,n
    op_table.load8(0x0E, DstR8(C), Im8()); // LD C,n
    op_table.load8(0x0A, DstR8(A), Mem(BC));
    op_table.load8(0x1A, DstR8(A), Mem(DE));

    op_table.load8(0x40, DstR8(B), SrcR8(B));
    op_table.load8(0x41, DstR8(B), SrcR8(C));
    op_table.load8(0x42, DstR8(B), SrcR8(D));
    op_table.load8(0x43, DstR8(B), SrcR8(E));
    op_table.load8(0x44, DstR8(B), SrcR8(H));
    op_table.load8(0x45, DstR8(B), SrcR8(L));
    op_table.load8(0x46, DstR8(B), Mem(HL));
    op_table.load8(0x47, DstR8(B), SrcR8(A));
    op_table.load8(0x48, DstR8(C), SrcR8(B));
    op_table.load8(0x49, DstR8(C), SrcR8(C));
    op_table.load8(0x4A, DstR8(C), SrcR8(D));
    op_table.load8(0x4B, DstR8(C), SrcR8(E));
    op_table.load8(0x4C, DstR8(C), SrcR8(H));
    op_table.load8(0x4D, DstR8(C), SrcR8(L));
    op_table.load8(0x4E, DstR8(C), Mem(HL));
    op_table.load8(0x4F, DstR8(C), SrcR8(A));

    op_table.load8(0x50, DstR8(D), SrcR8(B));
    op_table.load8(0x51, DstR8(D), SrcR8(C));
    op_table.load8(0x52, DstR8(D), SrcR8(D));
    op_table.load8(0x53, DstR8(D), SrcR8(E));
    op_table.load8(0x54, DstR8(D), SrcR8(H));
    op_table.load8(0x55, DstR8(D), SrcR8(L));
    op_table.load8(0x56, DstR8(D), Mem(HL));
    op_table.load8(0x57, DstR8(D), SrcR8(A));
    op_table.load8(0x58, DstR8(E), SrcR8(B));
    op_table.load8(0x59, DstR8(E), SrcR8(C));
    op_table.load8(0x5A, DstR8(E), SrcR8(D));
    op_table.load8(0x5B, DstR8(E), SrcR8(E));
    op_table.load8(0x5C, DstR8(E), SrcR8(H));
    op_table.load8(0x5D, DstR8(E), SrcR8(L));
    op_table.load8(0x5E, DstR8(E), Mem(HL));
    op_table.load8(0x5F, DstR8(E), SrcR8(A));

    op_table.load8(0x60, DstR8(H), SrcR8(B));
    op_table.load8(0x61, DstR8(H), SrcR8(C));
    op_table.load8(0x62, DstR8(H), SrcR8(D));
    op_table.load8(0x63, DstR8(H), SrcR8(E));
    op_table.load8(0x64, DstR8(H), SrcR8(H));
    op_table.load8(0x65, DstR8(H), SrcR8(L));
    op_table.load8(0x66, DstR8(H), Mem(HL));
    op_table.load8(0x67, DstR8(H), SrcR8(A));
    op_table.load8(0x68, DstR8(L), SrcR8(B));
    op_table.load8(0x69, DstR8(L), SrcR8(C));
    op_table.load8(0x6A, DstR8(L), SrcR8(D));
    op_table.load8(0x6B, DstR8(L), SrcR8(E));
    op_table.load8(0x6C, DstR8(L), SrcR8(H));
    op_table.load8(0x6D, DstR8(L), SrcR8(L));
    op_table.load8(0x6E, DstR8(L), Mem(HL));
    op_table.load8(0x6F, DstR8(L), SrcR8(A));

    op_table.load8(0x70, AddrDst(HL), SrcR8(B));
    op_table.load8(0x71, AddrDst(HL), SrcR8(C));
    op_table.load8(0x72, AddrDst(HL), SrcR8(D));
    op_table.load8(0x73, AddrDst(HL), SrcR8(E));
    op_table.load8(0x74, AddrDst(HL), SrcR8(H));
    op_table.load8(0x75, AddrDst(HL), SrcR8(L));
    //76
    op_table.load8(0x77, AddrDst(HL), SrcR8(A)); // LD L,A
    op_table.load8(0x78, DstR8(A), SrcR8(B));
    op_table.load8(0x79, DstR8(A), SrcR8(C));
    op_table.load8(0x7A, DstR8(A), SrcR8(D));
    op_table.load8(0x7B, DstR8(A), SrcR8(E));
    op_table.load8(0x7C, DstR8(A), SrcR8(H));
    op_table.load8(0x7D, DstR8(A), SrcR8(L));
    op_table.load8(0x7E, DstR8(A), Mem(HL));
    op_table.load8(0x7F, DstR8(A), SrcR8(A));

    op_table.load16(0x01, DstR16(BC), Src16::Im16()); // LD DE, nn
    op_table.load16(0x11, DstR16(DE), Src16::Im16()); // LD DE, nn
    op_table.load16(0x21, DstR16(HL), Src16::Im16()); // LD HL, nn
    op_table.load16(0x31, DstR16(SP), Src16::Im16()); // LD HL, nn

    op_table.load8(0x16, DstR8(D), Im8()); // LD D,n
    op_table.load8(0x1E, DstR8(E), Im8()); // LD E,n
    op_table.load8(0x26, DstR8(H), Im8()); // LD H,n
    op_table.load8(0x2E, DstR8(L), Im8()); // LD L,n
                                           // 0x36 => Some(LoadInstr(Load_addr_R2_u8(HL))),
    op_table.load8(0x36, AddrDst(HL), Im8()); // LD (HL),u8
    op_table.add_op(0x08, 3,20,Load16(Dst16::MemIm16(),Src16::SrcR16(SP)));
    op_table.add_op(0xF8, 3,12,Load16(Dst16::DstR16(HL),Src16::SrcR16WithOffset(SP)));
    op_table.load16(0xF9, DstR16(SP), Src16::SrcR16(HL)); // LD HL, nn

    op_table.load8(0x12, AddrDst(DE), SrcR8(A)); // LD L,A
    op_table.load8(0xEA, Dst8::MemIm16(), SrcR8(A)); // LD L,A
    op_table.load8(0xF0, DstR8(A), Src8::HiMemIm8());
    op_table.load8(0xF2, DstR8(A), Src8::HiMemR8(C));
    op_table.load8(0xFA, DstR8(A), Src8::MemIm16());
    op_table.load8(0xE0, Dst8::HiMemIm8(), SrcR8(A));
    op_table.load8(0xE2, Dst8::HiMemR8(C), SrcR8(A));
    op_table.load8(0x3E, DstR8(A), Im8());
    op_table.load8(0x2A, DstR8(A), Src8::MemWithInc(HL));
    op_table.load8(0x3A, DstR8(A), Src8::MemWithDec(HL));
    op_table.load8(0x22, Dst8::MemWithInc(HL), SrcR8(A));
    op_table.load8(0x32, Dst8::MemWithDec(HL), SrcR8(A));

    op_table.add_op(0xF3,1,4,DisableInterrupts());
    op_table.add_op(0xFB,1,4,EnableInterrupts());
    op_table.add_op(0x76,1,16, Halt());

    op_table.add_op(0x03,1,8,Math16u(Inc16(BC)));
    op_table.add_op(0x13,1,8,Math16u(Inc16(DE)));
    op_table.add_op(0x23,1,8,Math16u(Inc16(HL)));
    op_table.add_op(0x33,1,8,Math16u(Inc16(SP)));

    op_table.add_op(0x0B,1,8,Math16u(Dec16(BC)));
    op_table.add_op(0x1B,1,8,Math16u(Dec16(DE)));
    op_table.add_op(0x2B,1,8,Math16u(Dec16(HL)));
    op_table.add_op(0x3B,1,8,Math16u(Dec16(SP)));

    op_table.add_op(0x04,1,4,Inc8(DstR8(B)));
    op_table.add_op(0x05,1,4,Dec8(DstR8(B)));
    op_table.add_op(0x0C,1,4,Inc8(DstR8(C)));
    op_table.add_op(0x0D,1,4,Dec8(DstR8(C)));

    op_table.add_op(0x14,1,4,Inc8(DstR8(D)));
    op_table.add_op(0x15,1,4,Dec8(DstR8(D)));
    op_table.add_op(0x1C,1,4,Inc8(DstR8(E)));
    op_table.add_op(0x1D,1,4,Dec8(DstR8(E)));

    op_table.add_op(0x24,1,4,Inc8(DstR8(H)));
    op_table.add_op(0x25,1,4,Dec8(DstR8(H)));
    op_table.add_op(0x2C,1,4,Inc8(DstR8(L)));
    op_table.add_op(0x2D,1,4,Dec8(DstR8(L)));

    op_table.add_op(0x34,1,12,Inc8(AddrDst(HL)));
    op_table.add_op(0x35,1,12,Dec8(AddrDst(HL)));
    op_table.add_op(0x3C,1,4,Inc8(DstR8(A)));
    op_table.add_op(0x3D,1,4,Dec8(DstR8(A)));

    op_table.math(0x80,Add, DstR8(A), SrcR8(B));
    op_table.math(0x81,Add, DstR8(A), SrcR8(C));
    op_table.math(0x82,Add, DstR8(A), SrcR8(D));
    op_table.math(0x83,Add, DstR8(A), SrcR8(E));
    op_table.math(0x84,Add, DstR8(A), SrcR8(H));
    op_table.math(0x85,Add, DstR8(A), SrcR8(L));
    op_table.add_op(0x86,1,8,Math(Add,DstR8(A), Mem(HL)));
    op_table.math(0x87,Add, DstR8(A), SrcR8(A));
    op_table.math(0x88,ADC, DstR8(A), SrcR8(B));
    op_table.math(0x89,ADC, DstR8(A), SrcR8(C));
    op_table.math(0x8A,ADC, DstR8(A), SrcR8(D));
    op_table.math(0x8B,ADC, DstR8(A), SrcR8(E));
    op_table.math(0x8C,ADC, DstR8(A), SrcR8(H));
    op_table.math(0x8D,ADC, DstR8(A), SrcR8(L));
    op_table.add_op(0x8E,1,8,Math(ADC, DstR8(A), Mem(HL)));
    op_table.math(0x8F,ADC, DstR8(A), SrcR8(A));


    op_table.math(0x90,SUB, DstR8(A), SrcR8(B));
    op_table.math(0x91,SUB, DstR8(A), SrcR8(C));
    op_table.math(0x92,SUB, DstR8(A), SrcR8(D));
    op_table.math(0x93,SUB, DstR8(A), SrcR8(E));
    op_table.math(0x94,SUB, DstR8(A), SrcR8(H));
    op_table.math(0x95,SUB, DstR8(A), SrcR8(L));
    op_table.add_op(0x96, 1, 8,Math(SUB, DstR8(A), Mem(HL)));
    op_table.math(0x97,SUB, DstR8(A), SrcR8(A));

    op_table.math(0x98,SBC, DstR8(A), SrcR8(B));
    op_table.math(0x99,SBC, DstR8(A), SrcR8(C));
    op_table.math(0x9A,SBC, DstR8(A), SrcR8(D));
    op_table.math(0x9B,SBC, DstR8(A), SrcR8(E));
    op_table.math(0x9C,SBC, DstR8(A), SrcR8(H));
    op_table.math(0x9D,SBC, DstR8(A), SrcR8(L));
    op_table.add_op(0x9E,1,8,Math(SBC, DstR8(A), Mem(HL)));
    op_table.math(0x9F,SBC, DstR8(A), SrcR8(A));

    op_table.math(0xA0,And, DstR8(A), SrcR8(B));
    op_table.math(0xA1,And, DstR8(A), SrcR8(C));
    op_table.math(0xA2,And, DstR8(A), SrcR8(D));
    op_table.math(0xA3,And, DstR8(A), SrcR8(E));
    op_table.math(0xA4,And, DstR8(A), SrcR8(H));
    op_table.math(0xA5,And, DstR8(A), SrcR8(L));
    op_table.add_op(0xA6,1,8,Math(And, DstR8(A), Mem(HL)));
    op_table.math(0xA7,And, DstR8(A), SrcR8(A));

    op_table.math(0xA8,Xor, DstR8(A), SrcR8(B));
    op_table.math(0xA9,Xor, DstR8(A), SrcR8(C));
    op_table.math(0xAA,Xor, DstR8(A), SrcR8(D));
    op_table.math(0xAB,Xor, DstR8(A), SrcR8(E));
    op_table.math(0xAC,Xor, DstR8(A), SrcR8(H));
    op_table.math(0xAD,Xor, DstR8(A), SrcR8(L));
    op_table.add_op(0xAE,1,8,Math(Xor, DstR8(A), Mem(HL)));
    op_table.math(0xAF,Xor, DstR8(A), SrcR8(A));

    op_table.math(0xB0, Or, DstR8(A), SrcR8(B));
    op_table.math(0xB1, Or, DstR8(A), SrcR8(C));
    op_table.math(0xB2, Or, DstR8(A), SrcR8(D));
    op_table.math(0xB3, Or, DstR8(A), SrcR8(E));
    op_table.math(0xB4, Or, DstR8(A), SrcR8(H));
    op_table.math(0xB5, Or, DstR8(A), SrcR8(L));
    op_table.add_op(0xB6,1,8,Math(Or, DstR8(A), Mem(HL)));
    op_table.math(0xB7,Or, DstR8(A), SrcR8(A));
    op_table.add_op(0xB8,1,4,Compare(DstR8(A), SrcR8(B)));
    op_table.add_op(0xB9,1,4,Compare(DstR8(A), SrcR8(C)));
    op_table.add_op(0xBA,1,4,Compare(DstR8(A), SrcR8(D)));
    op_table.add_op(0xBB,1,4,Compare(DstR8(A), SrcR8(E)));
    op_table.add_op(0xBC,1,4,Compare(DstR8(A), SrcR8(H)));
    op_table.add_op(0xBD,1,4,Compare(DstR8(A), SrcR8(L)));
    op_table.add_op(0xBE,1,8,Compare(DstR8(A), Mem(HL)));
    op_table.add_op(0xBF,1,4,Compare(DstR8(A), SrcR8(A)));

    op_table.add_op(0x09,1,8,Math16(Add16, DstR16(HL), SrcR16(BC)));
    op_table.add_op(0x19,1,8,Math16(Add16, DstR16(HL), SrcR16(DE)));
    op_table.add_op(0x29,1,8,Math16(Add16, DstR16(HL), SrcR16(HL)));
    op_table.add_op(0x39,1,8,Math16(Add16, DstR16(HL), SrcR16(SP)));

    op_table.add_op(0xC6,2,8,Math(Add, DstR8(A), Im8()));
    op_table.add_op(0xD6,2,8,Math(SUB, DstR8(A), Im8()));

    op_table.add_op(0xE6,2,8,Math(And, DstR8(A), Im8()));
    op_table.add_op(0xE8,2,16,AddSP(DstR16(SP), Im8()));
    op_table.add_op(0xF6,2,8,Math(Or,  DstR8(A), Im8()));

    op_table.add_op(0xCE,2,8,Math(ADC, DstR8(A), Im8()));
    op_table.add_op(0xDE,2,8,Math(SBC, DstR8(A), Im8()));
    op_table.add_op(0xEE,2,8,Math(Xor, DstR8(A), Im8()));
    op_table.add_op(0xFE,2,8,Compare(DstR8(A), Im8()));


    op_table.add_op(0xC2,3,12,Jump(AbsoluteCond(NotZero(), AddrSrc::Imu16())));
    op_table.add_op(0xD2,3,12,Jump(AbsoluteCond(NotCarry(), AddrSrc::Imu16())));
    op_table.add_op(0xC3,3,12,Jump(Absolute(AddrSrc::Imu16())));
    op_table.add_op(0x20,2,12,Jump(RelativeCond(NotZero(), Im8())));
    op_table.add_op(0x30,2,12,Jump(RelativeCond(NotCarry(), Im8())));
    op_table.add_op(0x18,2,12,Jump(Relative(Im8())));
    op_table.add_op(0x28,2,12,Jump(RelativeCond(Zero(), Im8())));
    op_table.add_op(0x38,2,8, Jump(RelativeCond(Carry(), Im8())));
    op_table.add_op(0xE9,1,4, Jump(Absolute(AddrSrc::Src16(HL))));
    op_table.add_op(0xCA,3,12,Jump(AbsoluteCond(Zero(), AddrSrc::Imu16())));
    op_table.add_op(0xDA,3,12,Jump(AbsoluteCond(Carry(), AddrSrc::Imu16())));

    op_table.add_op(0x07,1,4,BitOp(RLCA()));
    op_table.add_op(0x0F,1,4,BitOp(RRCA()));
    op_table.add_op(0x17,1,4,BitOp(RLA()));
    op_table.add_op(0x1F,1,4,BitOp(RRA()));
    op_table.add_op(0x27,1,4,BitOp(DAA()));
    op_table.add_op(0x2F,1,4,BitOp(CPL()));
    op_table.add_op(0x37,1,4,BitOp(SCF()));
    op_table.add_op(0x3F,1,4,BitOp(CCF()));

    // almost the entire lower CB chart
    let r8list = [B, C, D, E, H, L];
    for (i, r8) in r8list.iter().enumerate() {
        let col = (i as u16);
        op_table.bitop(0xCB_00 + col, RLC(DstR8(*r8)));
        op_table.bitop(0xCB_08 + col, RRC(DstR8(*r8)));
        op_table.bitop(0xCB_10 + col, RL(DstR8(*r8)));
        op_table.bitop(0xCB_18 + col, RR(DstR8(*r8)));
        op_table.bitop(0xCB_20 + col, SLA(DstR8(*r8)));
        op_table.bitop(0xCB_28 + col, SRA(DstR8(*r8)));
        op_table.bitop(0xCB_30 + col, SWAP(DstR8(*r8)));
        op_table.bitop(0xCB_38 + col, SRL(DstR8(*r8)));
        for j in 0..8 {
            op_table.bitop(0xCB_40 + col + (j*8), BIT(j as u8, DstR8(*r8)));
            op_table.bitop(0xCB_80 + col + (j*8), RES(j as u8, DstR8(*r8)));
            op_table.bitop(0xCB_C0 + col + (j*8), SET(j as u8, DstR8(*r8)));
        }
    }

    op_table.bitop(0xCB_06,RLC(AddrDst(HL)));
    op_table.bitop(0xCB_07,RLC(DstR8(A)));
    op_table.bitop(0xCB_16,RL(AddrDst(HL)));
    op_table.bitop(0xCB_17,RL(DstR8(A)));
    op_table.bitop(0xCB_26,SLA(AddrDst(HL)));
    op_table.bitop(0xCB_27,SLA(DstR8(A)));
    op_table.bitop(0xCB_36,SWAP(AddrDst(HL)));
    op_table.bitop(0xCB_37,SWAP(DstR8(A)));
    for j in 0..8 {
        op_table.add_op(0xCB_46 + (j*8),2,12,BitOp(BIT(j as u8, AddrDst(HL))));
        op_table.add_op(0xCB_86 + (j*8),2,12,BitOp(RES(j as u8, AddrDst(HL))));
        op_table.add_op(0xCB_C6 + (j*8),2,12,BitOp(SET(j as u8, AddrDst(HL))));
        op_table.bitop( 0xCB_47 + (j*8), BIT(j as u8, DstR8(A)));
        op_table.bitop( 0xCB_87 + (j*8), RES(j as u8, DstR8(A)));
        op_table.bitop( 0xCB_C7 + (j*8), SET(j as u8, DstR8(A)));
    }

    op_table.bitop( 0xCB_0E, RRC(AddrDst(HL)));
    op_table.bitop( 0xCB_0F, RRC(DstR8(A)));
    op_table.bitop( 0xCB_1E, RR(AddrDst(HL)));
    op_table.bitop( 0xCB_1F, RR(DstR8(A)));
    op_table.bitop( 0xCB_2E, SRA(AddrDst(HL)));
    op_table.bitop( 0xCB_2F, SRA(DstR8(A)));
    op_table.bitop( 0xCB_3E, SRL(AddrDst(HL)));
    op_table.bitop( 0xCB_3F, SRL(DstR8(A)));

    op_table.add_op(0x00CD, 3, 24, Call(CallU16()));

    op_table.add_op(0x00C4, 3, 24, Call(CallCondU16(NotZero())));
    op_table.add_op(0x00D4, 3, 24, Call(CallCondU16(NotCarry())));
    op_table.add_op(0x00CC, 3, 24, Call(CallCondU16(Zero())));
    op_table.add_op(0x00DC, 3, 24, Call(CallCondU16(Carry())));

    op_table.add_op(0x00C0, 1, 20, Call(RetCond(NotZero())));
    op_table.add_op(0x00C8, 1, 20, Call(RetCond(Zero())));
    op_table.add_op(0x00D0, 1, 20, Call(RetCond(NotCarry())));
    op_table.add_op(0x00D8, 1, 20, Call(RetCond(Carry())));

    op_table.add_op(0x00C9, 1, 16, Call(Ret()));
    op_table.add_op(0x00D9, 1, 16, Call(RetI()));


    op_table.add_op(0x00C7, 1, 16, Jump(Restart(0x00)));
    op_table.add_op(0x00CF, 1, 16, Jump(Restart(0x08)));
    op_table.add_op(0x00D7, 1, 16, Jump(Restart(0x10)));
    op_table.add_op(0x00DF, 1, 16, Jump(Restart(0x18)));
    op_table.add_op(0x00E7, 1, 16, Jump(Restart(0x20)));
    op_table.add_op(0x00EF, 1, 16, Jump(Restart(0x28)));
    op_table.add_op(0x00F7, 1, 16, Jump(Restart(0x30)));
    op_table.add_op(0x00FF, 1, 16, Jump(Restart(0x38)));

    op_table.add_op(0x00C1,1,12,Call(Pop(BC)));
    op_table.add_op(0x00D1,1,12,Call(Pop(DE)));
    op_table.add_op(0x00E1,1,12,Call(Pop(HL)));
    op_table.add_op(0x00F1,1,16,Call(Pop(AF)));

    op_table.add_op(0x00C5,1,16,Call(Push(BC)));
    op_table.add_op(0x00D5,1,16,Call(Push(DE)));
    op_table.add_op(0x00E5,1,16,Call(Push(HL)));
    op_table.add_op(0x00F5,1,16,Call(Push(AF)));


    op_table
}

pub fn setup_test_rom(fname: &str) -> Option<GBState> {
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
        setup_test_rom("./resources/testroms/cpu_instrs/individual/10-bit ops.gb").unwrap();

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
fn op_tests() {
    let mut gb = GBState {
        cpu: CPU::init(),
        mmu: MMU2::init_empty(0xDE),
        ppu: PPU2::init(),
        ops: make_op_table(),
        clock: 0,
        count: 0,
        debug: false,
    };
    let op_table = make_op_table();
    if let Some(op) = op_table.lookup(&0x36) {
        println!("op is {:?}", op);
        assert_eq!(op.len, 2);
        assert_eq!(op.cycles, 12);

        let addr = 0x8000;
        let payload = 0x99;
        let pc = 0x200;

        //set pc to x200
        gb.set_pc(pc);

        //put x99 as the i8
        gb.mmu.write8(pc + 0, 0x36);
        gb.mmu.write8(pc + 1, payload);

        //set HL to x8000
        gb.cpu.set_r16(CPUR16::HL, addr);

        //confirm x8000 has xDE in it
        assert_eq!(gb.mmu.read8(addr), 0xDE);

        //execute instruction
        gb.execute();

        //confirm x8000 now has x99 in it
        assert_eq!(gb.mmu.read8(addr), payload);

        //confirm PC is now x202
        assert_eq!(gb.cpu.get_pc(), pc + 2);
        println!(
            "HL loaded to 0x8000 {:04x} {:02x}",
            addr,
            gb.mmu.read8(addr)
        );
    } else {
        gb.dump_current_state();
        panic!("op not found");
    }
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

#[test]
fn test_op_08() {
    let rom = [0x08, 0x42, 0x00]; // LD D, 0x05
    let mut gb: GBState = GBState::make_test_context(&rom.to_vec());
    gb.set_pc(0);
    gb.cpu.real_set_sp(0x66);
    assert_eq!(gb.mmu.read8(0x42),0xD3);
    gb.execute();
    assert_eq!(gb.mmu.read8(0x42),0x66);

}

#[test]
fn test_op_f8() {
    let rom: [u8; 2] = [0xF8, 0x02]; // LD D, 0x05
    let mut gb: GBState = GBState::make_test_context(&rom.to_vec());
    gb.set_pc(0);
    gb.cpu.real_set_sp(0x40);
    assert_eq!(gb.cpu.get_r16(CPUR16::HL),0);
    gb.execute();
    assert_eq!(gb.cpu.get_r16(CPUR16::HL),0x42);

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
