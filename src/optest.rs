use std::collections::HashMap;
use std::fmt::format;
use std::fs;
use std::path::{Path, PathBuf};
use BinOp::{And, Or, Xor};
use BitOps::Bit;
use Cond::{Carry, NotCarry, NotZero, Zero};
use Dst16::DstR16;
use Dst8::AddrDst;
use JumpType::Absolute;
use OpType::{BitOp, Dec16, Dec8, Inc16, Inc8, Jump, Load16, Math};
use Src16::Im16;
// use Op::Add8;
use crate::{load_romfile, MMU, Z80};
use crate::common::get_bit_as_bool;
use crate::mmu::{BGP, LCDC_LCDCONTROL, NR52_SOUND};
use crate::opcodes::{DoubleRegister, RegisterName, u8_as_i8};
use crate::optest::AddrSrc::Imu16;
use crate::optest::Dst8::DstR8;
use crate::optest::JumpType::{Relative, RelativeCond};
use crate::optest::OpType::Load8;
use crate::optest::R8::{A, B, C, D, E, H, L};
use crate::optest::R16::{BC, DE, HL, SP};
use crate::optest::Src8::{HiMemIm8, Im8, Mem, SrcR8};

#[derive(Debug)]
pub enum R8  { A, B, C, D, E, H, L,}
#[derive(Debug)]
pub enum R16 { BC, HL, DE, SP,}
#[derive(Debug)]
pub enum Src8  {  SrcR8(R8), Mem(R16), MemWithInc(R16), MemWithDec(R16), Im8(), HiMemIm8(), MemIm16() }
#[derive(Debug)]
pub enum Src16 {  Im16(),   }
#[derive(Debug)]
pub enum Dst8  {  DstR8(R8), AddrDst(R16), HiMemIm8(), HiMemR8(R8), MemIm16(), MemWithInc(R16), MemWithDec(R16)  }
#[derive(Debug)]
pub enum Dst16 {  DstR16(R16), }

#[derive(Debug)]
enum BinOp {
    Or,
    Xor,
    And,
}

#[derive(Debug)]
enum BitOps {
    Bit(u8, R8)
}

#[derive(Debug)]
enum OpType {
    Noop(),
    Jump(JumpType),
    Load16(Dst16, Src16),
    Load8(Dst8, Src8),
    DisableInterrupts(),
    Compare(Dst8, Src8),
    // Xor(Dst8, Src8),
    // Or(Dst8, Src8),
    Math(BinOp, Dst8, Src8),
    // And(Dst8, Src8),
    Inc16(R16),
    Dec16(R16),
    Inc8(R8),
    Dec8(R8),
    BitOp(BitOps),
}


struct Op {
    code:u16,
    len:u16,
    cycles:u16,
    typ:OpType,
}


impl R8 {
    fn get_value(&self, cpu: &Z80) -> u8 {
        match self {
            R8::A => cpu.r.a,
            R8::B => cpu.r.b,
            R8::C => cpu.r.c,
            R8::D => cpu.r.d,
            R8::E => cpu.r.e,
            R8::H => cpu.r.h,
            R8::L => cpu.r.l,
        }
    }
    fn set_value(&self, cpu: &mut Z80, value:u8) {
        match self {
            R8::A => cpu.r.set_u8reg(&RegisterName::A, value),
            R8::B => cpu.r.set_u8reg(&RegisterName::B, value),
            R8::C => cpu.r.set_u8reg(&RegisterName::C, value),
            R8::D => cpu.r.set_u8reg(&RegisterName::D, value),
            R8::E => cpu.r.set_u8reg(&RegisterName::E, value),
            R8::H => cpu.r.set_u8reg(&RegisterName::H, value),
            R8::L => cpu.r.set_u8reg(&RegisterName::L, value),
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
    fn get_value(&self, cpu: &Z80) -> u16 {
        match self {
            BC => cpu.r.get_u16reg(&DoubleRegister::BC),
            HL => cpu.r.get_u16reg(&DoubleRegister::HL),
            DE => cpu.r.get_u16reg(&DoubleRegister::DE),
            SP => cpu.r.get_u16reg(&DoubleRegister::SP),
        }
    }
    fn set_value(&self, cpu: &mut Z80, val:u16) {
        match self {
            BC => cpu.r.set_u16reg(&DoubleRegister::BC,val),
            HL => cpu.r.set_u16reg(&DoubleRegister::HL,val),
            DE => cpu.r.set_u16reg(&DoubleRegister::DE,val),
            SP => cpu.r.set_u16reg(&DoubleRegister::SP,val),
        }
    }
    fn name(&self) -> &'static str{
        match self {
            BC => "BC",
            HL => "HL",
            DE => "DE",
            SP => "SP",
        }
    }
}

#[derive(Debug)]
enum AddrSrc {
    Imu16(),
}

impl AddrSrc {
    fn name(&self) -> String {
        match self {
            AddrSrc::Imu16() => "u16".to_string(),
        }
    }
    fn real(&self, gb:&GBState) -> String {
        match self {
            AddrSrc::Imu16() => format!("{:04x}",gb.mmu.read16(gb.cpu.get_pc()+1)),
        }
    }
    pub(crate) fn get_addr(&self, gb:&mut GBState) -> u16 {
        gb.mmu.read16(gb.cpu.get_pc()+1)
    }
}

impl Src16 {
    fn name(&self) -> String {
        match self {
            Im16() => "nn".to_string(),
        }
    }
    fn get_value(&self, gb:&GBState) -> u16 {
        gb.mmu.read16(gb.cpu.get_pc()+1)
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            Im16() => format!("{:04x}", gb.mmu.read16(gb.cpu.get_pc()+1))
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
            DstR16(r2) => r2.set_value(&mut gb.cpu, val)
        }
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            DstR16(rr) => format!("{:04x}",rr.get_value(&gb.cpu)),
        }
    }
    fn get_value(&self, gb:&GBState) -> u16 {
        match self {
            DstR16(rr) => rr.get_value(&gb.cpu),
        }
    }
}
#[derive(Debug)]
pub enum Cond {
    Carry(),
    NotCarry(),
    Zero(),
    NotZero(),
}

impl Cond {
    pub(crate) fn get_value(&self, cpu: &Z80) -> bool {
        match self {
            Carry() => cpu.r.carry_flag,
            NotCarry() => !cpu.r.carry_flag,
            Zero() => cpu.r.zero_flag,
            NotZero() => !cpu.r.zero_flag,
        }
    }
}


#[derive(Debug)]
enum JumpType {
    Absolute(AddrSrc),
    RelativeCond(Cond,Src8),
    Relative(Src8),
}

impl BinOp {
    fn name(&self) -> &str {
        match self {
            Or => "OR",
            Xor => "XOR",
            And => "AND",
        }
    }
}
impl Cond {
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
            Carry() => gb.cpu.r.carry_flag,
            NotCarry() => !gb.cpu.r.carry_flag,
            Zero() => gb.cpu.r.zero_flag,
            NotZero() => !gb.cpu.r.zero_flag,
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
            SrcR8(r1) => r1.get_value(&gb.cpu),
            Mem(r2) => {
                // println!("reading memory at location {:04x}",r2.get_value(&gb.cpu));
                gb.mmu.read8(r2.get_value(&gb.cpu))
            },
            Src8::MemWithInc(r2) => gb.mmu.read8(r2.get_value(&gb.cpu)),
            Src8::MemWithDec(r2) => gb.mmu.read8(r2.get_value(&gb.cpu)),
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
            DstR8(r8) => r8.set_value(&mut gb.cpu, val),
            AddrDst(r16) => gb.mmu.write8(r16.get_value(&gb.cpu), val),
            Dst8::MemIm16() => gb.mmu.write8(gb.mmu.read16(gb.cpu.get_pc()+1),val),
            Dst8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc()+1);
                gb.mmu.write8(0xFF00 + im as u16,val)
            }
            Dst8::HiMemR8(r8) => {
                let addr = 0xFF00 + (r8.get_value(&gb.cpu) as u16);
                gb.mmu.write8(addr,val)
            }
            Dst8::MemWithInc(r16) => gb.mmu.write8(r16.get_value(&gb.cpu),val),
            Dst8::MemWithDec(r16) => gb.mmu.write8(r16.get_value(&gb.cpu),val),
        }
    }
    fn get_value(&self, gb:&GBState) -> u8 {
        match self {
            DstR8(r1) => r1.get_value(&gb.cpu),
            Dst8::AddrDst(r2) => {
                gb.mmu.read8(r2.get_value(&gb.cpu))
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
                let addr = 0xFF00 + (r8.get_value(&gb.cpu) as u16);
                gb.mmu.read8(addr)
            }
            Dst8::MemWithInc(r2) => gb.mmu.read8(r2.get_value(&gb.cpu)),
            Dst8::MemWithDec(r2) => gb.mmu.read8(r2.get_value(&gb.cpu)),
        }
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            DstR8(r8) =>  format!("{:02x}",r8.get_value(&gb.cpu)),
            AddrDst(addr) => format!("{:02x}",gb.mmu.read8(addr.get_value(&gb.cpu))),
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
                let addr = 0xFF00 + (r8.get_value(&gb.cpu) as u16);
                named_addr(addr,gb)
            }
            Dst8::MemWithInc(r16) => format!("{:02x}",gb.mmu.read8(r16.get_value(&gb.cpu))),
            Dst8::MemWithDec(r16) => format!("{:02x}",gb.mmu.read8(r16.get_value(&gb.cpu))),
        }
    }
}

fn named_addr(addr: u16, gb: &GBState) -> String {
    if addr == LCDC_LCDCONTROL { return "LCDC".to_string();  }
    if addr == BGP { return "BGP ".to_string();  }
    if addr == NR52_SOUND { return "NR52".to_string();  }
    let val = gb.mmu.read8(addr);
    format!("{:02x}",val)
}

impl Op {
    pub(crate) fn execute(&self, gb:&mut GBState) {
        match &self.typ {
            OpType::Noop() => {
                gb.cpu.inc_pc();
            }
            Jump(typ) => {
                match typ {
                    Absolute(src) => {
                        let addr = src.get_addr(gb);
                        gb.cpu.set_pc(addr);
                    }
                    JumpType::RelativeCond(cond,src) => {
                        let off = src.get_value(gb);
                        let e = u8_as_i8(off);
                        gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
                        if cond.get_value(&gb.cpu) {
                            let addr = (((gb.cpu.get_pc()) as i32) + (e as i32)) as u16;
                            gb.cpu.set_pc(addr);
                        }
                    }
                    JumpType::Relative(_) => {}
                }
            }
            Load16(dst,src) => {
                let val = src.get_value(gb);
                dst.set_value(gb,val);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            Load8(dst,src) => {
                // println!("executing Load8 dst = {:?} src = {:?}",dst,src);
                let val = src.get_value(gb);
                // println!("value from src is {:02x}",val);
                dst.set_value(gb,val);
                if let Src8::MemWithInc(r2) = src {
                    let v = r2.get_value(&gb.cpu);
                    let (v2,b) = v.overflowing_add(1);
                    r2.set_value(&mut gb.cpu,v2);
                }
                if let Src8::MemWithDec(r2) = src {
                    let v = r2.get_value(&gb.cpu);
                    let (v2,b) = v.overflowing_sub(1);
                    r2.set_value(&mut gb.cpu,v2);
                }
                if let Dst8::MemWithInc(r2) = dst {
                    let v = r2.get_value(&gb.cpu);
                    let (v2,b) = v.overflowing_add(1);
                    r2.set_value(&mut gb.cpu,v2);
                }
                if let Dst8::MemWithDec(r2) = dst {
                    let v = r2.get_value(&gb.cpu);
                    let (v2,b) = v.overflowing_sub(1);
                    r2.set_value(&mut gb.cpu,v2);
                }
                // println!("Len is {}",self.len);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            OpType::DisableInterrupts() => {
                // println!("pretending to disable interrupts");
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            OpType::Compare(dst, src) => {
                let src_v = src.get_value(gb);
                let dst_v = dst.get_value(gb);

                let result = src_v.wrapping_sub(dst_v);
                // println!("comparing {:02x} with {:02x}",dst_v, src_v);
                gb.cpu.r.zero_flag = result == 0;
                gb.cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
                gb.cpu.r.subtract_n_flag = true;
                gb.cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);


                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            // OpType::Xor(dst, src) => {
            //     let b = src.get_value(gb);
            //     let a = dst.get_value(gb);
            //     let res = a ^ b;
            //     gb.cpu.r.zero_flag = res == 0;
            //     gb.cpu.r.subtract_n_flag = false;
            //     gb.cpu.r.half_flag = false;
            //     gb.cpu.r.carry_flag = false;
            //     dst.set_value(gb,res);
            //     gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            // }
            Inc16(dst) => {
                let v1 = dst.get_value(&gb.cpu);
                let v2 = v1.wrapping_add(1);
                dst.set_value(&mut gb.cpu, v2);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            Inc8(dst) => {
                let v1 = dst.get_value(&gb.cpu);
                let result = v1.wrapping_add(1);
                dst.set_value(&mut gb.cpu, result);
                gb.cpu.r.zero_flag = result == 0;
                gb.cpu.r.half_flag = (result & 0x0F) + 1 > 0x0F;
                gb.cpu.r.subtract_n_flag = false;
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            Dec16(dst) => {
                let v1 = dst.get_value(&gb.cpu);
                let v2 = v1.wrapping_sub(1);
                // println!(" ### DEC {:04x}",v2);
                dst.set_value(&mut gb.cpu, v2);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            Dec8(dst) => {
                let v1 = dst.get_value(&gb.cpu);
                let result = v1.wrapping_sub(1);
                dst.set_value(&mut gb.cpu, result);
                gb.cpu.r.zero_flag = result == 0;
                gb.cpu.r.half_flag = (result & 0x0F) == 0;
                gb.cpu.r.subtract_n_flag = true;
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            // OpType::Or(dst, src) => {
            //     let b = src.get_value(gb);
            //     let a = dst.get_value(gb);
            //     let res = a | b;
            //     gb.cpu.r.zero_flag = res == 0;
            //     gb.cpu.r.subtract_n_flag = false;
            //     gb.cpu.r.half_flag = false;
            //     gb.cpu.r.carry_flag = false;
            //     dst.set_value(gb,res);
            //     gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            // }
            // OpType::And(dst, src) => {
            //     let res = dst.get_value(gb) & src.get_value(gb);
            //     // println!("result is {}",res);
            //     gb.cpu.r.zero_flag = res == 0;
            //     gb.cpu.r.subtract_n_flag = false;
            //     gb.cpu.r.half_flag = true;
            //     gb.cpu.r.carry_flag = false;
            //     dst.set_value(gb,res);
            //     gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            // }
            Math(binop, dst,src) => {
                let b = src.get_value(gb);
                let a = dst.get_value(gb);
                let (res, half) = match binop {
                    Or  => (a | b, false),
                    Xor => (a ^ b, false),
                    And => (a & b, true),
                };
                gb.cpu.r.zero_flag = res == 0;
                gb.cpu.r.subtract_n_flag = false;
                gb.cpu.r.half_flag = half;
                gb.cpu.r.carry_flag = false;
                dst.set_value(gb,res);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            BitOp(Bit(n, src)) => {
                let val = src.get_value(&gb.cpu);
                gb.cpu.r.zero_flag = !get_bit_as_bool(val, *n);
                gb.cpu.r.subtract_n_flag = false;
                gb.cpu.r.half_flag = true;
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
        }
    }
    pub(crate) fn to_asm(&self) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            Jump(typ) => {
                match typ {
                    Absolute(src) => "XXXXX".to_string(),
                    JumpType::RelativeCond(cond,src) => format!("JP {},{}",cond.name(),src.name()),
                    JumpType::Relative(src) => format!("JR i{}",src.name()),
                }
            },
            Load16(dst,src) => format!("LD {} {}", dst.name(), src.name()),
            OpType::Load8(dst,src) => format!("LD {}, {}", dst.name(), src.name()),
            OpType::DisableInterrupts() => "DI".to_string(),
            OpType::Compare(dst, src) => format!("CP {},{}", dst.name(), src.name()),
            // OpType::Xor(dst, src) => format!("XOR {},{}",dst.name(),src.name()),
            Inc16(dst,) => format!("INC {}", dst.name()),
            Dec16(dst,) => format!("DEC {}", dst.name()),
            Inc8(dst) => format!("INC {}",dst.name()),
            Dec8(dst) => format!("DEC {}",dst.name()),
            // OpType::Or(dst, src) => format!("OR {},{}",dst.name(),src.name()),
            // OpType::And(dst, src) => format!("AND {},{}",dst.name(),src.name()),
            Math(binop, dst, src) => format!("{} {},{}",binop.name(),dst.name(),src.name()),
            BitOp(Bit(n, src)) => format!("BIT {}, {}", n, src.name()),
        }
    }
    pub(crate) fn real(&self, gb:&GBState) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            Jump(typ) => {
                match typ {
                    Absolute(src) =>  "XXXX".to_string(),
                    JumpType::RelativeCond(cond, src) =>  format!("JR {}, {}", cond.real(gb), src.real(gb)),
                    JumpType::Relative(src) => format!("JR +/-{}", src.real(gb)),
                }
            },
            Load8(dst,src) => format!("LD {}, {}",dst.real(gb), src.real(gb)),
            Load16(dst,src) => format!("LD {}, {}",dst.real(gb), src.real(gb)),
            OpType::DisableInterrupts() => format!("DI"),
            OpType::Compare(dst, src) => format!("CP {}, {}",dst.real(gb), src.real(gb)),
            // OpType::Xor(dst, src) => format!("XOR {}, {}",dst.real(gb),src.real(gb)),
            Inc16(dst) => format!("INC {}", dst.get_value(&gb.cpu)),
            Dec16(dst) => format!("DEC {}", dst.get_value(&gb.cpu)),
            Inc8(dst) => format!("INC {}",dst.get_value(&gb.cpu)),
            Dec8(dst) => format!("DEC {}",dst.get_value(&gb.cpu)),
            // OpType::Or(dst, src) => format!("OR {}, {}", dst.real(gb),src.real(gb)),
            // OpType::And(dst, src) => format!("AND {}, {}", dst.real(gb),src.real(gb)),
            Math(binop, dst, src) => format!("{} {},{}",binop.name(),dst.real(gb),src.real(gb)),
            BitOp(Bit(n, src)) => format!("BIT {}, {}", n, src.get_value(&gb.cpu)),
        }
    }
}


fn fetch_opcode_from_memory(gb:&GBState) -> u16 {
    let pc = gb.cpu.r.pc;
    let fb:u8 = gb.mmu.read8(pc);
    if fb == 0xcb {
        let sb:u8 = gb.mmu.read8(pc+1);
        0xcb00 | sb as u16
    } else {
        fb as u16
    }
}

struct GBState {
    cpu:Z80,
    mmu:MMU,
    clock:u32,
    count:u32,
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
    fn insert(&mut self, code: u16, op: Op) {
        if self.ops.contains_key(&code) {
            println!("ERROR: table already contains {:04x}",code);
            panic!("ERROR table already contains opcode");
        }
        self.ops.insert(code,op);
    }
    fn add(&mut self, op:Op) {
        self.insert(op.code,op);
    }
    fn load8(&mut self, code: u16, dst: Dst8, src: Src8) {
        let (mut len, cycles) = match src {
            SrcR8(_) => (1, 4),
            Mem(_) => (1, 8),
            Im8() => (2, 8),
            Src8::HiMemIm8() => (2,12),
            Src8::MemIm16() => (3,16),
            Src8::MemWithInc(_) => (1,8),
            Src8::MemWithDec(_) => (1,8),
        };
        if let Dst8::HiMemIm8() = dst {
            println!("hi mem. add one more");
            len += 1;
        };
        self.insert(code, Op { code, len, cycles,  typ:OpType::Load8(dst, src)});
    }
    fn load16(&mut self, code: u16, dst: Dst16, src: Src16) {
        let (len, cycles) = match src {
            Im16() => (3,12)
        };
        self.insert(code, Op { code:code, len:len, cycles:cycles, typ: Load16(dst, src), });
    }
    fn lookup(&self, code: &u16) -> Option<&Op> {
        self.ops.get(code)
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

    op_table.load8( 0x78, DstR8(A), SrcR8(B));

    op_table.load16(0x11, DstR16(DE), Im16()); // LD DE, nn
    op_table.load16(0x21, DstR16(HL), Im16()); // LD HL, nn
    op_table.load16(0x31, DstR16(SP), Im16()); // LD HL, nn

    op_table.load8( 0x16, DstR8(D), Im8()); // LD D,n
    op_table.load8( 0x1E, DstR8(E), Im8()); // LD E,n
    op_table.load8( 0x26, DstR8(H), Im8()); // LD H,n
    op_table.load8( 0x2E, DstR8(L), Im8()); // LD L,n


    op_table.load8( 0x7F, DstR8(A), SrcR8(A)); // LD A,A

    op_table.load8( 0x12, AddrDst(DE), SrcR8(A)); // LD L,A
    op_table.load8( 0x77, AddrDst(HL), SrcR8(A)); // LD L,A
    op_table.load8( 0xEA, Dst8::MemIm16(), SrcR8(A)); // LD L,A
    op_table.load8(0xF0, DstR8(A), Src8::HiMemIm8());
    op_table.load8(0xFA,DstR8(A),Src8::MemIm16());
    op_table.load8(0xE0,Dst8::HiMemIm8(), SrcR8(A));
    op_table.load8(0xE2,Dst8::HiMemR8(C), SrcR8(A));
    op_table.load8(0x3E,DstR8(A), Im8());
    op_table.load8(0x2A,DstR8(A), Src8::MemWithInc(HL));
    op_table.load8(0x3A,DstR8(A), Src8::MemWithDec(HL));
    op_table.load8(0x22,Dst8::MemWithInc(HL), SrcR8(A));
    op_table.load8(0x32,Dst8::MemWithDec(HL), SrcR8(A));


    op_table.add(Op { code:0xF3, len: 1, cycles:4, typ:OpType::DisableInterrupts(), });
    op_table.insert(0xFE, Op {
        code: 0xFE,
        len: 2,
        cycles: 8,
        typ: OpType::Compare(DstR8(A), Im8()),
    });




    op_table.add(Op { code:0x03, len: 1, cycles: 8, typ: Inc16(BC)  });
    op_table.add(Op { code:0x13, len: 1, cycles: 8, typ: Inc16(DE)  });
    op_table.add(Op { code:0x23, len: 1, cycles: 8, typ: Inc16(HL)  });
    op_table.add(Op { code:0x33, len: 1, cycles: 8, typ: Inc16(SP)  });

    op_table.add(Op { code:0x0B, len: 1, cycles: 8, typ: Dec16(BC)  });
    op_table.add(Op { code:0x1B, len: 1, cycles: 8, typ: Dec16(DE)  });
    op_table.add(Op { code:0x2B, len: 1, cycles: 8, typ: Dec16(HL)  });
    op_table.add(Op { code:0x3B, len: 1, cycles: 8, typ: Dec16(SP)  });

    op_table.add(Op { code:0x04, len: 1, cycles: 4, typ: Inc8(B)   });
    op_table.add(Op { code:0x05, len: 1, cycles: 4, typ: Dec8(B)   });
    op_table.add(Op { code:0x0C, len: 1, cycles: 4, typ: Inc8(C)   });
    op_table.add(Op { code:0x0D, len: 1, cycles: 4, typ: Dec8(C)   });
    op_table.add(Op { code:0x14, len: 1, cycles: 4, typ: Inc8(D)   });
    op_table.add(Op { code:0x15, len: 1, cycles: 4, typ: Dec8(D)   });
    op_table.add(Op { code:0x1C, len: 1, cycles: 4, typ: Inc8(E)   });
    op_table.add(Op { code:0x1D, len: 1, cycles: 4, typ: Dec8(E)   });


    op_table.add(Op { code:0xB1, len: 1, cycles: 4, typ: Math(Or, DstR8(A),SrcR8(C)) });
    op_table.add(Op { code:0xA7, len: 1, cycles: 4, typ: Math(And,DstR8(A),SrcR8(C)) });
    op_table.add(Op { code:0xAF, len: 1, cycles: 4, typ: Math(Xor,DstR8(A),SrcR8(A)) });

    op_table.add(Op{ code: 0xC3, len: 3, cycles: 12, typ: Jump(Absolute(AddrSrc::Imu16()) )});
    op_table.add(Op{ code: 0x20, len: 2, cycles: 12, typ: Jump(RelativeCond(NotZero(), Im8())) });
    op_table.add(Op{ code: 0x30, len: 2, cycles: 12, typ: Jump(RelativeCond(NotCarry(),Im8())) });
    op_table.add(Op{ code: 0x38, len: 2, cycles: 8,  typ: Jump(RelativeCond(Carry(),   Im8())) });
    op_table.add(Op{ code: 0x18, len: 2, cycles: 12, typ: Jump(Relative(Im8())) });

    op_table.add(Op{ code: 0xCB44, len: 2, cycles: 8,  typ: BitOp(Bit(0, H))  });
    op_table.add(Op{ code: 0xCB4C, len: 2, cycles: 8,  typ: BitOp(Bit(1, H))  });
    op_table.add(Op{ code: 0xCB54, len: 2, cycles: 8,  typ: BitOp(Bit(2, H))  });
    op_table.add(Op{ code: 0xCB5C, len: 2, cycles: 8,  typ: BitOp(Bit(3, H))  });
    op_table.add(Op{ code: 0xCB64, len: 2, cycles: 8,  typ: BitOp(Bit(4, H))  });
    op_table.add(Op{ code: 0xCB6C, len: 2, cycles: 8,  typ: BitOp(Bit(5, H))  });
    op_table.add(Op{ code: 0xCB74, len: 2, cycles: 8,  typ: BitOp(Bit(6, H))  });
    op_table.add(Op{ code: 0xCB7C, len: 2, cycles: 8,  typ: BitOp(Bit(7, H))  });

    op_table
}

fn setup_test_rom(fname: &str) -> Option<GBState> {
    match load_romfile(&PathBuf::from(fname)) {
        Ok(cart) => {
            let mut gb = GBState {
                cpu: Z80::init(),
                mmu: MMU::init(&cart.data),
                clock: 0,
                count: 0,
            };
            gb.cpu.reset();
            gb.cpu.r.pc = 0x100;
            return Some(gb);
        }
        Err(_) => return None
    }
}

#[test]
fn test_cpuins() {
    let op_table = make_op_table();
    let mut gb = setup_test_rom("./resources/testroms/cpu_instrs/individual/10-bit ops.gb").unwrap();

    loop {
        println!("PC {:04x}",gb.cpu.get_pc());
        let opcode = fetch_opcode_from_memory(&gb);
        // println!("opcode is {:04x}",opcode);
        if let None = op_table.lookup(&opcode) {
            println!("failed to lookup op for code {:04x}",opcode);
            break;
        }
        let op = op_table.lookup(&opcode).unwrap();
        println!("PC {:04x} op {:04x} {}  ({},{})",gb.cpu.get_pc(), op.code, op.to_asm(), op.len,op.cycles);
        println!("PC {:04x} {}",gb.cpu.get_pc(),op.real(&gb));
        let prev_pc = gb.cpu.get_pc();
        op.execute(&mut gb);
        if gb.cpu.get_pc() == prev_pc {
            panic!("stuck in an infinite loop");
        }
        // gb.cpu.set_pc(gb.cpu.get_pc()+op.len);
        // mmu.update(&mut cpu, ss, &mut clock);
        // ppu.update(&mut self.mmu, ss, &mut self.clock, &self.to_screen, &self.receive_cpu, self.screen_enabled);
        gb.clock += (op.cycles as u32);
        gb.count += 1;
    }
    let goal = 7;
    println!("hopefully we reached count = {}  really = {} ", goal,gb.count);
    assert_eq!(gb.count>=goal,true);
}

#[test]
fn test_hellogithub() {
    let op_table = make_op_table();
    let mut gb = setup_test_rom("./resources/testroms/hello-world.gb").unwrap();
    let goal = 20_000;
    gb.cpu.set_pc(0);

    let mut debug = false;
    loop {
        if gb.cpu.get_pc() >= 0x001a {
            debug = true;
        }
        if debug {
            println!("==========");
            println!("PC {:04x}    clock = {}   count = {}", gb.cpu.get_pc(), gb.clock, gb.count);
        }
        let opcode = fetch_opcode_from_memory(&gb);
        // println!("opcode is {:04x}",opcode);
        if let None = op_table.lookup(&opcode) {
            println!("failed to lookup op for code {:04x}",opcode);
            break;
        }
        let op = op_table.lookup(&opcode).unwrap();
        if debug {
            println!("PC {:04x} {:04x}  =  {}      ({},{})", gb.cpu.get_pc(), op.code, op.to_asm(), op.len, op.cycles);
            println!("                 {}", op.real(&gb));
            // println!("contents of ram at 0x150 {:02x}", gb.mmu.read8(0x0150));
            // println!("contents of ram at 0x9000 {:02x}", gb.mmu.read8(0x9000));
        }
        let prev_pc = gb.cpu.get_pc();
        op.execute(&mut gb);
        if debug {
            println!("after A:{:02x} B:{:02x} C:{:02x}     BC:{:04x} DE:{:04x} HL:{:04x}  Z={}  C={}",
                     gb.cpu.r.a,
                     gb.cpu.r.b,
                     gb.cpu.r.c,
                     gb.cpu.r.get_bc(),
                     gb.cpu.r.get_de(),
                     gb.cpu.r.get_hl(),
                     gb.cpu.r.zero_flag,
                     gb.cpu.r.carry_flag,
            );
        }
        // println!("BC {:04x}",gb.cpu.r.get_bc());
        if gb.cpu.get_pc() == prev_pc {
            // panic!("stuck in an infinite loop");
            break;
        }
        // gb.cpu.set_pc(gb.cpu.get_pc()+op.len);
        // mmu.update(&mut cpu, ss, &mut clock);
        // ppu.update(&mut self.mmu, ss, &mut self.clock, &self.to_screen, &self.receive_cpu, self.screen_enabled);
        gb.clock += (op.cycles as u32);
        gb.count += 1;

        if gb.count % 20 == 0 {
            gb.mmu.hardware.LY+=1;
            if gb.mmu.hardware.LY >= 154 {
                gb.mmu.hardware.LY = 0;
            }
        }

        if gb.count > goal {
            break;
        }
    }
    println!("hopefully we reached count = {}  really = {} ", goal,gb.count);
    assert_eq!(gb.cpu.get_pc(),0x35)
    // assert_eq!(gb.count>=goal,true);
}


#[test]
fn test_bootrom() {
    let op_table = make_op_table();
    let pth = Path::new("./resources/testroms/dmg_boot.bin");
    let data:Vec<u8> = fs::read(pth).unwrap();

    let mut gb = GBState {
        cpu: Z80::init(),
        mmu: MMU::init(&data),
        clock: 0,
        count: 0,
    };

    let goal = 40_000;
    gb.cpu.set_pc(0);

    let mut debug = false;
    loop {
        // println!("HL is {:04x}",gb.cpu.r.get_hl());
        if gb.cpu.get_pc() >= 0x00_0c {
            println!("reached 0c");
            // break;
            debug = true;
        }
        if gb.count > 35_000 { debug = true;  }
        // if gb.cpu.r.h < 0x9a {
        //     debug = true;
        // }
        if debug {
            println!("==========");
            println!("PC {:04x}    clock = {}   count = {}", gb.cpu.get_pc(), gb.clock, gb.count);
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
        op.execute(&mut gb);
        if debug {
            println!("after A:{:02x} B:{:02x} C:{:02x}  H:{:02x}    BC:{:04x} DE:{:04x} HL:{:04x}  Z={}  C={}",
                     gb.cpu.r.a,
                     gb.cpu.r.b,
                     gb.cpu.r.c,
                     gb.cpu.r.h,
                     gb.cpu.r.get_bc(),
                     gb.cpu.r.get_de(),
                     gb.cpu.r.get_hl(),
                     gb.cpu.r.zero_flag,
                     gb.cpu.r.carry_flag,
            );
        }
        if gb.cpu.get_pc() == prev_pc {
            panic!("stuck in an infinite loop");
            break;
        }
        gb.clock += (op.cycles as u32);
        gb.count += 1;
        if gb.count % 20 == 0 {
            gb.mmu.hardware.LY+=1;
            if gb.mmu.hardware.LY >= 154 {
                gb.mmu.hardware.LY = 0;
            }
        }

        if gb.count > goal {
            break;
        }
    }
    println!("hopefully we reached count = {}  really = {} ", goal,gb.count);
    assert_eq!(gb.cpu.get_pc(),0x0C);
    // assert_eq!(gb.count>=goal,true);
}
