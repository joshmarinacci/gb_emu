use std::collections::HashMap;
use std::fmt::format;
use std::path::{Path, PathBuf};
use Dst16::DstR16;
use Dst8::AddrDst;
use OpType::{Jump, Load16};
use Src16::Im16;
// use Op::Add8;
use crate::{load_romfile, MMU, Z80};
use crate::mmu::{BGP, LCDC_LCDCONTROL, NR52_SOUND};
use crate::opcodes::{DoubleRegister, RegisterName, u8_as_i8};
use crate::optest::AddrSrc::Imu16;
use crate::optest::Dst8::DstR8;
use crate::optest::Extra::Nothing;
use crate::optest::OpType::Load8;
use crate::optest::R8::{A, B, C, D, E, H, L};
use crate::optest::R16::{BC, DE, HL, SP};
use crate::optest::Src8::{HiMemIm8, Im8, Mem, SrcR8};

#[derive(Debug)]
pub enum R8  { A, B, C, D, E, H, L,}
#[derive(Debug)]
pub enum R16 { BC, HL, DE, SP,}
#[derive(Debug)]
pub enum Src8  {  SrcR8(R8), Mem(R16), MemWithInc(R16), Im8(), HiMemIm8(), MemIm16() }
#[derive(Debug)]
pub enum Src16 {  Im16(),   }
#[derive(Debug)]
pub enum Dst8  {  DstR8(R8), AddrDst(R16), HiMemIm8(), MemIm16(), MemWithInc(R16)  }
#[derive(Debug)]
pub enum Dst16 {  DstR16(R16), }
#[derive(Debug)]
pub enum Extra {
    Nothing,
    Inc(R16),
}
#[derive(Debug)]
enum OpType {
    Noop(),
    Jump(JumpType),
    Load16(Dst16, Src16),
    Load8(Dst8, Src8),
    DisableInterrupts(),
    Compare(Dst8, Src8),
    Xor(Dst8, Src8),
    Or(Dst8, Src8),
    And(Dst8, Src8),
    Inc(Dst16),
    Dec(Dst16),
}


struct Op {
    code:u16,
    len:u16,
    cycles:u16,
    typ:OpType,
    extra: Extra,
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
            Cond::Carry() => cpu.r.carry_flag,
            Cond::NotCarry() => !cpu.r.carry_flag,
            Cond::Zero() => cpu.r.zero_flag,
            Cond::NotZero() => !cpu.r.zero_flag,
        }
    }
}


#[derive(Debug)]
enum JumpType {
    Absolute(AddrSrc),
    RelativeCond(Cond,Src8),
    Relative(Src8),
}


impl Cond {
    fn name(&self) -> &str {
        match self {
            Cond::Carry() => "C",
            Cond::NotCarry() => "NC",
            Cond::Zero() => "Z",
            Cond::NotZero() => "NZ",
        }
    }
    fn real(&self, gb: &GBState) -> bool {
        match self {
            Cond::Carry() => gb.cpu.r.carry_flag,
            Cond::NotCarry() => !gb.cpu.r.carry_flag,
            Cond::Zero() => gb.cpu.r.zero_flag,
            Cond::NotZero() => !gb.cpu.r.zero_flag,
        }
    }
}
impl Src8 {
    fn name(&self) -> String {
        match self {
            Src8::SrcR8(r1) => r1.name().to_string(),
            Mem(r2) => format!("({})",r2.name()),
            Im8() => "u8".to_string(),
            Src8::HiMemIm8() => "($FF00+n)".to_string(),
            Src8::MemIm16() => "(nn)".to_string(),
            Src8::MemWithInc(r2) => format!("({}+)",r2.name()),
        }
    }
    fn get_value(&self, gb:&GBState) -> u8 {
        match self {
            Src8::SrcR8(r1) => r1.get_value(&gb.cpu),
            Src8::Mem(r2) => {
                // println!("reading memory at location {:04x}",r2.get_value(&gb.cpu));
                gb.mmu.read8(r2.get_value(&gb.cpu))
            },
            Src8::MemWithInc(r2) => gb.mmu.read8(r2.get_value(&gb.cpu)),
            Src8::Im8() => gb.mmu.read8(gb.cpu.get_pc()+1),
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
            Dst8::DstR8(r8) => r8.name().to_string(),
            AddrDst(r16) => r16.name().to_string(),
            Dst8::MemIm16() => "(nn)".to_string(),
            Dst8::HiMemIm8() => "($FF00+n)".to_string(),
            Dst8::MemWithInc(r16) => format!("({}+)", r16.name()).to_string(),
        }
    }
    fn set_value(&self,gb:&mut GBState, val:u8) {
        match self {
            Dst8::DstR8(r8) => r8.set_value(&mut gb.cpu, val),
            AddrDst(r16) => gb.mmu.write8(r16.get_value(&gb.cpu), val),
            Dst8::MemIm16() => gb.mmu.write8(gb.mmu.read16(gb.cpu.get_pc()+1),val),
            Dst8::HiMemIm8() => {
                let im = gb.mmu.read8(gb.cpu.get_pc()+1);
                gb.mmu.write8(0xFF00 + im as u16,val)
            }
            Dst8::MemWithInc(r16) => {
                gb.mmu.write8(r16.get_value(&gb.cpu),val)
            }
        }
    }
    fn get_value(&self, gb:&GBState) -> u8 {
        match self {
            Dst8::DstR8(r1) => r1.get_value(&gb.cpu),
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
            Dst8::MemWithInc(r2) => gb.mmu.read8(r2.get_value(&gb.cpu))
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
            Dst8::MemWithInc(r16) => format!("{:02x}",gb.mmu.read8(r16.get_value(&gb.cpu))),
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
                    JumpType::Absolute(src) => {
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
                if let Dst8::MemWithInc(r2) = dst {
                    let v = r2.get_value(&gb.cpu);
                    let (v2,b) = v.overflowing_add(1);
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
            OpType::Xor(dst, src) => {
                let b = src.get_value(gb);
                let a = dst.get_value(gb);
                let res = a ^ b;
                gb.cpu.r.zero_flag = res == 0;
                gb.cpu.r.subtract_n_flag = false;
                gb.cpu.r.half_flag = false;
                gb.cpu.r.carry_flag = false;
                dst.set_value(gb,res);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            OpType::Inc(dst) => {
                let v1 = dst.get_value(gb);
                let v2 = v1.wrapping_add(1);
                // println!(" ### INC {:04x}",v2);
                dst.set_value(gb, v2);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            OpType::Dec(dst) => {
                let v1 = dst.get_value(gb);
                let v2 = v1.wrapping_sub(1);
                // println!(" ### DEC {:04x}",v2);
                dst.set_value(gb, v2);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            OpType::Or(dst, src) => {
                let b = src.get_value(gb);
                let a = dst.get_value(gb);
                let res = a | b;
                gb.cpu.r.zero_flag = res == 0;
                gb.cpu.r.subtract_n_flag = false;
                gb.cpu.r.half_flag = false;
                gb.cpu.r.carry_flag = false;
                dst.set_value(gb,res);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
            OpType::And(dst, src) => {
                let res = dst.get_value(gb) & src.get_value(gb);
                // println!("result is {}",res);
                gb.cpu.r.zero_flag = res == 0;
                gb.cpu.r.subtract_n_flag = false;
                gb.cpu.r.half_flag = true;
                gb.cpu.r.carry_flag = false;
                dst.set_value(gb,res);
                gb.cpu.set_pc(gb.cpu.get_pc()+self.len);
            }
        }
    }
    pub(crate) fn to_asm(&self) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            Jump(typ) => {
                match typ {
                    JumpType::Absolute(src) => "XXXXX".to_string(),
                    JumpType::RelativeCond(cond,src) => format!("JP {},{}",cond.name(),src.name()),
                    JumpType::Relative(src) => format!("JR i{}",src.name()),
                }
            },
            Load16(dst,src) => format!("LD {} {}", dst.name(), src.name()),
            OpType::Load8(dst,src) => format!("LD {}, {}", dst.name(), src.name()),
            OpType::DisableInterrupts() => "DI".to_string(),
            OpType::Compare(dst, src) => format!("CP {},{}", dst.name(), src.name()),
            OpType::Xor(dst, src) => format!("XOR {},{}",dst.name(),src.name()),
            OpType::Inc(dst,) => format!("INC {}",dst.name()),
            OpType::Dec(dst,) => format!("DEC {}",dst.name()),
            OpType::Or(dst, src) => format!("OR {},{}",dst.name(),src.name()),
            OpType::And(dst, src) => format!("AND {},{}",dst.name(),src.name()),
        }
    }
    pub(crate) fn real(&self, gb:&GBState) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            Jump(typ) => {
                match typ {
                    JumpType::Absolute(src) => {
                        "XXXX".to_string()
                    }
                    JumpType::RelativeCond(cond, src) => {
                        // format!("JP {}",src.real(gb))
                        format!("JR {}, {}", cond.real(gb), src.real(gb))
                    }
                    JumpType::Relative(src) => {
                        format!("JR +/-{}", src.real(gb))
                    }
                }
            },
            OpType::Load8(dst,src) => format!("LD {}, {}",dst.real(gb), src.real(gb)),
            Load16(dst,src) => format!("LD {}, {}",dst.real(gb), src.real(gb)),
            OpType::DisableInterrupts() => format!("DI"),
            OpType::Compare(dst, src) => format!("CP {}, {}",dst.real(gb), src.real(gb)),
            OpType::Xor(dst, src) => format!("XOR {}, {}",dst.real(gb),src.real(gb)),
            OpType::Inc(dst) => format!("INC {}",dst.real(gb)),
            OpType::Dec(dst) => format!("DEC {}",dst.real(gb)),
            OpType::Or(dst, src) => format!("OR {}, {}", dst.real(gb),src.real(gb)),
            OpType::And(dst, src) => format!("AND {}, {}", dst.real(gb),src.real(gb)),
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
    fn load8(&mut self, code: u16, dst: Dst8, src: Src8) {
        let (mut len, cycles) = match src {
            SrcR8(_) => (1, 4),
            Mem(_) => (1, 8),
            Im8() => (2, 8),
            Src8::HiMemIm8() => (2,12),
            Src8::MemIm16() => (3,16),
            Src8::MemWithInc(_) => (1,8),
        };
        if let Dst8::HiMemIm8() = dst {
            println!("hi mem. add one more");
            len += 1;
        };
        self.insert(code, Op { code, len, cycles,  typ:OpType::Load8(dst, src), extra:Extra::Nothing});
    }
    fn load16(&mut self, code: u16, dst: Dst16, src: Src16) {
        let (len, cycles) = match src {
            Im16() => (3,12)
        };
        self.insert(code, Op { code:code, len:len, cycles:cycles, typ: Load16(dst, src), extra:Extra::Nothing});
    }
    fn lookup(&self, code: &u16) -> Option<&Op> {
        self.ops.get(code)
    }
}


fn make_op_table() -> OpTable {
    let mut op_table = OpTable::new();//:HashMap<u16,Op> = HashMap::new();
    op_table.insert(0x00, Op { code:0x00, len:1, cycles:4,  typ:OpType::Noop(), extra:Extra::Nothing });
    op_table.insert(0xC3, Op { code:0xC3, len:3, cycles:12,
        typ: Jump(JumpType::Absolute(AddrSrc::Imu16()) ) ,
        extra:Extra::Nothing});

    op_table.load8( 0x78, DstR8(A), SrcR8(B));
    op_table.load8( 0x40, DstR8(B), SrcR8(B));

    op_table.load16(0x01, DstR16(BC), Im16()); // LD DE, nn
    op_table.load16(0x11, DstR16(DE), Im16()); // LD DE, nn
    op_table.load16(0x21, DstR16(HL), Im16()); // LD HL, nn
    op_table.load16(0x31, DstR16(SP), Im16()); // LD HL, nn

    op_table.load8( 0x06, DstR8(B), Im8()); // LD B,n
    op_table.load8( 0x0E, DstR8(C), Im8()); // LD C,n
    op_table.load8( 0x16, DstR8(D), Im8()); // LD D,n
    op_table.load8( 0x1E, DstR8(E), Im8()); // LD E,n
    op_table.load8( 0x26, DstR8(H), Im8()); // LD H,n
    op_table.load8( 0x2E, DstR8(L), Im8()); // LD L,n


    op_table.load8( 0x7F, DstR8(A), SrcR8(A)); // LD A,A
    op_table.load8( 0x47, DstR8(B), SrcR8(A)); // LD B,A
    op_table.load8( 0x4F, DstR8(C), SrcR8(A)); // LD C,A
    op_table.load8( 0x57, DstR8(D), SrcR8(A)); // LD D,A
    op_table.load8( 0x5F, DstR8(E), SrcR8(A)); // LD E,A
    op_table.load8( 0x67, DstR8(H), SrcR8(A)); // LD H,A
    op_table.load8( 0x6F, DstR8(L), SrcR8(A)); // LD L,A

    op_table.load8( 0x02, AddrDst(BC), SrcR8(A)); // LD L,A
    op_table.load8( 0x12, AddrDst(DE), SrcR8(A)); // LD L,A
    op_table.load8( 0x77, AddrDst(HL), SrcR8(A)); // LD L,A
    op_table.load8( 0xEA, Dst8::MemIm16(), SrcR8(A)); // LD L,A



    op_table.insert(0xF3, Op { code:0xF3, len: 1, cycles:4,
        typ:OpType::DisableInterrupts(),
        extra:Extra::Nothing,
    });

    // 0xF0 => Some(LoadInstr(Load_HI_R_U8(A))),
    op_table.load8(0xF0, DstR8(A), Src8::HiMemIm8());

    //0xFE => Some(CompareInst(CP_A_n())),
    op_table.insert(0xFE, Op {
        code: 0xFE,
        len: 2,
        cycles: 8,
        typ: OpType::Compare(DstR8(A),Src8::Im8()),
        extra: Extra::Nothing
    });

    //        0x38 => Some(JumpInstr(Relative_cond_carry_i8())), //2 bytes, if carry, relative signed
    op_table.insert(0x38, Op{
        code: 0x38,
        len: 2,
        cycles: 8,
        typ: OpType::Jump(JumpType::RelativeCond(Cond::Carry(), Src8::Im8())),
        extra: Extra::Nothing
    });

    //        0xFA => Some(LoadInstr(Load_A_addr_u16())),
    op_table.load8(0xFA, DstR8(A),Src8::MemIm16());

    // xAF => XOR A, A
    op_table.insert(0xAF, Op {
        code: 0xAF,
        len: 1,
        cycles: 4,
        typ: OpType::Xor(DstR8(A),SrcR8(A)),
        extra: Extra::Nothing
    });

    // 0xE0 => Some(LoadInstr(Load_HI_U8_R(A))),
    op_table.load8(0xE0, Dst8::HiMemIm8(), SrcR8(A));

    //        0x1a => Some(LoadInstr(Load_R_addr_R2(A, DE))),
    op_table.load8(0x1A,DstR8(A), Src8::Mem(DE));
    op_table.insert(0x2A, Op { code:0x2A, len:1, cycles:8,
        typ:Load8(DstR8(A), Src8::MemWithInc(HL) ), extra:Nothing,
    });
    op_table.insert(0x22, Op { code:0x22, len:1, cycles:8,
        typ:Load8(Dst8::MemWithInc(HL), Src8::SrcR8(A)),
        extra:Extra::Inc(HL),
    });


    //        0x13 => Some(MathInst(Inc_rr(DE))),
    op_table.insert(0x13, Op {
        code:0x13,
        len: 1,
        cycles: 8,
        typ: OpType::Inc(DstR16(DE)),
        extra: Extra::Nothing
    });
    //        0x0B => Some(MathInst(Dec_rr(BC))),
    op_table.insert(0x0B, Op {
        code:0x0B,
        len: 1,
        cycles: 8,
        typ: OpType::Dec(DstR16(BC)),
        extra: Extra::Nothing
    });
    //        0xB1 => Some(MathInst(OR_A_r(C))),
    op_table.insert(0xB1, Op {
        code:0xB1,
        len: 1,
        cycles: 4,
        typ: OpType::Or(Dst8::DstR8(A),Src8::SrcR8(C)),
        extra: Extra::Nothing
    });
    //        0x20 => Some(JumpInstr(Relative_cond_notzero_i8())),
    op_table.insert(0x20,Op {
        code: 0x20,
        len: 2,
        cycles: 12,
        typ: Jump(JumpType::RelativeCond(Cond::NotZero(),Src8::Im8())),
        extra: Extra::Nothing
    });
    //        0xA7 => Some(MathInst(AND_A_r(A))),
    op_table.insert(0xA7, Op {
        code:0xA7,
        len: 1,
        cycles: 4,
        typ: OpType::And(Dst8::DstR8(A),Src8::SrcR8(C)),
        extra: Extra::Nothing
    });


    // 0x3E => Some(LoadInstr(Load_R_u8(A))),
    op_table.load8(0x3E,Dst8::DstR8(A),Src8::Im8());

    op_table.insert(0x18, Op {
        code: 0x18,
        len: 2,
        cycles: 12,
        typ: OpType::Jump(JumpType::Relative(Src8::Im8())),
        extra: Extra::Nothing
    });

    // try to get through the memory setup routine that is copying everything to 9000
    // aka: Tile Data Block 2 filled. probably somewhere around 40,000 cycles?
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
