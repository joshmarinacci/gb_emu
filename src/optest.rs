use std::collections::HashMap;
use std::path::{Path, PathBuf};
use Carry::{No, Yes};
use Dst16::DstR16;
use Dst8::AddrDst;
use OpType::{Jump, Load16};
use Src16::Im16;
// use Op::Add8;
use crate::{load_romfile, MMU, Z80};
use crate::opcodes::{DoubleRegister, RegisterName};
use crate::optest::AddrSrc::Imu16;
use crate::optest::Dst8::DstR8;
use crate::optest::OpType::Load8;
use crate::optest::R8::{A, B, C, D, E, H, L};
use crate::optest::R16::{BC, DE, HL, SP};
use crate::optest::Src8::{Im8, Mem, SrcR8};

pub enum R8  { A, B, C, D, E, H, L,}
pub enum R16 { BC, HL, DE, SP,}
pub enum Src8  {  SrcR8(R8), Mem(R16),  Im8(), }
pub enum Src16 {  Im16(),   }
pub enum Dst8  {  DstR8(R8), AddrDst(R16), MemIm16()  }
pub enum Dst16 {  DstR16(R16), }
enum OpType {
    Noop(),
    Jump(JumpType,AddrSrc),
    Load16(Dst16, Src16),
    Load8(Dst8, Src8),
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
            DE => cpu.r.get_u16reg(&DoubleRegister::BC),
            SP => cpu.r.get_u16reg(&DoubleRegister::BC),
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
        self.name()
    }
}
pub enum Carry {
    Yes,
    No
}
pub enum JumpType {
    Absolute(),
}



impl Src8 {
    fn name(&self) -> String {
        match self {
            Src8::SrcR8(r1) => r1.name().to_string(),
            Mem(r2) => format!("({})",r2.name()),
            Im8() => "u8".to_string(),
        }
    }
    fn get_value(&self, gb:&GBState) -> u8 {
        match self {
            Src8::SrcR8(r1) => r1.get_value(&gb.cpu),
            Src8::Mem(r2) => gb.mmu.read8(r2.get_value(&gb.cpu)),
            Src8::Im8() => gb.mmu.read8(gb.cpu.get_pc()+1),
        }
    }
    fn real(&self, gb: &GBState) -> String {
        format!("{:02x}",self.get_value(gb))
    }
}
impl Dst8 {
    fn name(&self) -> &'static str {
        match self {
            Dst8::DstR8(r8) => r8.name(),
            AddrDst(r16) => r16.name(),
            Dst8::MemIm16() => "(nn)",
        }
    }
    fn set_value(&self,gb:&mut GBState, val:u8) {
        match self {
            Dst8::DstR8(r8) => r8.set_value(&mut gb.cpu, val),
            AddrDst(r16) => gb.mmu.write8(r16.get_value(&gb.cpu), val),
            Dst8::MemIm16() => gb.mmu.write8(gb.mmu.read16(gb.cpu.get_pc()+1),val),
        }
    }
    fn real(&self, gb: &GBState) -> String {
        self.name().to_string()
    }
}

impl Op {
    pub(crate) fn execute(&self, gb:&mut GBState) {
        match &self.typ {
            OpType::Noop() => {
                gb.cpu.inc_pc();
            }
            Jump(typ,src) => {
                let addr = src.get_addr(gb);
                gb.cpu.set_pc(addr);
            }
            Load16(dst,src) => {
                let val = src.get_value(gb);
                dst.set_value(gb,val);
                gb.cpu.inc_pc();
                gb.cpu.inc_pc();
                gb.cpu.inc_pc();
            }
            Load8(dst,src) => {
                let val = src.get_value(gb);
                dst.set_value(gb,val);
                gb.cpu.inc_pc();
            }
        }
    }
    pub(crate) fn to_asm(&self) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            Jump(typ, src,) => format!("JP {}",src.name()),
            Load16(dst,src) => format!("LD {} {}", dst.name(), src.name()),
            OpType::Load8(dst,src) => format!("LD {}, {}", dst.name(), src.name()),
        }
    }
    pub(crate) fn real(&self, gb:&GBState) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            Jump(typ,src) => format!("JP {}",src.real(gb)),
            OpType::Load8(dst,src) => format!("LD {}, {}",dst.real(gb), src.real(gb)),
            Load16(dst,src) => format!("LD {}, {}",dst.real(gb), src.real(gb)),
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
        self.ops.insert(code,op);
    }
    fn load8(&mut self, code: u16, dst: Dst8, src: Src8) {
        let (len, cycles) = match src {
            SrcR8(_) => (1, 4),
            Mem(_) => (1, 8),
            Im8() => (2, 8),
        };
        self.insert(code, Op { code, len, cycles,  typ:OpType::Load8(dst, src)});
    }
    fn load16(&mut self, code: u16, dst: Dst16, src: Src16) {
        let (len, cycles) = match src {
            Im16() => (3,12)
        };
        self.insert(code, Op { code:code, len:len, cycles:cycles, typ: Load16(dst, src) });
    }
    fn lookup(&self, code: &u16) -> Option<&Op> {
        self.ops.get(code)
    }
}


fn make_op_table() -> OpTable {
    let mut op_table = OpTable::new();//:HashMap<u16,Op> = HashMap::new();
    op_table.insert(0x00, Op { code:0x00, len:1, cycles:4,  typ:OpType::Noop() });
    op_table.insert(0xC3, Op { code:0xC3, len:3, cycles:12, typ: Jump(JumpType::Absolute(), AddrSrc::Imu16()) });
    op_table.load8( 0x47, DstR8(B), SrcR8(A));

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
    op_table.load8( 0x02, AddrDst(DE), SrcR8(A)); // LD L,A
    op_table.load8( 0x02, AddrDst(HL), SrcR8(A)); // LD L,A
    op_table.load8( 0x02, Dst8::MemIm16(),   SrcR8(A)); // LD L,A

    op_table
}


#[test]
fn test_cpuins() {
    let op_table = make_op_table();

    let fname = "./resources/testroms/cpu_instrs/individual/10-bit ops.gb";
    if let Ok(cart) = load_romfile(&PathBuf::from(fname)) {
        let mut gb = GBState {
            cpu:Z80::init(),
            mmu:MMU::init(&cart.data),
            clock:0,
            count:0,
        };
        gb.cpu.reset();
        gb.cpu.r.pc = 0x100;


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
    } else {
        println!("couldnt read file {}",fname);
        panic!("test failed");
    }
}
