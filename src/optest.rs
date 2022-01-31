use std::collections::HashMap;
use std::path::{Path, PathBuf};
use Carry::{No, Yes};
// use Op::Add8;
use crate::{load_romfile, MMU, Z80};
use crate::opcodes::{DoubleRegister, RegisterName};
use crate::optest::AddrSrc::Imu16;
use crate::optest::R1::{A, B, C, D, E, H, L};
use crate::optest::R2::{BC, DE, HL, SP};
use crate::optest::Src::{Imu8, Mem, SrcR1};

pub enum R1 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl R1 {
    fn get_value(&self, cpu: &Z80) -> u8 {
        match self {
            R1::A => cpu.r.a,
            R1::B => cpu.r.b,
            R1::C => cpu.r.c,
            R1::D => cpu.r.d,
            R1::E => cpu.r.e,
            R1::H => cpu.r.h,
            R1::L => cpu.r.l,
        }
    }
    fn set_value(&self, cpu: &mut Z80, value:u8) {
        match self {
            R1::A => cpu.r.set_u8reg(&RegisterName::A,value),
            R1::B => cpu.r.set_u8reg(&RegisterName::B,value),
            R1::C => cpu.r.set_u8reg(&RegisterName::C,value),
            R1::D => cpu.r.set_u8reg(&RegisterName::D,value),
            R1::E => cpu.r.set_u8reg(&RegisterName::E,value),
            R1::H => cpu.r.set_u8reg(&RegisterName::H,value),
            R1::L => cpu.r.set_u8reg(&RegisterName::L,value),
        }
    }
    fn name(&self) -> &'static str{
        match self {
            R1::A => "A",
            R1::B => "B",
            R1::C => "C",
            R1::D => "D",
            R1::E => "E",
            R1::H => "H",
            R1::L => "L",
        }
    }
}
pub enum R2 {
    BC,
    HL,
    DE,
    SP,
}
impl R2 {
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

pub enum Src {
    SrcR1(R1),
    Mem(R2),
    Imu8(),
}


pub enum Src16 {
    Imu16(),
}


impl Src16 {
    fn name(&self) -> String {
        match self {
            Src16::Imu16() => "nn".to_string(),
        }
    }
    fn get_value(&self, gb:&GBState) -> u16 {
        gb.mmu.read16(gb.cpu.get_pc()+1)
    }
    fn real(&self, gb: &GBState) -> String {
        match self {
            Src16::Imu16() => format!("{:04x}",gb.mmu.read16(gb.cpu.get_pc()+1))
        }
    }
}
pub enum Dst {
    DstR1(R1),
}

pub enum Dst16 {
    R2(R2),
}


impl Dst16 {
    fn name(&self) -> String {
        match self {
            Dst16::R2(r2) => r2.name().to_string(),
        }
    }
    fn set_value(&self, gb:&mut GBState, val:u16) {
        match self {
            Dst16::R2(r2) => r2.set_value(&mut gb.cpu,val)
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
enum OpType {
    // Add8(u16, Dst, Src, Carry, u8, u8),
    // Add16(u16, R2, R2, u8, u8),
    Noop(),
    Jump(JumpType,AddrSrc),
    Load16(Dst16, Src16),
    Load8(Dst, Src),
}


struct Op {
    code:u16,
    len:u16,
    cycles:u16,
    typ:OpType,
}

impl Src {
    fn name(&self) -> String {
        match self {
            Src::SrcR1(r1) => r1.name().to_string(),
            Mem(r2) => format!("({})",r2.name()),
            Imu8() => "u8".to_string(),
        }
    }
    fn get_value(&self, gb:&GBState) -> u8 {
        match self {
            Src::SrcR1(r1) => r1.get_value(&gb.cpu),
            Src::Mem(r2) => gb.mmu.read8(r2.get_value(&gb.cpu)),
            Src::Imu8() => gb.mmu.read8(gb.cpu.get_pc()+1),
        }
    }
    fn real(&self, gb: &GBState) -> String {
        format!("{:02x}",self.get_value(gb))
    }
}
impl Dst {
    fn name(&self) -> &'static str {
        match self {
            Dst::DstR1(r1) => r1.name()
        }
    }
    fn get_value(&self,cpu:&mut Z80, mmu:&mut MMU) -> u8 {
        match self {
            Dst::DstR1(r1) => r1.get_value(cpu),
        }
    }
    fn set_value(&self,gb:&mut GBState, val:u8) {
        match self {
            Dst::DstR1(r1) => r1.set_value(&mut gb.cpu,val),
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
            OpType::Jump(typ,src) => {
                let addr = src.get_addr(gb);
                gb.cpu.set_pc(addr);
            }
            OpType::Load16(dst,src) => {
                let val = src.get_value(gb);
                dst.set_value(gb,val);
                gb.cpu.inc_pc();
                gb.cpu.inc_pc();
                gb.cpu.inc_pc();
            }
            OpType::Load8(dst,src) => {
                let val = src.get_value(gb);
                dst.set_value(gb,val);
                gb.cpu.inc_pc();
            }
        }
    }
    pub(crate) fn to_asm(&self) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            OpType::Jump(typ, src,) => format!("JP {}",src.name()),
            OpType::Load16(dst,src) => format!("LD {} {}", dst.name(), src.name()),
            OpType::Load8(dst,src) => format!("LD {}, {}", dst.name(), src.name()),
        }
    }
    pub(crate) fn real(&self, gb:&GBState) -> String {
        match &self.typ {
            OpType::Noop() => "NOOP".to_string(),
            OpType::Jump(typ,src) => format!("JP {}",src.real(gb)),
            OpType::Load8(dst,src) => format!("LD {}, {}",dst.real(gb), src.real(gb)),
            OpType::Load16(dst,src) => format!("LD {}, {}",dst.real(gb), src.real(gb)),
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



#[test]
fn test_cpuins() {
    let mut op_table:HashMap<u16,Op> = HashMap::new();
    op_table.insert(0x00, Op { code:0x00, len:1, cycles:4,  typ:OpType::Noop() });
    op_table.insert(0xC3, Op { code:0xC3, len:3, cycles:12, typ:OpType::Jump(JumpType::Absolute(), AddrSrc::Imu16()) });
    op_table.insert(0x21, Op { code:0x21, len:3, cycles:12, typ:OpType::Load16(Dst16::R2(HL),Src16::Imu16()) });
    op_table.insert(0x47, Op { code:0x47, len:1, cycles:4,  typ:OpType::Load8(Dst::DstR1(R1::B),Src::SrcR1(R1::A))});

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
            if let None = op_table.get(&opcode) {
                println!("failed to lookup op for code {:04x}",opcode);
                break;
            }
            let op = op_table.get(&opcode).unwrap();
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
        let goal = 5;
        println!("hopefully we reached count = {}  really = {} ", goal,gb.count);
        assert_eq!(gb.count>=goal,true);
    } else {
        println!("couldnt read file {}",fname);
        panic!("test failed");
    }
}
