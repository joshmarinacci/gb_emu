use std::collections::HashMap;
use Carry::{No, Yes};
use Math::Add8;
use crate::{MMU, Z80};
use crate::opcodes::{DoubleRegister, RegisterName};
use crate::optest::Math::Add16;
use crate::optest::R1::{A, B, C, D, E, H, L};
use crate::optest::R2::{BC, DE, HL, SP};
use crate::optest::Src::{Imu8, Mem, SrcR1};

enum R1 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl R1 {
    fn get_value(&self, cpu: &mut Z80) -> u8 {
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
enum R2 {
    BC,
    HL,
    DE,
    SP,
}
impl R2 {
    fn get_value(&self, cpu: &mut Z80) -> u16 {
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
enum Src {
    SrcR1(R1),
    Mem(R2),
    Imu8(),
}
impl Src {
    fn name(&self) -> String {
        match self {
            Src::SrcR1(r1) => r1.name().to_string(),
            Mem(r2) => format!("({})",r2.name()),
            Imu8() => "u8".to_string(),
        }
    }
    fn get_value(&self, cpu:&mut Z80, mmu:&mut MMU) -> u8 {
        match self {
            Src::SrcR1(r1) => r1.get_value(cpu),
            Src::Mem(r2) => mmu.read8(r2.get_value(cpu)),
            Src::Imu8() => mmu.read8(cpu.get_pc()+1),
        }
    }
}
enum Dst {
    DstR1(R1),
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
    fn set_value(&self,cpu:&mut Z80, mmu:&mut MMU, val:u8) {
        match self {
            Dst::DstR1(r1) => r1.set_value(cpu,val),
        }
    }
}
enum Carry {
    Yes,
    No
}
enum Math {
    Add8(u16, Dst, Src, Carry, u8, u8),
    Add16(u16, R2, R2, u8, u8),
}

impl Math {
    pub(crate) fn execute(&self, cpu: &mut Z80, mmu: &mut MMU) {
        match self {
            Add8(_, dst, src, _, _, _) => {
                let dst_v = dst.get_value(cpu, mmu);
                let src_v = src.get_value(cpu, mmu);
                let result = dst_v.wrapping_add(src_v);
                dst.set_value(cpu,mmu,result);
                cpu.r.zero_flag = result == 0;
                cpu.r.subtract_n_flag = false;
                cpu.r.half_flag = (dst_v & 0x0F) + (src_v & 0x0f) > 0xF;
                cpu.r.carry_flag = (dst_v as u16) + (src_v as u16) > 0xFF;
            }
            Add16(_, dst, src, _, _) => {
                let src_v = src.get_value(cpu);
                let dst_v = dst.get_value(cpu);
                let result = dst_v.wrapping_add(src_v);
                dst.set_value(cpu,result);
                cpu.r.subtract_n_flag = false;
                //dont modify the zero flag
                cpu.r.half_flag = (dst_v & 0x07FF) + (src_v & 0x07FF) > 0x07FF;
                cpu.r.carry_flag = (dst_v) > (0xFFFF - src_v);
            }
        }

    }
}

impl Math {
    pub(crate) fn len(&self) -> u8 {
        match self {
            Add8(_, _, _, _, len, cycles) => *len,
            Add16(_, _, _, len, cy) => *len,
        }
    }
    pub(crate) fn cycles(&self) -> &u8 {
        match self {
            Add8(_, _, _, _, len, cycles) => cycles,
            Add16(_, _, _, _, cy) => cy,
        }
    }
    pub(crate) fn to_asm(&self) -> String {
        match self {
            Add8(_, dst, src, carry, _, _) => {
                match carry {
                    Yes => format!("ADC {},{}",dst.name(),src.name()),
                    No  => format!("ADD {},{}",dst.name(),src.name()),
                }
            }
            Add16(_, dst, src, _, _) => {
                format!("ADD {},{}",dst.name(),src.name())
            }
        }
    }
    pub(crate) fn to_code(&self) -> &u16 {
        match self {
            Add8(hex, _, _, _, _, _) => hex,
            Add16(hex, _, _, _, _) => hex,
        }
    }
}

fn lookup_op(code:u16) -> Math {
    match code {
        0x09 => Add16(0x09, HL, BC, 1, 8),
        0x19 => Add16(0x19, HL, DE, 1, 8),
        0x29 => Add16(0x29, HL, HL, 1, 8),
        0x39 => Add16(0x39, HL, SP, 1, 8),

        0x80 => Add8(0x80, Dst::DstR1(A), Src::SrcR1(B), No, 1, 4),
        0x81 => Add8(0x81, Dst::DstR1(A), Src::SrcR1(C), No, 1, 4),
        0x82 => Add8(0x82, Dst::DstR1(A), Src::SrcR1(D), No, 1, 4),
        0x83 => Add8(0x83, Dst::DstR1(A), Src::SrcR1(E), No, 1, 4),
        0x84 => Add8(0x84, Dst::DstR1(A), Src::SrcR1(H), No, 1, 4),
        0x85 => Add8(0x85, Dst::DstR1(A), Src::SrcR1(L), No, 1, 4),
        0x86 => Add8(0x86, Dst::DstR1(A), Mem(HL), No, 2, 8),
        0x87 => Add8(0x87, Dst::DstR1(A), Src::SrcR1(A), No, 1, 4),

        0x88 => Add8(0x88, Dst::DstR1(A), Src::SrcR1(B), Yes, 1, 4),
        0x89 => Add8(0x89, Dst::DstR1(A), Src::SrcR1(C), Yes, 1, 4),
        0x8A => Add8(0x8A, Dst::DstR1(A), Src::SrcR1(D), Yes, 1, 4),
        0x8B => Add8(0x8B, Dst::DstR1(A), Src::SrcR1(E), Yes, 1, 4),
        0x8C => Add8(0x8c, Dst::DstR1(A), Src::SrcR1(H), Yes, 1, 4),
        0x8D => Add8(0x8d, Dst::DstR1(A), Src::SrcR1(L), Yes, 1, 4),
        0x8E => Add8(0x8e, Dst::DstR1(A), Mem(HL), Yes, 1, 8),
        0xCE => Add8(0xCE, Dst::DstR1(A), Imu8(), Yes, 2, 8),

        0xC6 => Add8(0xC6, Dst::DstR1(A), Imu8(), No, 2, 8),
        0xE8 => Add16(0xE8, Dst::DstR2(SP), Imu8(),  2,8),

        _ => {
            panic!("unknown op cde")
        }
    }
}

#[test]
fn test1() {
    let code = 0xC6;
    let op = lookup_op(code);
    println!("got the op {} == {}",code, op.to_code());
    println!("in assembly notation {}", op.to_asm());
    println!("cycles {}  instruction length {}", op.cycles(), op.len());
    println!("no we will execute it ");

    let mut cpu = Z80::init();
    let rom:[u8;3] = [
        0xC6,0x05, // LD A, 0x05
        0x09, // ADD HL, BC,
    ];
    let mut mmu = MMU::init(&rom.to_vec());
    let mut clock = 0;
    op.execute(&mut cpu, &mut mmu);
    cpu.set_pc(cpu.get_pc()+(op.len() as u16));
    clock += op.cycles();
    println!("after register A is {} {}",cpu.r.a,0x05);
    println!("clock = {} pc = {:04x}",clock,cpu.get_pc());

    {
        let op = lookup_op((rom[2]) as u16);
        println!("got the op {} == {}",code, op.to_code());
        println!("in assembly notation {}", op.to_asm());
        println!("cycles {}  instruction length {}", op.cycles(), op.len());
        println!("no we will execute it ");
    }
}
