use std::fmt::{Display, Formatter};
use serde_json::Value;
use crate::cpu::Op;
use crate::{MMU, OpList, Z80};
use crate::opcodes::DoubleRegister::{BC, DE, HL, SP};
use crate::opcodes::Load::Load_r_u8;
use crate::opcodes::RegisterName::{A, B, C, D, E, H, L};
use crate::opcodes::Special::{CALL_u16, DisableInterrupts, NOOP, POP, PUSH, RET, STOP};

pub fn setup_op_codes() -> OpList {
    let mut ol = OpList::init();

    //NO-OP
    ol.add(0x00,"NOOP",1,1, |cpu,mmu|());

    //
    // ol.add(0x0e,"LD C, d8",2,8,|cpu,mmu|cpu.r.c = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_1e,"LD E, d8",2,8,|cpu,mmu|cpu.r.e = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_2e,"LD L, d8",2,8,|cpu,mmu|cpu.r.l = mmu.read8(cpu.r.pc+1));
    // ol.add(0x3e,"LD A, d8",2,8,|cpu,mmu|cpu.r.a = mmu.read8(cpu.r.pc+1));

    //16bit immediate loads
    // ol.add(0x00_01,"LD BC d16", 3, 12, |cpu,mmu| {cpu.r.set_bc(mmu.read16(cpu.r.pc+1));});
    ol.add(0x11,"LD DE d16", 3, 12, |cpu,mmu| {cpu.r.set_de(mmu.read16(cpu.r.pc+1));});
    // ol.add(0x21,"LD HL d16", 3, 12, |cpu,mmu| {cpu.r.set_hl(mmu.read16(cpu.r.pc+1));});
    ol.add(0x31,"LD SP d16", 3, 12, |cpu,mmu| {cpu.r.set_sp(mmu.read16(cpu.r.pc+1));});
    //
    //
    // // load 8bit register to 8bit register
    ol.add(0x47, "LD B, A", 1,4,|cpu,mmu| cpu.r.b = cpu.r.a);
    // ol.add(0x00_5f, "LD E, A", 1,4,|cpu,mmu| cpu.r.e = cpu.r.a);
    // ol.add(0x00_67, "LD H, A", 1,4,|cpu,mmu| cpu.r.h = cpu.r.a);
    // ol.add(0x00_6f, "LD L, A", 1,4,|cpu,mmu| cpu.r.l = cpu.r.a);
    //
    // e0 => LDH (n), A => load contents of A into address of (0xFF00 + immediate value)
    // ol.add(0xe0, "LDH(n), A",2,12, |cpu,mmu| {
    //     let v = mmu.read8(cpu.r.pc+1);
    //     println!("n is {:x}",v);
    //     let addr = (0xFF00 as u16) + (v as u16);
    //     println!("calculated address {:04x}",addr);
    //     mmu.write8(addr,cpu.r.a);
    // });

    // put the memory addres 0xFF00 + n into A
    // ol.add(0xF0,"LDH A,(n)",2,3,|cpu,mmu|{
    //     println!("running LDH");
    //     let n = mmu.read8(cpu.r.pc+1);
    //     let addr = (0xFF00 as u16) + (n as u16);
    //     cpu.r.a = mmu.read8(addr);
    //     println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    // });

    // put the immediate value into BC
    ol.add(0x01,"LD BC, u16",3,3,|cpu,mmu|{
        let nn = mmu.read16(cpu.r.pc+1);
        cpu.r.set_bc(nn);
        // let n = mmu.read8(cpu.r.pc+1);
        // let addr = (0xFF00 as u16) + (n as u16);
        // cpu.r.a = mmu.read8(addr);
        // println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    });

    // put value pointed to by DE into A
    // ol.add(0x1a,"LD A,(DE)",1,2,|cpu,mmu|{
    //     cpu.r.a = mmu.read8(cpu.r.get_de())
    //     // let n = mmu.read8(cpu.r.pc+1);
    //     // let addr = (0xFF00 as u16) + (n as u16);
    //     // cpu.r.a = mmu.read8(addr);
    //     // println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    // });

    //MATH
    // ol.add(0xAF,"XOR A,A",1,1,|cpu,mmu|{
    //     cpu.r.a = cpu.r.a ^ cpu.r.a;
    //     if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    // });
    // ol.add(0xA8,"XOR A,B",1,1,|cpu,mmu|{
    //     cpu.r.a = cpu.r.b ^ cpu.r.a;
    //     if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    // });
    // ol.add(0xA9,"XOR A,C",1,1,|cpu,mmu|{
    //     cpu.r.a = cpu.r.b ^ cpu.r.a;
    //     if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    // });


    // // // JUMPs
    // ol.add(0xC3, "JP u16", 0,16,|cpu,mmu|{
    //     let addr = mmu.read16(cpu.r.pc+1);
    //     println!("jumping to address {:04x}",addr);
    //     // subtract off an extra two because the CPU will automatically move us forward three
    //     cpu.r.pc = addr;
    //     // cpu.r.pc = (addr -3) as u16;
    // });
    // JUMP if not zero to the address
    // ol.add(0x20,"JR NZ r8",2,12,|cpu,mmu| {
    //     //convert e to i8 then i32 so it will be interpreted as signed
    //     let e = mmu.read8(cpu.r.pc+1);
    //     let addr = ((cpu.r.pc+2) as i32) + (e as i8 as i32);
    //     if !cpu.r.zero_flag {
    //         //subtract off an extra two because the CPU will automatically move us forward two
    //         cpu.r.pc =  (addr - 2) as u16;
    //         println!("jumping to {:04x}", cpu.r.pc);
    //     }
    // });
    //jump to relative immediate address if condition, signed immediate value.
    //check the carry flag
    // ol.add(0x38, "JR cc,e Carry Flag",0,4,|cpu,mmu|{
    //     let e = mmu.read8(cpu.r.pc+1);
    //     cpu.r.pc += 2;
    //     // println!("immediate value is 0x{:x} {} {}",e, (e as i8), u8_as_i8(e));
    //     println!("carry flag is set to {}",cpu.r.carry_flag);
    //     if cpu.r.carry_flag {
    //         // let v2 = (e as i8);
    //         // println!("signed {}",v2);
    //         // let v3 = cpu.r.pc as i32;
    //         // println!("v3 is {}",v3);
    //         // let v4 = v3 + (v2 as i32);
    //         let addr = (((cpu.r.pc) as i32) + (u8_as_i8(e) as i32));
    //         cpu.r.pc =  addr as u16;
    //         println!("jumping to {:04x}", cpu.r.pc);
    //     } else {
    //         println!("not jumping, just continuing");
    //     }
    // });
    //unconditional Jump to relative address specified by signed 8bit immediate value
    // ol.add(0x18,"JR e",0,3,|cpu,mmu|{
    //     let e = mmu.read8(cpu.r.pc+1);
    //     let addr = ((cpu.r.pc as i32) + (u8_as_i8(e)) as i32);
    //     println!("jumping by {}",u8_as_i8(e));
    //     cpu.r.pc = addr as u16;
    // });


    // //Returns
    // ol.add(0x00C9, "RET",1,4,|cpu,mmu|{
    //     let dst = mmu.read16(cpu.r.sp);
    //     cpu.r.sp = cpu.r.sp + 2;
    //     println!("returning from a function at {:0x} to dst {:0x}", cpu.r.sp,dst);
    //     cpu.r.pc = dst;
    // });
    // ol.add(0x00_c0, "RET NZ",1,2,|cpu,mmu|{
    //     if cpu.r.zero_flag {
    //         println!("returning");
    //     } else {
    //         println!("not returning");
    //     }
    // });
    //

    // //Load A, (HL+),  copy contents of memory at HL to A, then INC HL
    // ol.add(0x2a,"LD A, (HL+)",1,2,|cpu,mmu|{
    //     cpu.r.a = mmu.read8(cpu.r.get_hl());
    //     cpu.r.set_hl(cpu.r.get_hl()+1);
    // });
    // Load (HL+), A, copy contents of A into memory at HL, then INC HL
    // ol.add(0x22,"LD (HL+), A",1,2,|cpu,mmu|{
    //     mmu.write8(cpu.r.get_hl(),cpu.r.a);
    //     cpu.r.set_hl(cpu.r.get_hl()+1);
    // });

    // ol.add(0x13,"INC DE",1,2,|cpu,mmu|{
    //     println!("incrementing DE");
    //     cpu.r.set_de(cpu.r.get_de()+1);
    // });

    // ol.add(0x0B,"DEC BC",1,1,|cpu,mmu|{
    //     println!("decrementing BC");
    //     let (v2, changed) = cpu.r.get_bc().overflowing_sub(1);
    //     cpu.r.set_bc(v2);
    //     if v2 == 0 { cpu.r.zero_flag = true; }
    //     cpu.r.subtract_n_flag = true;
    // });


    // ol.add(0xB1,"OR A, C",1,1,|cpu,mmu|{
    //     println!("ORING C with A");
    //     cpu.r.a = cpu.r.c | cpu.r.a;
    //     if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    // });
    // ol.add(0xB2,"OR A, D",1,1,|cpu,mmu|{
    //     println!("ORING C with D");
    //     cpu.r.a = cpu.r.d | cpu.r.a;
    //     if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    // });

    // ol.add(0xA7,"AND A",1,1,|cpu,mmu|{
    //     println!("ANDING A with itself");
    //     cpu.r.a = cpu.r.a | cpu.r.a;
    // });
    // ol.add(0x0012,"LD (DE),A",1,2,|cpu,mmu|{
    //     mmu.write8(cpu.r.get_de(),cpu.r.a);
    // });

    // ol.add(0x00f3,"DI", 1,1,|cpu,mmu|{
    //     println!("Disabling interrupts");
    // });

    // Compare A with u8 n. sets flags based on teh comparison
    // ol.add(0xFE, "CP A,n",2,4,|cpu,mmu|{
    //     let n = mmu.read8(cpu.r.pc+1);
    //     println!("comparing A:{:x} to n:{:x}",cpu.r.a,n);
    //     // let v  = cpu.r.a - n;
    //     cpu.r.zero_flag = cpu.r.a == n;
    //     cpu.r.carry_flag = cpu.r.a < n;
    //     cpu.r.subtract_n_flag = true;
    // });

    return ol;
}

// pub fn LD(r1: RegisterName, r2: RegisterName, cpu:&mut Z80) -> Option<(usize, usize)> {
//     let value = get_cpu_register_u8(cpu, &r2);
//     set_cpu_register_u8(cpu, &r1, value);
//     println!("LDing {:?} {:?}",r1.to_string(),r2.to_string());
//     Some((1,1))
// }
// pub fn LD_r_u8(reg: RegisterName, cpu:&mut Z80, mmu:&mut MMU) -> Option<(usize, usize)> {
//     set_cpu_register_u8(cpu, &reg, mmu.read8(cpu.r.pc+1));
//     Some((2,8))
// }
// pub fn INC(reg:RegisterName, cpu:&mut Z80) -> Option<(usize, usize)> {
//     let old_value = get_cpu_register_u8(cpu,&reg);
//     // println!("register a contains {:x}",old_value);
//     let (new_value, changed) = old_value.overflowing_add(1);
//     //         println!("now it is {:x} flipped={}", new_value, changed);
//     set_cpu_register_u8(cpu,&reg,new_value);
//     //         // cpu.r.a = new_value;
//     if new_value == 0 { cpu.r.zero_flag = true; }
//     cpu.r.subtract_n_flag = false;
//     //         println!("zero flag is {}",cpu.r.zero_flag);
//     Some((1,1))
// }
//
// pub fn DEC(reg:RegisterName, cpu:&mut Z80) -> Option<(usize, usize)> {
//     // println!("register c contains {:x}",get_cpu_register_u8(cpu,&reg));
//     let (v2, changed) = get_cpu_register_u8(cpu,&reg).overflowing_sub(1);
//     // println!("now it is {:x} flipped={}",v2, changed);
//     set_cpu_register_u8(cpu,&reg,v2);
//     // cpu.r.c = v2;
//     if v2 == 0 { cpu.r.zero_flag = true; }
//     cpu.r.subtract_n_flag = true;
//     // println!("zero flag is {}",cpu.r.zero_flag);
//     Some((1,1))
// }


pub fn u8_as_i8(v: u8) -> i8 {
    return v as i8
}

pub enum RegisterName {
    A,B,C,D,E,H,L
}
pub enum DoubleRegister {
    BC,DE,HL, SP,
}
pub enum Special {
    NOOP(),
    STOP(),
    DisableInterrupts(),
    CALL_u16(),
    PUSH(DoubleRegister),
    POP(DoubleRegister),
    RET(),
}
pub enum Load {
    Load_r_u8(RegisterName),
    Load_r_r(RegisterName,RegisterName),

    Load_high_r_u8(RegisterName),
    Load_high_u8_r(RegisterName),
    Load_high_r_r(RegisterName,RegisterName),

    Load_R2_u16(DoubleRegister),
    Load_r_addr_R2(DoubleRegister),
    Load_addr_R2_A_inc(DoubleRegister),  // Load (HL+), A, copy contents of A into memory at HL, then INC HL
    Load_addr_R2_A_dec(DoubleRegister),  // Load (HL+), A, copy contents of A into memory at HL, then DEC HL
    Load_A_addr_R2_inc(DoubleRegister),  // Load A, (HL+), copy contents of memory at HL to A, then INC HL
    Load_addr_R2_A(DoubleRegister),      // Load (rr), A
    Load_addr_u16_A() // Load (nn), A
}
pub enum Jump {
    JumpAbsolute_u16(),
    JumpRelative_i8(),
    JumpRelative_cond_carry_i8(),
    JumpRelative_cond_notcarry_i8(),
    JumpRelative_cond_zero_i8(),
    JumpRelative_cond_notzero_i8(),
}
pub enum Compare {
    CP_A_r(RegisterName),
    CP_A_n()
}
pub enum Math {
    XOR_A_r(RegisterName),
    OR_A_r(RegisterName),
    AND_A_r(RegisterName),
    Inc_r(RegisterName),
    Inc_rr(DoubleRegister),
    Dec_r(RegisterName),
    Dec_rr(DoubleRegister),
    BIT(u8,RegisterName),
    RL(RegisterName),
    RLA(),
    RLC(RegisterName),
    RLCA(),
    RR(RegisterName),
    RRA(),
    RRC(RegisterName),
    RRCA(),
}
pub enum Instr {
    Load(Load),
    Special(Special),
    Jump(Jump),
    Compare(Compare),
    Math(Math),
}

impl Display for RegisterName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            A => "A",
            B => "B",
            C => "C",
            D => "D",
            E => "E",
            H => "H",
            L => "L",
        })
    }
}

impl Display for DoubleRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            DoubleRegister::BC => "BC",
            DoubleRegister::DE => "DE",
            DoubleRegister::HL => "HL",
            SP => "SP",
        })
    }
}

fn set_cpu_register_u8(cpu: &mut Z80, reg: &RegisterName, nv: u8) {
    match reg {
        RegisterName::A => cpu.r.a = nv,
        RegisterName::B => cpu.r.b = nv,
        RegisterName::C => cpu.r.c = nv,
        RegisterName::D => cpu.r.d = nv,
        RegisterName::E => cpu.r.e = nv,
        RegisterName::H => cpu.r.h = nv,
        RegisterName::L => cpu.r.l = nv,
    }
}

fn get_cpu_register_u8(cpu: &mut Z80, reg: &RegisterName) -> u8 {
    match reg {
        RegisterName::A => cpu.r.a,
        RegisterName::B => cpu.r.b,
        RegisterName::C => cpu.r.c,
        RegisterName::D => cpu.r.d,
        RegisterName::E => cpu.r.e,
        RegisterName::H => cpu.r.h,
        RegisterName::L => cpu.r.l,
    }
}

pub fn lookup_opcode(code:u16) -> Option<Instr> {
    return match code {

        // loads
        0x40 => Some(Instr::Load(Load::Load_r_r(B,B))),
        0x41 => Some(Instr::Load(Load::Load_r_r(B,C))),
        0x42 => Some(Instr::Load(Load::Load_r_r(B,D))),
        0x43 => Some(Instr::Load(Load::Load_r_r(B,E))),
        0x44 => Some(Instr::Load(Load::Load_r_r(B,H))),
        0x45 => Some(Instr::Load(Load::Load_r_r(B,L))),
        0x47 => Some(Instr::Load(Load::Load_r_r(B,A))),
        0x48 => Some(Instr::Load(Load::Load_r_r(C,B))),
        0x49 => Some(Instr::Load(Load::Load_r_r(C,C))),
        0x4A => Some(Instr::Load(Load::Load_r_r(C,D))),
        0x4B => Some(Instr::Load(Load::Load_r_r(C,E))),
        0x4C => Some(Instr::Load(Load::Load_r_r(C,H))),
        0x4D => Some(Instr::Load(Load::Load_r_r(C,L))),
        0x4F => Some(Instr::Load(Load::Load_r_r(C,A))),

        0x50 => Some(Instr::Load(Load::Load_r_r(D,B))),
        0x51 => Some(Instr::Load(Load::Load_r_r(D,C))),
        0x52 => Some(Instr::Load(Load::Load_r_r(D,D))),
        0x53 => Some(Instr::Load(Load::Load_r_r(D,E))),
        0x54 => Some(Instr::Load(Load::Load_r_r(D,H))),
        0x55 => Some(Instr::Load(Load::Load_r_r(D,L))),
        0x57 => Some(Instr::Load(Load::Load_r_r(D,A))),
        0x58 => Some(Instr::Load(Load::Load_r_r(E,B))),
        0x59 => Some(Instr::Load(Load::Load_r_r(E,C))),
        0x5A => Some(Instr::Load(Load::Load_r_r(E,D))),
        0x5B => Some(Instr::Load(Load::Load_r_r(E,E))),
        0x5C => Some(Instr::Load(Load::Load_r_r(E,H))),
        0x5D => Some(Instr::Load(Load::Load_r_r(E,L))),
        0x5F => Some(Instr::Load(Load::Load_r_r(E,A))),

        0x60 => Some(Instr::Load(Load::Load_r_r(H,B))),
        0x61 => Some(Instr::Load(Load::Load_r_r(H,C))),
        0x62 => Some(Instr::Load(Load::Load_r_r(H,D))),
        0x63 => Some(Instr::Load(Load::Load_r_r(H,E))),
        0x64 => Some(Instr::Load(Load::Load_r_r(H,H))),
        0x65 => Some(Instr::Load(Load::Load_r_r(H,L))),
        0x67 => Some(Instr::Load(Load::Load_r_r(H,A))),

        0x68 => Some(Instr::Load(Load::Load_r_r(L,B))),
        0x69 => Some(Instr::Load(Load::Load_r_r(L,C))),
        0x6A => Some(Instr::Load(Load::Load_r_r(L,D))),
        0x6B => Some(Instr::Load(Load::Load_r_r(L,E))),
        0x6C => Some(Instr::Load(Load::Load_r_r(L,H))),
        0x6D => Some(Instr::Load(Load::Load_r_r(L,L))),
        0x6F => Some(Instr::Load(Load::Load_r_r(L,A))),


        0x78 => Some(Instr::Load(Load::Load_r_r(A,B))),
        0x79 => Some(Instr::Load(Load::Load_r_r(A,C))),
        0x7A => Some(Instr::Load(Load::Load_r_r(A,D))),
        0x7B => Some(Instr::Load(Load::Load_r_r(A,E))),
        0x7C => Some(Instr::Load(Load::Load_r_r(A,H))),
        0x7D => Some(Instr::Load(Load::Load_r_r(A,L))),
        0x7F => Some(Instr::Load(Load::Load_r_r(A,A))),


        // 8bit immediate value to register copy: LD r,n
        0x06 => Some(Instr::Load(Load_r_u8(B))),
        0x0E => Some(Instr::Load(Load_r_u8(C))),
        0x16 => Some(Instr::Load(Load_r_u8(D))),
        0x1E => Some(Instr::Load(Load_r_u8(E))),
        0x26 => Some(Instr::Load(Load_r_u8(H))),
        0x2E => Some(Instr::Load(Load_r_u8(L))),
        0x3E => Some(Instr::Load(Load_r_u8(A))),

        0xE0 => Some(Instr::Load(Load::Load_high_u8_r(A))),

        0x01 => Some(Instr::Load(Load::Load_R2_u16(BC))),
        0x11 => Some(Instr::Load(Load::Load_R2_u16(DE))),
        0x21 => Some(Instr::Load(Load::Load_R2_u16(HL))),
        0x31 => Some(Instr::Load(Load::Load_R2_u16(SP))),

        0x1a => Some(Instr::Load(Load::Load_r_addr_R2(DE))),
        // put value pointed to by DE into A

        0x22 => Some(Instr::Load(Load::Load_addr_R2_A_inc(HL))),
        0x32 => Some(Instr::Load(Load::Load_addr_R2_A_dec(HL))),
        0x2A => Some(Instr::Load(Load::Load_A_addr_R2_inc(HL))),

        0x02 => Some(Instr::Load(Load::Load_addr_R2_A(BC))),
        0x12 => Some(Instr::Load(Load::Load_addr_R2_A(DE))),
        0x77 => Some(Instr::Load(Load::Load_addr_R2_A(HL))),

        0xe2 => Some(Instr::Load(Load::Load_high_r_r(C,A))),
        0xea => Some(Instr::Load(Load::Load_addr_u16_A())),


        0x00 => Some(Instr::Special(NOOP())),
        0xF3 => Some(Instr::Special(DisableInterrupts())),
        0x10 => Some(Instr::Special(STOP())),
        0xCD => Some(Instr::Special(CALL_u16())),
        0xC1 => Some(Instr::Special(POP(BC))),
        0xC5 => Some(Instr::Special(PUSH(BC))),
        0xD1 => Some(Instr::Special(POP(DE))),
        0xD5 => Some(Instr::Special(PUSH(DE))),
        0xE1 => Some(Instr::Special(POP(HL))),
        0xE5 => Some(Instr::Special(PUSH(HL))),
        0xC9 => Some(Instr::Special(RET())),
        // 0xF1 => Some(Instr::Special(POP(AF))),
        // 0xF5 => Some(Instr::Special(PUSH(AF))),

        0xF0 => Some(Instr::Load(Load::Load_high_r_u8(A))),

        0x18 => Some(Instr::Jump(Jump::JumpRelative_i8())), //2 bytes, no flags, relative signed
        0x20 => Some(Instr::Jump(Jump::JumpRelative_cond_notzero_i8())),
        0x28 => Some(Instr::Jump(Jump::JumpRelative_cond_zero_i8())), //2 bytes, if zero, relative signed
        0x30 => Some(Instr::Jump(Jump::JumpRelative_cond_notcarry_i8())), //2 bytes, if not carry, realtive signed
        0x38 => Some(Instr::Jump(Jump::JumpRelative_cond_carry_i8())), //2 bytes, if carry, relative signed
        0xC3 => Some(Instr::Jump(Jump::JumpAbsolute_u16())),
        0xFE => Some(Instr::Compare(Compare::CP_A_n())),

        0xA8 => Some(Instr::Math(Math::XOR_A_r(B))),
        0xA9 => Some(Instr::Math(Math::XOR_A_r(C))),
        0xAA => Some(Instr::Math(Math::XOR_A_r(D))),
        0xAB => Some(Instr::Math(Math::XOR_A_r(E))),
        0xAC => Some(Instr::Math(Math::XOR_A_r(H))),
        0xAD => Some(Instr::Math(Math::XOR_A_r(L))),
        0xAF => Some(Instr::Math(Math::XOR_A_r(A))),

        0xA0 => Some(Instr::Math(Math::AND_A_r(B))),
        0xA1 => Some(Instr::Math(Math::AND_A_r(C))),
        0xA2 => Some(Instr::Math(Math::AND_A_r(D))),
        0xA3 => Some(Instr::Math(Math::AND_A_r(E))),
        0xA4 => Some(Instr::Math(Math::AND_A_r(H))),
        0xA5 => Some(Instr::Math(Math::AND_A_r(L))),
        0xA7 => Some(Instr::Math(Math::AND_A_r(A))),

        0xB0 => Some(Instr::Math(Math::OR_A_r(B))),
        0xB1 => Some(Instr::Math(Math::OR_A_r(C))),
        0xB2 => Some(Instr::Math(Math::OR_A_r(D))),
        0xB3 => Some(Instr::Math(Math::OR_A_r(E))),
        0xB4 => Some(Instr::Math(Math::OR_A_r(H))),
        0xB5 => Some(Instr::Math(Math::OR_A_r(L))),
        0xB7 => Some(Instr::Math(Math::OR_A_r(A))),



        //increments and decrements
        0x04 => Some(Instr::Math(Math::Inc_r(B))),
        0x14 => Some(Instr::Math(Math::Inc_r(D))),
        0x24 => Some(Instr::Math(Math::Inc_r(H))),
        0x05 => Some(Instr::Math(Math::Dec_r(B))),
        0x15 => Some(Instr::Math(Math::Dec_r(D))),
        0x25 => Some(Instr::Math(Math::Dec_r(H))),

        0x0c => Some(Instr::Math(Math::Inc_r(C))),
        0x1c => Some(Instr::Math(Math::Inc_r(E))),
        0x2c => Some(Instr::Math(Math::Inc_r(L))),
        0x3c => Some(Instr::Math(Math::Inc_r(A))),

        0x0d => Some(Instr::Math(Math::Dec_r(C))),
        0x1d => Some(Instr::Math(Math::Dec_r(E))),
        0x2d => Some(Instr::Math(Math::Dec_r(L))),
        0x3d => Some(Instr::Math(Math::Dec_r(A))),

        0x03 => Some(Instr::Math(Math::Inc_rr(BC))),
        0x13 => Some(Instr::Math(Math::Inc_rr(DE))),
        0x23 => Some(Instr::Math(Math::Inc_rr(HL))),
        0x33 => Some(Instr::Math(Math::Inc_rr(SP))),

        0x0B => Some(Instr::Math(Math::Dec_rr(BC))),
        0x1B => Some(Instr::Math(Math::Dec_rr(DE))),
        0x2B => Some(Instr::Math(Math::Dec_rr(HL))),
        0x3B => Some(Instr::Math(Math::Dec_rr(SP))),


        // bit manipulation
        0xCB7C => Some(Instr::Math(Math::BIT(7, H))),
        0x07 => Some(Instr::Math(Math::RLCA())),
        0x17 => Some(Instr::Math(Math::RLA())),
        0x0F => Some(Instr::Math(Math::RRCA())),
        0x1F => Some(Instr::Math(Math::RRA())),

        0xCB00 => Some(Instr::Math(Math::RLC(B))),
        0xCB01 => Some(Instr::Math(Math::RLC(C))),
        0xCB02 => Some(Instr::Math(Math::RLC(D))),
        0xCB03 => Some(Instr::Math(Math::RLC(E))),
        0xCB04 => Some(Instr::Math(Math::RLC(H))),
        0xCB05 => Some(Instr::Math(Math::RLC(L))),
        0xCB07 => Some(Instr::Math(Math::RLC(A))),

        0xCB08 => Some(Instr::Math(Math::RRC(B))),
        0xCB09 => Some(Instr::Math(Math::RRC(C))),
        0xCB0A => Some(Instr::Math(Math::RRC(D))),
        0xCB0B => Some(Instr::Math(Math::RRC(E))),
        0xCB0C => Some(Instr::Math(Math::RRC(H))),
        0xCB0D => Some(Instr::Math(Math::RRC(L))),
        0xCB0F => Some(Instr::Math(Math::RRC(A))),

        0xCB11 => Some(Instr::Math(Math::RL(C))),



        _ => {
            println!("WARNING. can't lookup opcode {:04x}",code);
            None
        }
    }
}
pub fn decode(code:u16, arg:u16, cpu:&mut Z80, mmu:&mut MMU, opcodes: &Value) -> (usize, usize) {
    println!("executing op {:02x}", code);
    if let Some(op) = cpu.ops.ops.get(&code) {
        println!("PC {:04x}: OP {:04x}: {}", cpu.r.pc, code, op.name);
        let il = op.inst_len;
        let tl = op.tim_len;
        (op.fun)(cpu, mmu);
        return (il, tl);
    }
    let opcode_str =format!("0x{:2x}",code);
    println!("unknown opcode {}",opcode_str);
    let def = opcodes.as_object().unwrap()
        .get("unprefixed").unwrap().as_object().unwrap()
        .get(&opcode_str).unwrap();
    let j = serde_json::to_string_pretty(&def).unwrap();
    println!("def is {}",j);
    panic!("unknown op code {:04x}", code);
}
