use std::fmt::{Display, Formatter};
use log::debug;
use Compare::{CP_A_n, CP_A_r};
use Instr::{CompareInst, JumpInstr, LoadInstr, MathInst, SpecialInstr};
use Jump::{Absolute_cond_notzero_u16, Absolute_u16, Relative_cond_carry_i8, Relative_cond_notcarry_i8, Relative_cond_notzero_i8, Relative_cond_zero_i8, Relative_i8};
use Load::{Load_A_addr_R2_inc, Load_addr_R2_A_dec, Load_addr_R2_A_inc, Load_addr_R2_r, Load_addr_u16_A, Load_addr_u16_R2, Load_HI_R_R, Load_HI_R_U8, Load_HI_U8_R, Load_R2_U16, Load_R_addr_R2, Load_R_R};
use Math::{ADD_R_R, ADD_R_u8, AND_A_r, BIT, BITR2, Dec_r, Dec_rr, Inc_r, Inc_rr, OR_A_r, RL, RLA, RLC, RLCA, RR, RRA, RRC, RRCA, SLA, SUB_R_R, XOR_A_r};
use crate::opcodes::Compare::CP_A_addr;
use crate::opcodes::DoubleRegister::{AF, BC, DE, HL, SP};
use crate::opcodes::Load::{Load_A_addr_u16, Load_addr_R2_u8, Load_R_HI_R, Load_R_u8};
use crate::opcodes::Math::{ADD_A_addr, ADD_RR_RR, AND_A_addr, AND_A_u8, OR_A_addr, SUB_A_addr, XOR_A_addr, XOR_A_u8};
use crate::opcodes::RegisterName::{A, B, C, D, E, F, H, L};
use crate::opcodes::Special::{CALL_u16, DisableInterrupts, EnableInterrupts, HALT, NOOP, POP, PUSH, RET, RETI, RETZ, RST, STOP};
use crate::{MMU, Z80};
use crate::common::get_bit_as_bool;

pub fn u8_as_i8(v: u8) -> i8 {
    v as i8
}

#[derive(Debug)]
pub enum RegisterName {
    A,B,C,D,E,H,L,F,
}
#[derive(Debug)]
pub enum DoubleRegister {
    BC,DE,HL,SP,AF,
}
#[derive(Debug)]
pub enum Special {
    NOOP(),
    STOP(),
    HALT(),
    DisableInterrupts(),
    EnableInterrupts(),
    CALL_u16(),
    PUSH(DoubleRegister),
    POP(DoubleRegister),
    RET(),
    RETZ(),
    RETI(),
    RST(u8),
}
#[derive(Debug)]
pub enum Load {
    Load_R_u8(RegisterName),
    Load_R_R(RegisterName, RegisterName),

    Load_HI_R_U8(RegisterName),
    Load_HI_U8_R(RegisterName),
    Load_HI_R_R(RegisterName, RegisterName),
    Load_R_HI_R(RegisterName, RegisterName),
    Load_R2_U16(DoubleRegister),
    Load_R_addr_R2(RegisterName, DoubleRegister),
    Load_addr_R2_A_inc(DoubleRegister),  // Load (HL+), A, copy contents of A into memory at HL, then INC HL
    Load_addr_R2_A_dec(DoubleRegister),  // Load (HL+), A, copy contents of A into memory at HL, then DEC HL
    Load_A_addr_R2_inc(DoubleRegister),  // Load A, (HL+), copy contents of memory at HL to A, then INC HL
    Load_addr_R2_r(DoubleRegister, RegisterName),      // Load (rr), R
    Load_addr_R2_u8(DoubleRegister),
    Load_addr_u16_A(), // Load (nn), A
    Load_A_addr_u16(), // Load (nn), A
    Load_addr_u16_R2(DoubleRegister),
}
#[derive(Debug)]
pub enum Jump {
    Absolute_u16(),
    Relative_i8(),
    Relative_cond_carry_i8(),
    Relative_cond_notcarry_i8(),
    Relative_cond_zero_i8(),
    Relative_cond_notzero_i8(),
    Absolute_cond_notzero_u16(),
}
#[derive(Debug)]
pub enum Compare {
    CP_A_r(RegisterName),
    CP_A_addr(DoubleRegister),
    CP_A_n()
}
#[derive(Debug)]
pub enum Math {
    ADD_R_u8(RegisterName),
    ADD_R_R(RegisterName,RegisterName),
    ADD_RR_RR(DoubleRegister,DoubleRegister),
    ADD_A_addr(DoubleRegister),
    SUB_R_R(RegisterName,RegisterName),
    SUB_A_addr(DoubleRegister),
    XOR_A_r(RegisterName),
    XOR_A_u8(),
    XOR_A_addr(DoubleRegister),
    OR_A_r(RegisterName),
    OR_A_addr(DoubleRegister),
    AND_A_r(RegisterName),
    AND_A_u8(),
    AND_A_addr(DoubleRegister),
    Inc_r(RegisterName),
    Inc_rr(DoubleRegister),
    Dec_r(RegisterName),
    Dec_rr(DoubleRegister),
    BIT(u8,RegisterName),
    BITR2(u8,DoubleRegister),
    RL(RegisterName),
    RLA(),
    RLC(RegisterName),
    RLCA(),
    RR(RegisterName),
    RRA(),
    RRC(RegisterName),
    RRCA(),
    SLA(RegisterName),
}
#[derive(Debug)]
pub enum Instr {
    LoadInstr(Load),
    SpecialInstr(Special),
    JumpInstr(Jump),
    CompareInst(Compare),
    MathInst(Math),
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
            F => "F",
        })
    }
}

impl Display for DoubleRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            BC => "BC",
            DE => "DE",
            HL => "HL",
            SP => "SP",
            AF => "AF",
        })
    }
}


pub fn lookup_opcode(code:u16) -> Option<Instr> {
    return match code {

        // loads
        0x40 => Some(LoadInstr(Load_R_R(B, B))),
        0x41 => Some(LoadInstr(Load_R_R(B, C))),
        0x42 => Some(LoadInstr(Load_R_R(B, D))),
        0x43 => Some(LoadInstr(Load_R_R(B, E))),
        0x44 => Some(LoadInstr(Load_R_R(B, H))),
        0x45 => Some(LoadInstr(Load_R_R(B, L))),
        0x46 => Some(LoadInstr(Load_R_addr_R2(B, HL))),
        0x47 => Some(LoadInstr(Load_R_R(B, A))),
        0x48 => Some(LoadInstr(Load_R_R(C, B))),
        0x49 => Some(LoadInstr(Load_R_R(C, C))),
        0x4A => Some(LoadInstr(Load_R_R(C, D))),
        0x4B => Some(LoadInstr(Load_R_R(C, E))),
        0x4C => Some(LoadInstr(Load_R_R(C, H))),
        0x4D => Some(LoadInstr(Load_R_R(C, L))),
        0x4E => Some(LoadInstr(Load_R_addr_R2(C, HL))),
        0x4F => Some(LoadInstr(Load_R_R(C, A))),

        0x50 => Some(LoadInstr(Load_R_R(D, B))),
        0x51 => Some(LoadInstr(Load_R_R(D, C))),
        0x52 => Some(LoadInstr(Load_R_R(D, D))),
        0x53 => Some(LoadInstr(Load_R_R(D, E))),
        0x54 => Some(LoadInstr(Load_R_R(D, H))),
        0x55 => Some(LoadInstr(Load_R_R(D, L))),
        0x56 => Some(LoadInstr(Load_R_addr_R2(D, HL))),
        0x57 => Some(LoadInstr(Load_R_R(D, A))),
        0x58 => Some(LoadInstr(Load_R_R(E, B))),
        0x59 => Some(LoadInstr(Load_R_R(E, C))),
        0x5A => Some(LoadInstr(Load_R_R(E, D))),
        0x5B => Some(LoadInstr(Load_R_R(E, E))),
        0x5C => Some(LoadInstr(Load_R_R(E, H))),
        0x5D => Some(LoadInstr(Load_R_R(E, L))),
        0x5E => Some(LoadInstr(Load_R_addr_R2(E, HL))),
        0x5F => Some(LoadInstr(Load_R_R(E, A))),

        0x60 => Some(LoadInstr(Load_R_R(H, B))),
        0x61 => Some(LoadInstr(Load_R_R(H, C))),
        0x62 => Some(LoadInstr(Load_R_R(H, D))),
        0x63 => Some(LoadInstr(Load_R_R(H, E))),
        0x64 => Some(LoadInstr(Load_R_R(H, H))),
        0x65 => Some(LoadInstr(Load_R_R(H, L))),
        0x66 => Some(LoadInstr(Load_R_addr_R2(H, HL))),
        0x67 => Some(LoadInstr(Load_R_R(H, A))),
        0x68 => Some(LoadInstr(Load_R_R(L, B))),
        0x69 => Some(LoadInstr(Load_R_R(L, C))),
        0x6A => Some(LoadInstr(Load_R_R(L, D))),
        0x6B => Some(LoadInstr(Load_R_R(L, E))),
        0x6C => Some(LoadInstr(Load_R_R(L, H))),
        0x6D => Some(LoadInstr(Load_R_R(L, L))),
        0x6E => Some(LoadInstr(Load_R_addr_R2(L, HL))),
        0x6F => Some(LoadInstr(Load_R_R(L, A))),


        0x70 => Some(LoadInstr(Load_addr_R2_r(HL, B))),
        0x71 => Some(LoadInstr(Load_addr_R2_r(HL, C))),
        0x72 => Some(LoadInstr(Load_addr_R2_r(HL, D))),
        0x73 => Some(LoadInstr(Load_addr_R2_r(HL, E))),
        0x74 => Some(LoadInstr(Load_addr_R2_r(HL, H))),
        0x75 => Some(LoadInstr(Load_addr_R2_r(HL, L))),

        0x77 => Some(LoadInstr(Load_addr_R2_r(HL, A))),
        0x78 => Some(LoadInstr(Load_R_R(A, B))),
        0x79 => Some(LoadInstr(Load_R_R(A, C))),
        0x7A => Some(LoadInstr(Load_R_R(A, D))),
        0x7B => Some(LoadInstr(Load_R_R(A, E))),
        0x7C => Some(LoadInstr(Load_R_R(A, H))),
        0x7D => Some(LoadInstr(Load_R_R(A, L))),
        0x7E => Some(LoadInstr(Load_R_addr_R2(A, HL))),
        0x7F => Some(LoadInstr(Load_R_R(A, A))),


        // 8bit immediate value to register copy: LD r,n
        0x06 => Some(LoadInstr(Load_R_u8(B))),
        0x0E => Some(LoadInstr(Load_R_u8(C))),
        0x16 => Some(LoadInstr(Load_R_u8(D))),
        0x1E => Some(LoadInstr(Load_R_u8(E))),
        0x26 => Some(LoadInstr(Load_R_u8(H))),
        0x2E => Some(LoadInstr(Load_R_u8(L))),
        0x36 => Some(LoadInstr(Load_addr_R2_u8(HL))),
        0x3E => Some(LoadInstr(Load_R_u8(A))),

        0xE0 => Some(LoadInstr(Load_HI_U8_R(A))),

        0x01 => Some(LoadInstr(Load_R2_U16(BC))),
        0x11 => Some(LoadInstr(Load_R2_U16(DE))),
        0x21 => Some(LoadInstr(Load_R2_U16(HL))),
        0x31 => Some(LoadInstr(Load_R2_U16(SP))),
        0x08 => Some(LoadInstr(Load_addr_u16_R2(SP))),

        0x0a => Some(LoadInstr(Load_R_addr_R2(A, BC))),
        0x1a => Some(LoadInstr(Load_R_addr_R2(A, DE))),
        // put value pointed to by DE into A

        0x22 => Some(LoadInstr(Load_addr_R2_A_inc(HL))),
        0x32 => Some(LoadInstr(Load_addr_R2_A_dec(HL))),
        0x2A => Some(LoadInstr(Load_A_addr_R2_inc(HL))),

        0x02 => Some(LoadInstr(Load_addr_R2_r(BC, A))),
        0x12 => Some(LoadInstr(Load_addr_R2_r(DE, A))),

        0xe2 => Some(LoadInstr(Load_HI_R_R(C, A))),
        0xea => Some(LoadInstr(Load_addr_u16_A())),
        0xFA => Some(LoadInstr(Load_A_addr_u16())),
        0xF2 => Some(LoadInstr(Load_R_HI_R(A,C))),


        0x00 => Some(SpecialInstr(NOOP())),
        0xF3 => Some(SpecialInstr(DisableInterrupts())),
        0xFB => Some(SpecialInstr(EnableInterrupts())),
        0x10 => Some(SpecialInstr(STOP())),
        0x76 => Some(SpecialInstr(HALT())),
        0xCD => Some(SpecialInstr(CALL_u16())),
        0xC1 => Some(SpecialInstr(POP(BC))),
        0xC5 => Some(SpecialInstr(PUSH(BC))),
        0xD1 => Some(SpecialInstr(POP(DE))),
        0xD5 => Some(SpecialInstr(PUSH(DE))),
        0xE1 => Some(SpecialInstr(POP(HL))),
        0xE5 => Some(SpecialInstr(PUSH(HL))),
        0xF1 => Some(SpecialInstr(POP(AF))),
        0xF5 => Some(SpecialInstr(PUSH(AF))),

        0xC8 => Some(SpecialInstr(RETZ())),
        0xC9 => Some(SpecialInstr(RET())),
        0xD9 => Some(SpecialInstr(RETI())),

        0xC7 => Some(SpecialInstr(RST(0x00))),
        0xD7 => Some(SpecialInstr(RST(0x10))),
        0xE7 => Some(SpecialInstr(RST(0x20))),
        0xF7 => Some(SpecialInstr(RST(0x30))),
        0xCF => Some(SpecialInstr(RST(0x08))),
        0xDF => Some(SpecialInstr(RST(0x18))),
        0xEF => Some(SpecialInstr(RST(0x28))),
        0xFF => Some(SpecialInstr(RST(0x38))),

        0xF0 => Some(LoadInstr(Load_HI_R_U8(A))),

        0x18 => Some(JumpInstr(Relative_i8())), //2 bytes, no flags, relative signed
        0x20 => Some(JumpInstr(Relative_cond_notzero_i8())),
        0x28 => Some(JumpInstr(Relative_cond_zero_i8())), //2 bytes, if zero, relative signed
        0x30 => Some(JumpInstr(Relative_cond_notcarry_i8())), //2 bytes, if not carry, realtive signed
        0x38 => Some(JumpInstr(Relative_cond_carry_i8())), //2 bytes, if carry, relative signed
        0xC2 => Some(JumpInstr(Absolute_cond_notzero_u16())),
        0xC3 => Some(JumpInstr(Absolute_u16())),
        0xFE => Some(CompareInst(CP_A_n())),


        0x39 => Some(MathInst(ADD_RR_RR(HL, SP))),

        0x80 => Some(MathInst(ADD_R_R(A, B))),
        0x81 => Some(MathInst(ADD_R_R(A, C))),
        0x82 => Some(MathInst(ADD_R_R(A, D))),
        0x83 => Some(MathInst(ADD_R_R(A, E))),
        0x84 => Some(MathInst(ADD_R_R(A, H))),
        0x85 => Some(MathInst(ADD_R_R(A, L))),
        0x86 => Some(MathInst(ADD_A_addr(HL))),
        0x87 => Some(MathInst(ADD_R_R(A, A))),

        0x90 => Some(MathInst(SUB_R_R(A, B))),
        0x91 => Some(MathInst(SUB_R_R(A, C))),
        0x92 => Some(MathInst(SUB_R_R(A, D))),
        0x93 => Some(MathInst(SUB_R_R(A, E))),
        0x94 => Some(MathInst(SUB_R_R(A, H))),
        0x95 => Some(MathInst(SUB_R_R(A, L))),
        0x96 => Some(MathInst(SUB_A_addr(HL))),
        0x97 => Some(MathInst(SUB_R_R(A, A))),

        0xA0 => Some(MathInst(AND_A_r(B))),
        0xA1 => Some(MathInst(AND_A_r(C))),
        0xA2 => Some(MathInst(AND_A_r(D))),
        0xA3 => Some(MathInst(AND_A_r(E))),
        0xA4 => Some(MathInst(AND_A_r(H))),
        0xA5 => Some(MathInst(AND_A_r(L))),
        0xA7 => Some(MathInst(AND_A_r(A))),
        0xA6 => Some(MathInst(AND_A_addr(HL))),
        0xE6 => Some(MathInst(AND_A_u8())),

        0xA8 => Some(MathInst(XOR_A_r(B))),
        0xA9 => Some(MathInst(XOR_A_r(C))),
        0xAA => Some(MathInst(XOR_A_r(D))),
        0xAB => Some(MathInst(XOR_A_r(E))),
        0xAC => Some(MathInst(XOR_A_r(H))),
        0xAD => Some(MathInst(XOR_A_r(L))),
        0xAE => Some(MathInst(XOR_A_addr(HL))),
        0xAF => Some(MathInst(XOR_A_r(A))),
        0xEE => Some(MathInst(XOR_A_u8())),

        0xB0 => Some(MathInst(OR_A_r(B))),
        0xB1 => Some(MathInst(OR_A_r(C))),
        0xB2 => Some(MathInst(OR_A_r(D))),
        0xB3 => Some(MathInst(OR_A_r(E))),
        0xB4 => Some(MathInst(OR_A_r(H))),
        0xB5 => Some(MathInst(OR_A_r(L))),
        0xB6 => Some(MathInst(OR_A_addr(HL))),
        0xB7 => Some(MathInst(OR_A_r(A))),

        0xB8 => Some(CompareInst(CP_A_r(B))),
        0xB9 => Some(CompareInst(CP_A_r(C))),
        0xBA => Some(CompareInst(CP_A_r(D))),
        0xBB => Some(CompareInst(CP_A_r(E))),
        0xBC => Some(CompareInst(CP_A_r(H))),
        0xBD => Some(CompareInst(CP_A_r(L))),
        0xBE => Some(CompareInst(CP_A_addr(HL))),

        0xC6 => Some(MathInst(ADD_R_u8(A))),



        //increments and decrements
        0x04 => Some(MathInst(Inc_r(B))),
        0x14 => Some(MathInst(Inc_r(D))),
        0x24 => Some(MathInst(Inc_r(H))),
        0x05 => Some(MathInst(Dec_r(B))),
        0x15 => Some(MathInst(Dec_r(D))),
        0x25 => Some(MathInst(Dec_r(H))),

        0x0c => Some(MathInst(Inc_r(C))),
        0x1c => Some(MathInst(Inc_r(E))),
        0x2c => Some(MathInst(Inc_r(L))),
        0x3c => Some(MathInst(Inc_r(A))),

        0x0d => Some(MathInst(Dec_r(C))),
        0x1d => Some(MathInst(Dec_r(E))),
        0x2d => Some(MathInst(Dec_r(L))),
        0x3d => Some(MathInst(Dec_r(A))),

        0x03 => Some(MathInst(Inc_rr(BC))),
        0x13 => Some(MathInst(Inc_rr(DE))),
        0x23 => Some(MathInst(Inc_rr(HL))),
        0x33 => Some(MathInst(Inc_rr(SP))),

        0x0B => Some(MathInst(Dec_rr(BC))),
        0x1B => Some(MathInst(Dec_rr(DE))),
        0x2B => Some(MathInst(Dec_rr(HL))),
        0x3B => Some(MathInst(Dec_rr(SP))),


        // bit manipulation
        0xCB38 => Some(MathInst(SLA(B))),
        0xCB7C => Some(MathInst(BIT(7, H))),
        0xCB7E => Some(MathInst(BITR2(7, HL))),
        0xCB19 => Some(MathInst(RR(C))),
        0xCB1A => Some(MathInst(RR(D))),
        0xCB1B => Some(MathInst(RR(E))),
        0xCB1C => Some(MathInst(RR(H))),
        0xCB1D => Some(MathInst(RR(L))),
        0xCB1F => Some(MathInst(RR(A))),

        0x07 => Some(MathInst(RLCA())),
        0x17 => Some(MathInst(RLA())),
        0x0F => Some(MathInst(RRCA())),
        0x1F => Some(MathInst(RRA())),

        0xCB00 => Some(MathInst(RLC(B))),
        0xCB01 => Some(MathInst(RLC(C))),
        0xCB02 => Some(MathInst(RLC(D))),
        0xCB03 => Some(MathInst(RLC(E))),
        0xCB04 => Some(MathInst(RLC(H))),
        0xCB05 => Some(MathInst(RLC(L))),
        0xCB07 => Some(MathInst(RLC(A))),

        0xCB08 => Some(MathInst(RRC(B))),
        0xCB09 => Some(MathInst(RRC(C))),
        0xCB0A => Some(MathInst(RRC(D))),
        0xCB0B => Some(MathInst(RRC(E))),
        0xCB0C => Some(MathInst(RRC(H))),
        0xCB0D => Some(MathInst(RRC(L))),
        0xCB0F => Some(MathInst(RRC(A))),

        0xCB11 => Some(MathInst(RL(C))),



        _ => {
            println!("WARNING. can't lookup opcode {:04x}",code);
            None
        }
    }
}

pub fn lookup_opcode_info(op: Instr) -> String {
    match op {
        LoadInstr(Load_R_u8(r)) => format!("LD {},n -- Load register from immediate u8", r),
        LoadInstr(Load_HI_R_U8(r)) => format!("LDH {},(n) -- Load High: put contents of 0xFF00 + u8 into register {}", r, r),
        LoadInstr(Load_HI_U8_R(r)) => format!("LDH (n),{} -- Load High at u8 address with contents of {}", r, r),
        LoadInstr(Load_HI_R_R(off, src)) => format!("LD (FF00 + {}),{}  -- Load High: put contents of register {} into memory of 0xFF00 + {} ", off, src, src, off),
        LoadInstr(Load_R_HI_R(dst, off)) => format!("LD {}, (FF00 + {}) -- Load High: put contents of memory at 0xFF00 + {} into register {}", dst, off, off, dst),
        LoadInstr(Load_R2_U16(rr)) => format!("LD {} u16 -- Load immediate u16 into register {}", rr, rr),
        LoadInstr(Load_R_addr_R2(r, rr)) => format!("LD {}, ({}) -- load data pointed to by {} into {}", r, rr, rr, r),
        LoadInstr(Load_addr_R2_u8(rr)) => format!("LD ({}),u8 -- load immediate value to memory pointed at by {}",rr,rr),
        LoadInstr(Load_addr_R2_r(rr, r)) => format!("LD ({}),{} -- load contents of {} into memory pointed to by {}", rr, r, r, rr),
        LoadInstr(Load_addr_R2_A_inc(rr)) => format!("LD ({}+), A -- load contents of A into memory pointed to by {}, then increment {}", rr, rr, rr),
        LoadInstr(Load_addr_R2_A_dec(rr)) => format!("LD ({}-), A -- load contents of A into memory pointed to by {}, then decrement {}", rr, rr, rr),
        LoadInstr(Load_A_addr_R2_inc(rr)) => format!("LD A (HL+) -- load contents of memory pointed to by {} into A, then increment {}", rr, rr),
        LoadInstr(Load_R_R(dst, src)) => format!("LD {},{} -- copy {} to {}", dst, src, src, dst),
        LoadInstr(Load_addr_u16_A()) => format!("LD (nn),A -- load A into memory at address from immediate u16"),
        LoadInstr(Load_A_addr_u16()) => format!("LD A (nn) -- Load contents of memory at address from immediate u16 into A"),
        LoadInstr(Load_addr_u16_R2(rr)) => format!("LD (nn),{} -- load {} into memory at address from immediate u16", rr, rr),

        JumpInstr(Absolute_u16()) => format!("JP nn -- Jump unconditionally to absolute address"),
        JumpInstr(Relative_i8()) => format!("JR e --   Jump relative to signed offset"),
        JumpInstr(Relative_cond_carry_i8()) => format!("JR C,e -- Jump relative if Carry Flag set"),
        JumpInstr(Relative_cond_notcarry_i8()) => format!("JR NC,e -- Jump relative if not Carry flag set"),
        JumpInstr(Relative_cond_zero_i8()) => format!("JR Z,e -- Jump relative if Zero flag set"),
        JumpInstr(Relative_cond_notzero_i8()) => format!("JR NZ,e -- Jump relative if not Zero flag set"),
        JumpInstr(Absolute_cond_notzero_u16()) => format!("JR NZ,u16 -- Jump absolute if not Zero flag set"),
        CompareInst(CP_A_n()) => format!("CP A,n  -- Compare A with u8 n. sets flags"),
        CompareInst(CP_A_r(r)) => format!("CP A,{} -- Compare A with {}. sets flags", r, r),
        CompareInst(CP_A_addr(r)) => format!("CP A, ({}) -- Compare A with ({}). sets flags", r, r),

        MathInst(XOR_A_r(r)) => format!("XOR A, {}  -- Xor A with {}, store in A", r, r),
        MathInst(XOR_A_u8()) => format!("XOR A,u8  -- Xor A with immediate u8 store in A"),
        MathInst(XOR_A_addr(rr)) => format!("XOR A,(HL) {} -- XOR A with memory at address inside {}", rr, rr),
        MathInst(OR_A_r(r))  => format!("OR A, {}   -- OR A with {}, store in A", r, r),
        MathInst(OR_A_addr(rr)) => format!("OR A, ({}) -- OR A with contents of memory at {} ",rr,rr),
        MathInst(AND_A_r(r)) => format!("AND A, {}  -- AND A with {}, store in A", r, r),
        MathInst(AND_A_u8()) => format!("AND A, u8  -- AND A with immediate u8, store in A"),
        MathInst(AND_A_addr(rr)) => format!("AND A, ({}) -- AND A with contents of memory at {} ",rr,rr),
        MathInst(ADD_R_u8(r)) => format!("ADD {} u8 -- add immediate u8 to register {}", r, r),
        MathInst(ADD_R_R(dst, src)) => format!("ADD {} {} -- add {} to {}, store result in {}", dst, src, src, dst, dst),
        MathInst(ADD_RR_RR(dst, src)) => format!("ADD {} {}", dst, src),
        MathInst(ADD_A_addr(rr)) => format!("ADD A, ({}) -- ADD A with contents of memory at {}",rr,rr),
        MathInst(SUB_R_R(dst, src)) => format!("SUB {} {} -- subtract {} from {}, store result in {}", dst, src, src, dst, dst),
        MathInst(SUB_A_addr(rr)) => format!("SUB A, ({}) -- SUB A with contents of memory at {}",rr,rr),

        MathInst(Inc_rr(rr)) => format!("INC {} -- Increment register {}", rr, rr),
        MathInst(Dec_rr(rr)) => format!("DEC {} -- Decrement register {}", rr, rr),
        MathInst(Inc_r(r)) => format!("INC {} -- Increment register {}. sets flags", r, r),
        MathInst(Dec_r(r)) => format!("DEC {} -- Decrement register {}. sets flags", r, r),

        MathInst(BIT(bit, r)) => format!("BIT {:0x}, {}", bit, r),
        MathInst(BITR2(bit, r)) => format!("BIT {:0x}, ({})", bit, r),
        MathInst(RLC(r)) => format!("RLC {} --- rotate register {} .old bit 7 to carry flag. sets flags", r, r),
        MathInst(RL(r)) => format!("RL {} -- Rotate {} left through Carry flag. sets flags", r, r),
        MathInst(RRC(r)) => format!("RRC {} -- Rotate {} right, Old bit 0 to carry flag. sets flags.", r, r),
        MathInst(RR(r)) => format!("RR {} -- Rotate {} right through carry flag. sets flags", r, r),
        MathInst(RLA()) => format!("RLA -- rotate A left through carry flag. Same as RL A"),
        MathInst(SLA(rr)) => format!("SLA {} -- shift A left through carry flag by {} bits", rr, rr),
        MathInst(RLCA()) => format!("RLCA -- rotate A left. same as RLC A "),
        MathInst(RRA()) => format!("RRA -- Rotate A right. Same as RR A"),
        MathInst(RRCA()) => format!("RRCA -- Rotate A right, Same as RRC A"),

        SpecialInstr(DisableInterrupts()) => format!("DI -- disable interrupts"),
        SpecialInstr(EnableInterrupts()) => format!("EI -- enable interrupts"),
        SpecialInstr(NOOP()) => format!("NOOP -- do nothing"),
        SpecialInstr(STOP()) => format!("STOP -- stop interrupts?"),
        SpecialInstr(CALL_u16()) => format!("CALL u16 -- save next addr to the stack, then jump to the specified address"),
        SpecialInstr(PUSH(rr)) => format!("PUSH {} -- push contents of register {} to the stack", rr, rr),
        SpecialInstr(POP(rr)) => format!("POP {} -- pop off stack, back to register {}", rr, rr),
        SpecialInstr(RET()) => format!("RET -- pop two bytes from the stack and jump to that address"),
        SpecialInstr(RETI()) => format!("RET -- pop two bytes from the stack and jump to that address, plus enable interrupts"),
        SpecialInstr(RST(h)) => format!("RST {:02x} -- put present address onto stack, jump to address {:02x}", h, h),
        SpecialInstr(RETZ()) => format!("RET Z  -- return of zflag is set"),
        SpecialInstr(HALT()) => format!("HALT -- completely stop the emulator"),
    }
}

pub fn execute_special_instructions(cpu:&mut Z80, mmu:&mut MMU, special: &Special) {
    match special {
        Special::DisableInterrupts() => {
            cpu.inc_pc();
            mmu.hardware.IME = 0;
        }
        Special::EnableInterrupts() => {
            cpu.inc_pc();
            mmu.hardware.IME = 1;
        }
        Special::NOOP() => {
            cpu.inc_pc();
        }
        Special::HALT() => {
            cpu.inc_pc();
            // self.running = false;
        }
        Special::STOP() => {
            cpu.inc_pc();
            mmu.hardware.IME = 0;
        }
        Special::CALL_u16() => {
            cpu.inc_pc();
            let addr = mmu.read16(cpu.get_pc());
            cpu.inc_pc();
            cpu.inc_pc();
            cpu.dec_sp();
            cpu.dec_sp();
            mmu.write16(cpu.get_sp(), cpu.get_pc());
            cpu.set_pc(addr);
        }
        Special::PUSH(rr) => {
            cpu.inc_pc();
            cpu.dec_sp();
            cpu.dec_sp();
            let value = cpu.r.get_u16reg(rr);
            // println!("pushing {:04x}",value);
            mmu.write16(cpu.r.sp, value);
        }
        Special::POP(rr) => {
            cpu.inc_pc();
            let value = mmu.read16(cpu.r.sp);
            cpu.inc_sp();
            cpu.inc_sp();
            // println!("popped {:04x}",value);
            // println!("sp is {:04x}",self.cpu.r.sp);
            cpu.r.set_u16reg(rr,value);
        }
        Special::RET() => {
            cpu.inc_pc();
            let addr = mmu.read16(cpu.r.sp);
            cpu.inc_sp();
            cpu.inc_sp();
            cpu.set_pc(addr);
        }
        Special::RETI() => {
            cpu.inc_pc();
            let addr = mmu.read16(cpu.r.sp);
            cpu.inc_sp();
            cpu.inc_sp();
            cpu.set_pc(addr);
            mmu.hardware.IME = 1;
            // println!("returned from interrupt handler. going back to {:04x}",self.cpu.get_pc());
        }
        Special::RETZ() => {
            cpu.inc_pc();
            if cpu.r.zero_flag {
                let addr = mmu.read16(cpu.r.sp);
                cpu.inc_sp();
                cpu.inc_sp();
                cpu.set_pc(addr);
            }
        }
        Special::RST(h) => {
            cpu.inc_pc();
            mmu.write16(cpu.r.sp, cpu.r.pc);
            cpu.set_pc((0x0000 | h) as u16);
        }
    }
}

pub fn execute_load_instructions(cpu: &mut Z80, mmu: &mut MMU, load: &Load) {
    match load {
        Load_R_u8(r) => {
            // load immediate u8 into the register
            cpu.inc_pc();
            let val = mmu.read8(cpu.r.pc);
            cpu.r.set_u8reg(r, val);
            cpu.inc_pc();
        }
        // put the memory address 0xFF00 + n into register r
        Load::Load_HI_R_U8(r) => {
            // println!("running LDH");
            let n = mmu.read8(cpu.r.pc+1);
            let addr = 0xFF00 + (n as u16);
            cpu.r.set_u8reg(&r,mmu.read8(addr));
            // println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
            cpu.inc_pc();
            cpu.inc_pc();
        }
        Load::Load_HI_U8_R(r) => {
            cpu.inc_pc();
            let e = mmu.read8(cpu.r.pc);
            cpu.inc_pc();
            let addr = 0xFF00 + (e as u16);
            let val = cpu.r.get_u8reg(r);
            mmu.write8(addr, val);
        },
        //LD (FF00+C),A    put contents of A into address 0xF00+C
        Load::Load_HI_R_R(off, r) => {
            cpu.inc_pc();
            let v = cpu.r.get_u8reg(r);
            let addr = 0xFF00 + (cpu.r.get_u8reg(off) as u16);
            // println!("copying value x{:02x} from register {} to address ${:04x}", v, r, addr);
            mmu.write8(addr,v);
            cpu.inc_pc();
        }
        Load::Load_R_HI_R(dst, src) => {
            cpu.inc_pc();
            let addr = 0xFF00 + (cpu.r.get_u8reg(src) as u16);
            let v = mmu.read8(addr);
            cpu.r.set_u8reg(dst,v);
            cpu.inc_pc();
        }
        Load::Load_R_R(dst, src) => {
            cpu.inc_pc();
            let val = cpu.r.get_u8reg(src);
            cpu.r.set_u8reg(dst,val);
        }

        Load::Load_R2_U16(rr) => {
            cpu.inc_pc();
            let val = mmu.read16(cpu.get_pc());
            cpu.inc_pc();
            cpu.inc_pc();
            cpu.r.set_u16reg(rr,val);
            // println!("copied immediate value {:04x} into register {}",val,rr)
        }
        Load::Load_R_addr_R2(r, rr) => {
            // load to the 8bit register A, data from the address in the 16 bit register
            cpu.inc_pc();
            let addr = cpu.r.get_u16reg(rr);
            let val = mmu.read8(addr);
            cpu.r.set_u8reg(r,val);
            // println!("copied value {:02x} from address {:04x} determined from register {} into register A",val,addr,rr);
        }
        Load::Load_A_addr_R2_inc(rr) => {
            // load to the 8bit register A, data from the address in the 16 bit register, then increment that register
            cpu.inc_pc();
            let addr = cpu.r.get_u16reg(rr);
            let val = mmu.read8(addr);
            cpu.r.set_u8reg(&A,val);
            // println!("copied value {:02x} from address {:04x} determined from register {} into register A",val,addr,rr);
            let (v2,_bool) = cpu.r.get_u16reg(rr).overflowing_add(1);
            cpu.r.set_u16reg(rr,v2);
        },
        Load::Load_addr_R2_A_inc(rr) => {
            cpu.inc_pc();
            let val = cpu.r.get_u8reg(&A);
            let addr = cpu.r.get_u16reg(rr);
            mmu.write8(addr,val);
            cpu.r.set_hl(cpu.r.get_hl()+1);
        }
        Load::Load_addr_R2_A_dec(rr) => {
            cpu.inc_pc();
            let val = cpu.r.get_u8reg(&A);
            let addr = cpu.r.get_u16reg(rr);
            mmu.write8(addr,val);
            cpu.r.set_hl(cpu.r.get_hl()-1);
        }

        Load::Load_addr_R2_r(rr, r) => {
            //copy contents of R to memory pointed to by RR
            cpu.inc_pc();
            let val = cpu.r.get_u8reg(r);
            let addr = cpu.r.get_u16reg(rr);
            mmu.write8(addr,val);
        }
        Load::Load_addr_R2_u8(rr) => {
            cpu.inc_pc();
            let val = mmu.read8(cpu.r.pc);
            cpu.inc_pc();
            let addr = cpu.r.get_u16reg(rr);
            mmu.write8(addr,val);
        }

        Load::Load_addr_u16_A() => {
            cpu.inc_pc();
            let addr = mmu.read16(cpu.r.pc);
            let val = cpu.r.get_u8reg(&A);
            mmu.write8(addr,val);
            cpu.inc_pc();
            cpu.inc_pc();
        }
        Load::Load_A_addr_u16() => {
            cpu.inc_pc();
            let addr = mmu.read16(cpu.r.pc);
            cpu.inc_pc();
            cpu.inc_pc();
            let val = mmu.read8(addr);
            cpu.r.set_u8reg(&A,val);
        }

        Load::Load_addr_u16_R2(rr) => {
            cpu.inc_pc();
            let addr = mmu.read16(cpu.r.pc);
            let val = cpu.r.get_u16reg(rr);
            mmu.write16(addr,val);
            cpu.inc_pc();
            cpu.inc_pc();
        }
    }
}

pub fn execute_compare_instructions(cpu:&mut Z80, mmu:&mut MMU, comp:&Compare) {
    match comp {
        Compare::CP_A_n() => {
            cpu.inc_pc();
            let src_v = mmu.read8(cpu.r.pc);
            cpu.inc_pc();

            let dst_v = cpu.r.get_u8reg(&A);
            let result = src_v.wrapping_sub(dst_v);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
            cpu.r.subtract_n_flag = true;
            cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
        }
        Compare::CP_A_r(r) => {
            cpu.inc_pc();
            let src_v = cpu.r.get_u8reg(r);
            let dst_v = cpu.r.get_u8reg(&A);
            let result = dst_v.wrapping_sub(src_v);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
            cpu.r.subtract_n_flag = true;
            cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
        }
        Compare::CP_A_addr(r) => {
            cpu.inc_pc();
            let addr = cpu.r.get_u16reg(r);
            let src_v = mmu.read8(addr);
            let dst_v = cpu.r.get_u8reg(&A);
            let result = dst_v.wrapping_sub(src_v);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
            cpu.r.subtract_n_flag = true;
            cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
        }
    }
}

pub fn execute_math_instructions(cpu:&mut Z80, mmu:&mut MMU, math: &Math) {
    match math {
        Math::XOR_A_r(r) => {
            cpu.inc_pc();
            let res = cpu.r.get_u8reg(&A) ^ cpu.r.get_u8reg(r);
            cpu.r.zero_flag = res == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = false;
            cpu.r.carry_flag = false;
            cpu.r.set_u8reg(&A,res);
        }
        Math::XOR_A_u8() => {
            cpu.inc_pc();
            let n = mmu.read8(cpu.r.pc);
            let res = cpu.r.get_u8reg(&A) ^ n;
            cpu.r.zero_flag = res == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = false;
            cpu.r.carry_flag = false;
            cpu.r.set_u8reg(&A,res);
        }
        Math::XOR_A_addr(rr) => {
            cpu.inc_pc();
            let addr = cpu.r.get_u16reg(rr);
            let val = mmu.read8(addr);
            let res = cpu.r.get_u8reg(&A) ^ val;
            cpu.r.zero_flag = res == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = false;
            cpu.r.carry_flag = false;
            cpu.r.set_u8reg(&A,res);
        }
        Math::OR_A_r(r) => {
            cpu.inc_pc();
            cpu.r.set_u8reg(&A, cpu.r.get_u8reg(&A) | cpu.r.get_u8reg(r));
            cpu.r.zero_flag = cpu.r.get_u8reg(&A) == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = false;
            cpu.r.carry_flag = false;
        }
        Math::OR_A_addr(rr) => {
            cpu.inc_pc();
            let addr = cpu.r.get_u16reg(rr);
            let val = mmu.read8(addr);
            cpu.r.set_u8reg(&A, cpu.r.get_u8reg(&A) | val);
            cpu.r.zero_flag = cpu.r.get_u8reg(&A) == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = false;
            cpu.r.carry_flag = false;
        }
        Math::AND_A_r(r) => {
            cpu.inc_pc();
            cpu.r.set_u8reg(&A, cpu.r.get_u8reg(&A) & cpu.r.get_u8reg(r));
            cpu.r.zero_flag = cpu.r.get_u8reg(&A) == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = true;
            cpu.r.carry_flag = false;
        }
        Math::AND_A_u8() => {
            cpu.inc_pc();
            let n = mmu.read8(cpu.r.pc);
            cpu.inc_pc();
            cpu.r.set_u8reg(&A, cpu.r.get_u8reg(&A) & n);
            cpu.r.zero_flag = cpu.r.get_u8reg(&A) == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = true;
            cpu.r.carry_flag = false;
        }
        Math::AND_A_addr(rr) => {
            cpu.inc_pc();
            let addr = cpu.r.get_u16reg(rr);
            let val = mmu.read8(addr);
            cpu.r.set_u8reg(&A, cpu.r.get_u8reg(&A) & val);
            cpu.r.zero_flag = cpu.r.get_u8reg(&A) == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = true;
            cpu.r.carry_flag = false;
        }
        Math::ADD_R_u8(r) => {
            cpu.inc_pc();
            let v1 = cpu.r.get_u8reg(r);
            let v2 = mmu.read8(cpu.r.pc);
            let result = v1.wrapping_add(v2);
            cpu.inc_pc();
            cpu.r.set_u8reg(r, v2);
            cpu.r.zero_flag = result == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = (v1 & 0xF) + (v2 & 0xF) > 0xFF;
            cpu.r.carry_flag = (v1 as u16) + (v2 as u16) > 0xFF;
        }
        Math::ADD_R_R(dst, src) => {
            cpu.inc_pc();
            let dst_v = cpu.r.get_u8reg(dst);
            let src_v = cpu.r.get_u8reg(src);
            let result = dst_v.wrapping_add(src_v);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (dst_v & 0x0F) + (src_v & 0x0f) > 0xF;
            cpu.r.subtract_n_flag = false;
            cpu.r.carry_flag = (dst_v as u16) + (src_v as u16) > 0xFF;
            cpu.r.set_u8reg(dst, result);
        }
        Math::ADD_RR_RR(dst,src) => {
            cpu.inc_pc();
            let src_v = cpu.r.get_u16reg(src);
            let dst_v = cpu.r.get_u16reg(dst);
            let result = dst_v.wrapping_add(src_v);
            cpu.r.subtract_n_flag = false;
            //dont modify the zero flag
            cpu.r.half_flag = (dst_v & 0x07FF) + (src_v & 0x07FF) > 0x07FF;
            cpu.r.carry_flag = (dst_v) > (0xFFFF - src_v);
            cpu.r.set_u16reg(dst,result);
        }
        Math::ADD_A_addr(rr) => {
            cpu.inc_pc();
            let dst_v = cpu.r.get_u8reg(&A);
            let addr = cpu.r.get_u16reg(rr);
            let src_v = mmu.read8(addr);
            let result = dst_v.wrapping_add(src_v);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (dst_v & 0x0F) + (src_v & 0x0f) > 0xF;
            cpu.r.subtract_n_flag = false;
            cpu.r.carry_flag = (dst_v as u16) + (src_v as u16) > 0xFF;
            cpu.r.set_u8reg(&A, result);
        }
        Math::SUB_R_R(dst, src) => {
            cpu.inc_pc();
            let dst_v = cpu.r.get_u8reg(dst);
            let src_v = cpu.r.get_u8reg(src);
            let result = dst_v.wrapping_sub(src_v);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
            cpu.r.subtract_n_flag = true;
            cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
            cpu.r.set_u8reg(dst, result);
        }
        Math::SUB_A_addr(rr) => {
            cpu.inc_pc();
            let dst_v = cpu.r.get_u8reg(&A);
            let addr = cpu.r.get_u16reg(rr);
            let src_v = mmu.read8(addr);
            let result = dst_v.wrapping_sub(src_v);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
            cpu.r.subtract_n_flag = true;
            cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
            cpu.r.set_u8reg(&A, result);
        }
        Math::Inc_r(dst_r) => {
            cpu.inc_pc();
            let src_v = cpu.r.get_u8reg(dst_r);
            let result = src_v.wrapping_add(1);
            cpu.r.set_u8reg(dst_r, result);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (result & 0x0F) + 1 > 0x0F;
            cpu.r.subtract_n_flag = false;
        }
        Math::Dec_r(r) => {
            cpu.inc_pc();
            let val = cpu.r.get_u8reg(r);
            let result = val.wrapping_sub(1);
            cpu.r.set_u8reg(r, result);
            cpu.r.zero_flag = result == 0;
            cpu.r.half_flag = (result & 0x0F) == 0;
            cpu.r.subtract_n_flag = true;
        }
        Math::Inc_rr(rr) => {
            cpu.inc_pc();
            let val = cpu.r.get_u16reg(rr);
            let result = val.wrapping_add(1);
            cpu.r.set_u16reg(rr, result);
        }
        Math::Dec_rr(rr) => {
            cpu.inc_pc();
            let mut val = cpu.r.get_u16reg(rr);
            val -= 1;
            cpu.r.set_u16reg(rr, val);
        }
        Math::BIT(b,r) => {
            cpu.inc_pc();
            let val = cpu.r.get_u8reg(r);
            cpu.inc_pc();
            cpu.r.zero_flag = !get_bit_as_bool(val, *b);
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = true;
        }
        Math::BITR2(b,r) => {
            cpu.inc_pc();
            let addr = cpu.r.get_u16reg(r);
            let val = mmu.read16(addr);
            let mask = match b {
                0 => 0b0000_0001,
                1 => 0b0000_0010,
                2 => 0b0000_0100,
                3 => 0b0000_1000,
                4 => 0b0001_0000,
                5 => 0b0010_0000,
                6 => 0b0100_0000,
                7 => 0b1000_0000,
                _ => {
                    panic!("we can't look at bits higher than 7")
                }
            };
            cpu.r.zero_flag = !((val & mask) > 0);
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = true;
        }
        Math::RLC(r) => {
            cpu.inc_pc();
            let r_val = cpu.r.get_u8reg(r);
            let carry_flag = r_val & 0x80 == 0x80;
            let f_val = (r_val << 1) | if carry_flag { 1 } else { 0 };
            cpu.r.half_flag = false;
            cpu.r.subtract_n_flag = false;
            cpu.r.zero_flag = f_val == 0;
            cpu.r.carry_flag = carry_flag;
            cpu.r.set_u8reg(r,f_val);
            cpu.inc_pc();
        }
        Math::RLCA() => {
            cpu.inc_pc();
            let r_val = cpu.r.get_u8reg(&A);
            let carry_flag = r_val & 0x80 == 0x80;
            let f_val = (r_val << 1) | if carry_flag { 1 } else { 0 };
            cpu.r.half_flag = false;
            cpu.r.subtract_n_flag = false;
            cpu.r.zero_flag = f_val == 0;
            cpu.r.carry_flag = carry_flag;
            cpu.r.set_u8reg(&A,f_val);
        }
        Math::RL(r) => {
            cpu.inc_pc();
            let r_val = cpu.r.get_u8reg(r);
            let carry_flag = r_val & 0x80 == 0x80;
            let f_val = (r_val << 1) | (if cpu.r.carry_flag { 1 } else { 0 });
            cpu.r.half_flag = false;
            cpu.r.subtract_n_flag = false;
            cpu.r.zero_flag = f_val == 0;
            cpu.r.carry_flag = carry_flag;
            cpu.r.set_u8reg(r, f_val);
            cpu.inc_pc();
        }
        Math::RLA() => {
            cpu.inc_pc();
            let r_val = cpu.r.get_u8reg(&A);
            let carry_flag = r_val & 0x80 == 0x80;
            let f_val = (r_val << 1) | (if cpu.r.carry_flag { 1 } else { 0 });
            cpu.r.half_flag = false;
            cpu.r.subtract_n_flag = false;
            cpu.r.zero_flag = f_val == 0;
            cpu.r.carry_flag = carry_flag;
            cpu.r.set_u8reg(&A, f_val);
        }
        Math::RRC(r) => {
            cpu.inc_pc();
            let r_val = cpu.r.get_u8reg(r);
            let carry_flag = r_val & 0x01 == 0x01;
            let f_val = (r_val >> 1) | (if carry_flag {0x80} else { 0x00 });
            cpu.r.half_flag = false;
            cpu.r.subtract_n_flag = false;
            cpu.r.zero_flag = f_val == 0;
            cpu.r.carry_flag = carry_flag;
            cpu.r.set_u8reg(r, f_val);
            cpu.inc_pc();
        }
        Math::RRCA() => {
            cpu.inc_pc();
            let r_val = cpu.r.get_u8reg(&A);
            let carry_flag = r_val & 0x01 == 0x01;
            let f_val = (r_val >> 1) | (if carry_flag {0x80} else { 0x00 });
            cpu.r.half_flag = false;
            cpu.r.subtract_n_flag = false;
            cpu.r.zero_flag = f_val == 0;
            cpu.r.carry_flag = carry_flag;
            cpu.r.set_u8reg(&A, f_val);
        }
        Math::RR(r) => {
            cpu.inc_pc();
            let r_val = cpu.r.get_u8reg(r);
            let carry_flag = r_val & 0x01 == 0x01;
            let f_val = (r_val >> 1) | (if cpu.r.carry_flag { 0x80 } else { 0x00 });
            cpu.r.half_flag = false;
            cpu.r.subtract_n_flag = false;
            cpu.r.zero_flag = f_val == 0;
            cpu.r.carry_flag = carry_flag;
            cpu.r.set_u8reg(r, f_val);
            cpu.inc_pc();
        }
        Math::RRA() => {
            cpu.inc_pc();
            let r_val = cpu.r.get_u8reg(&A);
            let carry_flag = r_val & 0x01 == 0x01;
            let f_val = (r_val >> 1) | (if cpu.r.carry_flag { 0x80 } else { 0x00 });
            cpu.r.half_flag = false;
            cpu.r.subtract_n_flag = false;
            cpu.r.zero_flag = f_val == 0;
            cpu.r.carry_flag = carry_flag;
            cpu.r.set_u8reg(&A, f_val);
        }
        Math::SLA(r) => {
            cpu.inc_pc();
            let n = cpu.r.get_u8reg(r);
            let v = cpu.r.get_u8reg(&A);
            let (v2,carry) = v.overflowing_shl(n as u32);
            cpu.r.set_u8reg(&A,v2);
            cpu.r.zero_flag = v2 == 0;
            cpu.r.subtract_n_flag = false;
            cpu.r.half_flag = false;
            cpu.r.carry_flag = carry;
            cpu.inc_pc();
        }
    }
}

pub fn execute_jump_instructions(cpu:&mut Z80, mmu:&mut MMU, jump: &Jump) {
    match jump {
        Jump::Absolute_u16() => {
            let addr = mmu.read16(cpu.r.pc + 1);
            cpu.set_pc(addr);
            // println!("abs jump to {:04x}",addr);
            debug!("Abs Jump to {:04x}",addr);
        },
        Jump::Relative_cond_carry_i8() => {
            cpu.inc_pc();
            let e = u8_as_i8(mmu.read8(cpu.r.pc));
            cpu.inc_pc();
            // println!("carry flag is set to {}",cpu.r.carry_flag);
            if cpu.r.carry_flag { cpu.set_pc((((cpu.r.pc) as i32) + e as i32) as u16); }
        },
        Jump::Relative_cond_notcarry_i8() => {
            cpu.inc_pc();
            let e = u8_as_i8(mmu.read8(cpu.r.pc));
            cpu.inc_pc();
            if !cpu.r.carry_flag { cpu.set_pc((((cpu.r.pc) as i32) + e as i32) as u16); }
        }
        Jump::Relative_cond_zero_i8() => {
            cpu.inc_pc();
            let e = u8_as_i8(mmu.read8(cpu.r.pc));
            cpu.inc_pc();
            if cpu.r.zero_flag { cpu.set_pc((((cpu.r.pc) as i32) + e as i32) as u16); }
        }
        Jump::Relative_cond_notzero_i8() => {
            cpu.inc_pc();
            let e = u8_as_i8(mmu.read8(cpu.r.pc));
            cpu.inc_pc();
            if !cpu.r.zero_flag { cpu.set_pc((((cpu.r.pc) as i32) + e as i32) as u16); }
        },
        Jump::Absolute_cond_notzero_u16() => {
            cpu.inc_pc();
            let dst = mmu.read16(cpu.r.pc);
            cpu.inc_pc();
            cpu.inc_pc();
            if !cpu.r.zero_flag {
                cpu.set_pc(dst);
            }
        },
        Jump::Relative_i8() => {
            cpu.inc_pc();
            let e = u8_as_i8(mmu.read8(cpu.r.pc));
            cpu.inc_pc();
            cpu.set_pc((((cpu.r.pc) as i32) + e as i32) as u16);
        }
    }
}
