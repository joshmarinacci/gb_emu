use std::fmt::{Display, Formatter};
use serde_json::Value;
use Compare::CP_A_r;
use Math::{ADD_R_R, AND_A_r, OR_A_r, SUB_R_R, XOR_A_r};
use crate::{MMU, Z80};
use crate::opcodes::DoubleRegister::{AF, BC, DE, HL, SP};
use crate::opcodes::Load::Load_r_u8;
use crate::opcodes::Math::{ADD_RR_RR, XOR_A_addr, XOR_A_u8};
use crate::opcodes::RegisterName::{A, B, C, D, E, H, L};
use crate::opcodes::Special::{CALL_u16, DisableInterrupts, HALT, NOOP, POP, PUSH, RET, RETI, RETZ, RST, STOP};

pub fn u8_as_i8(v: u8) -> i8 {
    return v as i8
}

pub enum RegisterName {
    A,B,C,D,E,H,L,F,
}
pub enum DoubleRegister {
    BC,DE,HL,SP,AF,
}
pub enum Special {
    NOOP(),
    STOP(),
    HALT(),
    DisableInterrupts(),
    CALL_u16(),
    PUSH(DoubleRegister),
    POP(DoubleRegister),
    RET(),
    RETZ(),
    RETI(),
    RST(u8),
}
pub enum Load {
    Load_r_u8(RegisterName),
    Load_r_r(RegisterName,RegisterName),

    Load_high_r_u8(RegisterName),
    Load_high_u8_r(RegisterName),
    Load_high_r_r(RegisterName,RegisterName),

    Load_R2_u16(DoubleRegister),
    Load_R_addr_R2(RegisterName,DoubleRegister),
    Load_addr_R2_A_inc(DoubleRegister),  // Load (HL+), A, copy contents of A into memory at HL, then INC HL
    Load_addr_R2_A_dec(DoubleRegister),  // Load (HL+), A, copy contents of A into memory at HL, then DEC HL
    Load_A_addr_R2_inc(DoubleRegister),  // Load A, (HL+), copy contents of memory at HL to A, then INC HL
    Load_addr_R2_A(DoubleRegister, RegisterName),      // Load (rr), R
    Load_addr_u16_A(), // Load (nn), A
    Load_addr_u16_R2(DoubleRegister),
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
    ADD_R_u8(RegisterName),
    ADD_R_R(RegisterName,RegisterName),
    ADD_RR_RR(DoubleRegister,DoubleRegister),
    SUB_R_R(RegisterName,RegisterName),
    XOR_A_r(RegisterName),
    XOR_A_u8(RegisterName),
    XOR_A_addr(DoubleRegister),
    OR_A_r(RegisterName),
    AND_A_r(RegisterName),
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
        0x40 => Some(Instr::Load(Load::Load_r_r(B,B))),
        0x41 => Some(Instr::Load(Load::Load_r_r(B,C))),
        0x42 => Some(Instr::Load(Load::Load_r_r(B,D))),
        0x43 => Some(Instr::Load(Load::Load_r_r(B,E))),
        0x44 => Some(Instr::Load(Load::Load_r_r(B,H))),
        0x45 => Some(Instr::Load(Load::Load_r_r(B,L))),
        0x46 => Some(Instr::Load(Load::Load_R_addr_R2(B,HL))),
        0x47 => Some(Instr::Load(Load::Load_r_r(B,A))),
        0x48 => Some(Instr::Load(Load::Load_r_r(C,B))),
        0x49 => Some(Instr::Load(Load::Load_r_r(C,C))),
        0x4A => Some(Instr::Load(Load::Load_r_r(C,D))),
        0x4B => Some(Instr::Load(Load::Load_r_r(C,E))),
        0x4C => Some(Instr::Load(Load::Load_r_r(C,H))),
        0x4D => Some(Instr::Load(Load::Load_r_r(C,L))),
        0x4E => Some(Instr::Load(Load::Load_R_addr_R2(C,HL))),
        0x4F => Some(Instr::Load(Load::Load_r_r(C,A))),

        0x50 => Some(Instr::Load(Load::Load_r_r(D,B))),
        0x51 => Some(Instr::Load(Load::Load_r_r(D,C))),
        0x52 => Some(Instr::Load(Load::Load_r_r(D,D))),
        0x53 => Some(Instr::Load(Load::Load_r_r(D,E))),
        0x54 => Some(Instr::Load(Load::Load_r_r(D,H))),
        0x55 => Some(Instr::Load(Load::Load_r_r(D,L))),
        0x56 => Some(Instr::Load(Load::Load_R_addr_R2(D,HL))),
        0x57 => Some(Instr::Load(Load::Load_r_r(D,A))),
        0x58 => Some(Instr::Load(Load::Load_r_r(E,B))),
        0x59 => Some(Instr::Load(Load::Load_r_r(E,C))),
        0x5A => Some(Instr::Load(Load::Load_r_r(E,D))),
        0x5B => Some(Instr::Load(Load::Load_r_r(E,E))),
        0x5C => Some(Instr::Load(Load::Load_r_r(E,H))),
        0x5D => Some(Instr::Load(Load::Load_r_r(E,L))),
        0x5E => Some(Instr::Load(Load::Load_R_addr_R2(E,HL))),
        0x5F => Some(Instr::Load(Load::Load_r_r(E,A))),

        0x60 => Some(Instr::Load(Load::Load_r_r(H,B))),
        0x61 => Some(Instr::Load(Load::Load_r_r(H,C))),
        0x62 => Some(Instr::Load(Load::Load_r_r(H,D))),
        0x63 => Some(Instr::Load(Load::Load_r_r(H,E))),
        0x64 => Some(Instr::Load(Load::Load_r_r(H,H))),
        0x65 => Some(Instr::Load(Load::Load_r_r(H,L))),
        0x66 => Some(Instr::Load(Load::Load_R_addr_R2(H,HL))),
        0x67 => Some(Instr::Load(Load::Load_r_r(H,A))),
        0x68 => Some(Instr::Load(Load::Load_r_r(L,B))),
        0x69 => Some(Instr::Load(Load::Load_r_r(L,C))),
        0x6A => Some(Instr::Load(Load::Load_r_r(L,D))),
        0x6B => Some(Instr::Load(Load::Load_r_r(L,E))),
        0x6C => Some(Instr::Load(Load::Load_r_r(L,H))),
        0x6D => Some(Instr::Load(Load::Load_r_r(L,L))),
        0x6E => Some(Instr::Load(Load::Load_R_addr_R2(L,HL))),
        0x6F => Some(Instr::Load(Load::Load_r_r(L,A))),


        0x70 => Some(Instr::Load(Load::Load_addr_R2_A(HL,B))),
        0x71 => Some(Instr::Load(Load::Load_addr_R2_A(HL,C))),
        0x72 => Some(Instr::Load(Load::Load_addr_R2_A(HL,D))),
        0x73 => Some(Instr::Load(Load::Load_addr_R2_A(HL,E))),
        0x74 => Some(Instr::Load(Load::Load_addr_R2_A(HL,H))),
        0x75 => Some(Instr::Load(Load::Load_addr_R2_A(HL,L))),

        0x77 => Some(Instr::Load(Load::Load_addr_R2_A(HL,A))),
        0x78 => Some(Instr::Load(Load::Load_r_r(A,B))),
        0x79 => Some(Instr::Load(Load::Load_r_r(A,C))),
        0x7A => Some(Instr::Load(Load::Load_r_r(A,D))),
        0x7B => Some(Instr::Load(Load::Load_r_r(A,E))),
        0x7C => Some(Instr::Load(Load::Load_r_r(A,H))),
        0x7D => Some(Instr::Load(Load::Load_r_r(A,L))),
        0x7E => Some(Instr::Load(Load::Load_R_addr_R2(A,HL))),
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
        0x08 => Some(Instr::Load(Load::Load_addr_u16_R2(SP))),

        0x0a => Some(Instr::Load(Load::Load_R_addr_R2(A,BC))),
        0x1a => Some(Instr::Load(Load::Load_R_addr_R2(A,DE))),
        // put value pointed to by DE into A

        0x22 => Some(Instr::Load(Load::Load_addr_R2_A_inc(HL))),
        0x32 => Some(Instr::Load(Load::Load_addr_R2_A_dec(HL))),
        0x2A => Some(Instr::Load(Load::Load_A_addr_R2_inc(HL))),

        0x02 => Some(Instr::Load(Load::Load_addr_R2_A(BC,A))),
        0x12 => Some(Instr::Load(Load::Load_addr_R2_A(DE,A))),

        0xe2 => Some(Instr::Load(Load::Load_high_r_r(C,A))),
        0xea => Some(Instr::Load(Load::Load_addr_u16_A())),


        0x00 => Some(Instr::Special(NOOP())),
        0xF3 => Some(Instr::Special(DisableInterrupts())),
        0x10 => Some(Instr::Special(STOP())),
        0x76 => Some(Instr::Special(HALT())),
        0xCD => Some(Instr::Special(CALL_u16())),
        0xC1 => Some(Instr::Special(POP(BC))),
        0xC5 => Some(Instr::Special(PUSH(BC))),
        0xD1 => Some(Instr::Special(POP(DE))),
        0xD5 => Some(Instr::Special(PUSH(DE))),
        0xE1 => Some(Instr::Special(POP(HL))),
        0xE5 => Some(Instr::Special(PUSH(HL))),
        0xF1 => Some(Instr::Special(POP(AF))),
        0xF5 => Some(Instr::Special(PUSH(AF))),

        0xC8 => Some(Instr::Special(RETZ())),
        0xC9 => Some(Instr::Special(RET())),
        0xD9 => Some(Instr::Special(RETI())),

        0xC7 => Some(Instr::Special(RST(0x00))),
        0xD7 => Some(Instr::Special(RST(0x10))),
        0xE7 => Some(Instr::Special(RST(0x20))),
        0xF7 => Some(Instr::Special(RST(0x30))),
        0xCF => Some(Instr::Special(RST(0x08))),
        0xDF => Some(Instr::Special(RST(0x18))),
        0xEF => Some(Instr::Special(RST(0x28))),
        0xFF => Some(Instr::Special(RST(0x38))),

        0xF0 => Some(Instr::Load(Load::Load_high_r_u8(A))),

        0x18 => Some(Instr::Jump(Jump::JumpRelative_i8())), //2 bytes, no flags, relative signed
        0x20 => Some(Instr::Jump(Jump::JumpRelative_cond_notzero_i8())),
        0x28 => Some(Instr::Jump(Jump::JumpRelative_cond_zero_i8())), //2 bytes, if zero, relative signed
        0x30 => Some(Instr::Jump(Jump::JumpRelative_cond_notcarry_i8())), //2 bytes, if not carry, realtive signed
        0x38 => Some(Instr::Jump(Jump::JumpRelative_cond_carry_i8())), //2 bytes, if carry, relative signed
        0xC3 => Some(Instr::Jump(Jump::JumpAbsolute_u16())),
        0xFE => Some(Instr::Compare(Compare::CP_A_n())),


        0x39 => Some(Instr::Math(ADD_RR_RR(HL,SP))),

        0x80 => Some(Instr::Math(ADD_R_R(A, B))),
        0x81 => Some(Instr::Math(ADD_R_R(A, C))),
        0x82 => Some(Instr::Math(ADD_R_R(A, D))),
        0x83 => Some(Instr::Math(ADD_R_R(A, E))),
        0x84 => Some(Instr::Math(ADD_R_R(A, H))),
        0x85 => Some(Instr::Math(ADD_R_R(A, L))),
        0x87 => Some(Instr::Math(ADD_R_R(A, A))),

        0x90 => Some(Instr::Math(SUB_R_R(A, B))),
        0x91 => Some(Instr::Math(SUB_R_R(A, C))),
        0x92 => Some(Instr::Math(SUB_R_R(A, D))),
        0x93 => Some(Instr::Math(SUB_R_R(A, E))),
        0x94 => Some(Instr::Math(SUB_R_R(A, H))),
        0x95 => Some(Instr::Math(SUB_R_R(A, L))),
        0x97 => Some(Instr::Math(SUB_R_R(A, A))),

        0xA0 => Some(Instr::Math(AND_A_r(B))),
        0xA1 => Some(Instr::Math(AND_A_r(C))),
        0xA2 => Some(Instr::Math(AND_A_r(D))),
        0xA3 => Some(Instr::Math(AND_A_r(E))),
        0xA4 => Some(Instr::Math(AND_A_r(H))),
        0xA5 => Some(Instr::Math(AND_A_r(L))),
        0xA7 => Some(Instr::Math(AND_A_r(A))),

        0xA8 => Some(Instr::Math(XOR_A_r(B))),
        0xA9 => Some(Instr::Math(XOR_A_r(C))),
        0xAA => Some(Instr::Math(XOR_A_r(D))),
        0xAB => Some(Instr::Math(XOR_A_r(E))),
        0xAC => Some(Instr::Math(XOR_A_r(H))),
        0xAD => Some(Instr::Math(XOR_A_r(L))),
        0xAE => Some(Instr::Math(XOR_A_addr(HL))),
        0xAF => Some(Instr::Math(XOR_A_r(A))),
        0xEE => Some(Instr::Math(XOR_A_u8(A))),

        0xB0 => Some(Instr::Math(OR_A_r(B))),
        0xB1 => Some(Instr::Math(OR_A_r(C))),
        0xB2 => Some(Instr::Math(OR_A_r(D))),
        0xB3 => Some(Instr::Math(OR_A_r(E))),
        0xB4 => Some(Instr::Math(OR_A_r(H))),
        0xB5 => Some(Instr::Math(OR_A_r(L))),
        0xB7 => Some(Instr::Math(OR_A_r(A))),

        0xB8 => Some(Instr::Compare(CP_A_r(B))),
        0xB9 => Some(Instr::Compare(CP_A_r(C))),
        0xBA => Some(Instr::Compare(CP_A_r(D))),

        0xC6 => Some(Instr::Math(Math::ADD_R_u8(A))),



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
        0xCB38 => Some(Instr::Math(Math::SLA(B))),
        0xCB7C => Some(Instr::Math(Math::BIT(7, H))),
        0xCB7E => Some(Instr::Math(Math::BITR2(7, HL))),
        0xCB19 => Some(Instr::Math(Math::RR(C))),
        0xCB1A => Some(Instr::Math(Math::RR(D))),
        0xCB1B => Some(Instr::Math(Math::RR(E))),
        0xCB1C => Some(Instr::Math(Math::RR(H))),
        0xCB1D => Some(Instr::Math(Math::RR(L))),
        0xCB1F => Some(Instr::Math(Math::RR(A))),

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
