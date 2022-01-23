use core::default::Default;
use core::fmt::{Debug, Formatter};
use std::collections::HashMap;
use crate::bootrom::BOOT_ROM;
use crate::{MMU};
use crate::common::{get_bit, get_bit_as_bool, set_bit};
use crate::opcodes::{DoubleRegister, RegisterName};
use crate::opcodes::RegisterName::{*};

#[derive(Debug)]
pub struct Z80_registers {
    // 8 bit registers
    pub(crate) a:u8,
    pub(crate) b:u8,
    pub(crate) c:u8,
    pub(crate) d:u8,
    pub(crate) e:u8,

    // f:u8, // flag register
    pub(crate) zero_flag:bool,
    pub(crate) subtract_n_flag:bool,
    pub(crate) half_flag:bool,
    pub(crate) carry_flag:bool,

    pub(crate) h:u8,
    pub(crate) l:u8,

    // 16 bit registers
    pub(crate) pc:u16, // program counter
    pub(crate) sp:u16, // stack pointer

    m:u8,
    t:u8,
    ime:u8,
}


impl Z80_registers {
    pub(crate) fn get_u8reg(&self, reg: &RegisterName) -> u8 {
        match reg {
            RegisterName::A => self.a,
            RegisterName::B => self.b,
            RegisterName::C => self.c,
            RegisterName::D => self.d,
            RegisterName::E => self.e,
            RegisterName::H => self.h,
            RegisterName::L => self.l,
            F => {
                let mut v = 0x00;
                v = set_bit(v,7,self.zero_flag);
                v = set_bit(v,6,self.subtract_n_flag);
                v = set_bit(v,5,self.half_flag);
                v = set_bit(v,4,self.carry_flag);
                v
            }
        }
    }
    pub(crate) fn set_u8reg(&mut self, reg: &RegisterName, val: u8) {
        match reg {
            A => self.a = val,
            B => self.b = val,
            C => self.c = val,
            D => self.d = val,
            E => self.e = val,
            H => self.h = val,
            L => self.l = val,
            F => {
                self.zero_flag = get_bit_as_bool(val,7);
                self.subtract_n_flag = get_bit_as_bool(val,6);
                self.half_flag = get_bit_as_bool(val,5);
                self.carry_flag = get_bit_as_bool(val,4);
            }
        }
    }
    pub(crate) fn get_u16reg(&self, reg: &DoubleRegister) -> u16 {
        match reg {
            DoubleRegister::BC => self.get_bc(),
            DoubleRegister::DE => self.get_de(),
            DoubleRegister::HL => self.get_hl(),
            DoubleRegister::SP => self.get_sp(),
            DoubleRegister::AF => self.get_af(),
        }
    }
    pub(crate) fn set_u16reg(&mut self, reg: &DoubleRegister, val: u16) {
        match reg {
            DoubleRegister::BC => self.set_bc(val),
            DoubleRegister::DE => self.set_de(val),
            DoubleRegister::HL => self.set_hl(val),
            DoubleRegister::SP => self.set_sp(val),
            DoubleRegister::AF => self.set_af(val),
        }
    }
}


impl Z80_registers {
    pub fn get_hl(&self) -> u16 {
        (self.l as u16) + ((self.h as u16) << 8)
    }
    pub fn get_bc(&self) -> u16 { (self.c as u16) + ((self.b as u16) << 8) }
    pub fn set_bc(&mut self, val:u16) {
        self.b = (val >> 8) as u8;
        self.c = (0x00FF & val) as u8;
    }
    pub fn set_hl(&mut self, val:u16) {
        self.h = (val >> 8) as u8;
        self.l = (0x00FF & val) as u8;
    }
    pub fn get_de(&self) -> u16 {
        (self.e as u16) + ((self.d as u16) << 8)
    }
    pub fn set_de(&mut self, val:u16) {
        self.d = (val >> 8) as u8;
        self.e = (0x00FF & val) as u8;
    }
    pub fn get_sp(&self) -> u16 {
        self.sp
    }
    pub fn set_sp(&mut self, val:u16) {
        self.sp = val;
    }
    pub fn get_af(&self) -> u16 {
        let v1 = self.get_u8reg(&A);
        let v2:u8 = self.get_u8reg(&F);
        (v2 as u16) + (v1 as u16)
    }
    pub fn set_af(&mut self, val:u16) {
        self.a = (val >>8 ) as u8;
        let f = (0x00ff & val) as u8;
        self.set_u8reg(&F,f)

    }
}

#[derive(Debug)]
pub struct Z80 {
    clock_m: u16,
    clock_t: u16,
    pub r: Z80_registers,
    halt: bool,
}

impl Z80 {
    pub(crate) fn init() -> Z80 {
        Z80 {
            clock_m:0,
            clock_t:0,
            halt:false,
            // stop:0,
            r: Z80_registers {
                a: 0,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                zero_flag: false,
                subtract_n_flag: false,
                carry_flag: false,
                half_flag:false,
                h: 0,
                l: 0,
                pc: 0,
                sp: 0,
                m: 0,
                t: 0,
                ime: 0
            },
        }
    }
    pub(crate) fn reset(&mut self) {
        self.r.a = 0;
        self.r.b = 0;
        self.r.c = 0;
        self.r.d = 0;
        self.r.e = 0;

        self.r.zero_flag = false;
        self.r.h = 0;
        self.r.l = 0;

        self.r.sp = 0;
        self.r.pc = 0;

        self.r.m = 0;                       // set timing
        self.r.t = 0;
    }
}
