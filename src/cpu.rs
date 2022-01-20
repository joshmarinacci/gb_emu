use core::default::Default;
use core::fmt::{Debug, Formatter};
use std::collections::HashMap;
use crate::bootrom::BOOT_ROM;
use crate::{MMU, RegisterName};
use crate::opcodes::{DoubleRegister, setup_op_codes};

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
        }
    }
    pub(crate) fn set_u8reg(&mut self, reg: &RegisterName, val: u8) {
        match reg {
            RegisterName::A => self.a = val,
            RegisterName::B => self.b = val,
            RegisterName::C => self.c = val,
            RegisterName::D => self.d = val,
            RegisterName::E => self.e = val,
            RegisterName::H => self.h = val,
            RegisterName::L => self.l = val,
        }
    }
    pub(crate) fn get_u16reg(&self, reg: &DoubleRegister) -> u16 {
        match reg {
            DoubleRegister::BC => self.get_bc(),
            DoubleRegister::DE => self.get_de(),
            DoubleRegister::HL => self.get_hl(),
            DoubleRegister::SP => self.get_sp(),
        }
    }
    pub(crate) fn set_u16reg(&mut self, reg: &DoubleRegister, val: u16) {
        match reg {
            DoubleRegister::BC => self.set_bc(val),
            DoubleRegister::DE => self.set_de(val),
            DoubleRegister::HL => self.set_hl(val),
            DoubleRegister::SP => self.set_sp(val),
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
}

#[derive(Debug)]
pub struct Z80 {
    clock_m: u16,
    clock_t: u16,
    pub r: Z80_registers,
    // halt:u8,
    // stop:u8,
    halt: bool,
    pub(crate) ops: OpList,
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
            ops: setup_op_codes(),
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


pub struct Op {
    pub(crate) name:String,
    pub(crate) inst_len:usize,
    pub(crate) tim_len: usize,
    pub(crate) fun:fn(&mut Z80, &mut MMU),
}

impl Debug for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("stuff")
    }
}

#[derive(Debug)]
pub struct OpList {
    pub(crate) ops:HashMap<u16,Op>,
}

impl OpList {
    pub(crate) fn init() -> OpList {
        OpList {
            ops: Default::default()
        }
    }
    pub(crate) fn add(&mut self, code:u16, name: &str, inst_len:usize, tim_len: usize, fun: fn(cpu:&mut Z80, mmu:&mut MMU)) {
        self.ops.insert(code, Op {
            name:name.to_string(),
            inst_len,
            tim_len,
            fun,
        });
    }
}
