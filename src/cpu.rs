use core::default::Default;
use core::fmt::{Debug, Formatter};
use std::collections::HashMap;
use crate::bootrom::BOOT_ROM;
use crate::{MMU, setup_op_codes};

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
    pub fn get_hl(&self) -> u16 {
        (self.l as u16) + ((self.h as u16) << 8)
    }
    pub fn set_bc(&mut self, val:u16) {
        self.b = (val >> 8) as u8;
        self.c = (0x00FF & val) as u8;
    }
    pub fn set_hl(&mut self, val:u16) {
        self.h = (val >> 8) as u8;
        self.l = (0x00FF & val) as u8;
    }
    pub fn set_de(&mut self, val:u16) {
        self.d = (val >> 8) as u8;
        self.e = (0x00FF & val) as u8;
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