use crate::common::{get_bit_as_bool, set_bit};
use crate::optest::R16;
use std::collections::VecDeque;

pub struct Flags {
    pub zero: bool,
    pub carry: bool,
    pub half: bool,
    pub subn: bool,
}
pub struct CPU {
    pub(crate) pc: u16,
    sp: u16,
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    pub r: Flags,
    pub recent_pcs: VecDeque<u16>,
}

impl CPU {
    pub fn reg_to_str(&self) -> String {
        format!("A:{:02x}  B:{:02x}  C:{:02x}  D:{:02x}  E:{:02x}  H:{:02x}  L:{:02x}    BC:{:04x}  DE:{:04x}  HL:{:04x} ",
                self.get_r8(CPUR8::R8A),
                self.get_r8(CPUR8::R8B),
                self.get_r8(CPUR8::R8C),
                self.get_r8(CPUR8::R8D),
                self.get_r8(CPUR8::R8E),
                self.get_r8(CPUR8::R8H),
                self.get_r8(CPUR8::R8L),
                self.get_r16(CPUR16::BC),
                self.get_r16(CPUR16::DE),
                self.get_r16(CPUR16::HL),
        )
    }
}

pub enum CPUR8 {
    R8A,
    R8B,
    R8C,
    R8D,
    R8E,
    R8H,
    R8L,
    R8F,
}
pub enum CPUR16 {
    BC,
    PC,
    SP,
    DE,
    HL,
    AF,
}
pub enum CPURegister {
    CpuR8(CPUR8),
    R16(CPUR16),
}

impl CPU {
    pub(crate) fn get_r8(&self, reg: CPUR8) -> u8 {
        match reg {
            CPUR8::R8A => self.a,
            CPUR8::R8B => self.b,
            CPUR8::R8C => self.c,
            CPUR8::R8D => self.d,
            CPUR8::R8E => self.e,
            CPUR8::R8H => self.h,
            CPUR8::R8L => self.l,
            CPUR8::R8F => {
                let mut v = 0x00;
                v = set_bit(v, 7, self.r.zero);
                v = set_bit(v, 6, self.r.subn);
                v = set_bit(v, 5, self.r.half);
                v = set_bit(v, 4, self.r.carry);
                v
            }
        }
    }
    pub(crate) fn set_r8(&mut self, reg: CPUR8, val: u8) {
        match reg {
            CPUR8::R8A => self.a = val,
            CPUR8::R8B => self.b = val,
            CPUR8::R8C => self.c = val,
            CPUR8::R8D => self.d = val,
            CPUR8::R8E => self.e = val,
            CPUR8::R8H => self.h = val,
            CPUR8::R8L => self.l = val,
            CPUR8::R8F => {
                self.r.zero = get_bit_as_bool(val, 7);
                self.r.subn = get_bit_as_bool(val, 6);
                self.r.half = get_bit_as_bool(val, 5);
                self.r.carry = get_bit_as_bool(val, 4);
            }
        }
    }

    pub(crate) fn get_r16(&self, reg: CPUR16) -> u16 {
        match reg {
            CPUR16::PC => self.pc,
            CPUR16::SP => self.sp,
            CPUR16::BC => (self.c as u16) + ((self.b as u16) << 8),
            CPUR16::DE => (self.e as u16) + ((self.d as u16) << 8),
            CPUR16::HL => (self.l as u16) + ((self.h as u16) << 8),
            CPUR16::AF => {
                let a = self.get_r8(CPUR8::R8A);
                let f = self.get_r8(CPUR8::R8F);
                (f as u16) + ((a as u16) << 8)
            }
        }
    }
    pub(crate) fn set_r16(&mut self, reg: CPUR16, val: u16) {
        match reg {
            CPUR16::PC => self.pc = val,
            CPUR16::SP => self.sp = val,
            CPUR16::BC => {
                self.b = (val >> 8) as u8;
                self.c = (0x00FF & val) as u8;
            }
            CPUR16::DE => {
                self.d = (val >> 8) as u8;
                self.e = (0x00FF & val) as u8;
            }
            CPUR16::HL => {
                self.h = (val >> 8) as u8;
                self.l = (0x00FF & val) as u8;
            }
            CPUR16::AF => {
                self.set_r8(CPUR8::R8A, (val >> 8) as u8);
                self.set_r8(CPUR8::R8F, (0x00ff & val) as u8);
            }
        }
    }
}

const MAX_PC_SCROLLBACK: usize = 50;
impl CPU {
    pub(crate) fn init() -> CPU {
        CPU {
            pc: 0,
            sp: 0,
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            r: Flags {
                zero: false,
                carry: false,
                half: false,
                subn: false,
            },
            recent_pcs: VecDeque::new(),
        }
    }
    pub fn get_pc(&self) -> u16 {
        self.pc
    }
    pub fn real_set_pc(&mut self, pc: u16) {
        self.recent_pcs.push_front(pc);
        if self.recent_pcs.len() > MAX_PC_SCROLLBACK {
            self.recent_pcs.pop_back();
        }
        self.pc = pc;
    }
    pub(crate) fn inc_pc(&mut self) {
        self.pc += 1;
    }
    pub fn get_sp(&self) -> u16 {
        self.sp
    }
    pub(crate) fn dec_sp(&mut self) {
        self.sp -= 1;
    }
    pub(crate) fn inc_sp(&mut self) {
        self.sp += 1;
    }
}
