use crate::common::{get_bit_as_bool, set_bit};
use std::collections::VecDeque;
use crate::gbstate::GBState;

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
    pub IME:bool,
    pub halt:bool,
}

impl CPU {
    pub fn reg_to_str(&self) -> String {
        format!("A:{:02x}  B:{:02x}  C:{:02x}  D:{:02x}  E:{:02x}  H:{:02x}  L:{:02x}    BC:{:04x}  DE:{:04x}  HL:{:04x} ",
                self.get_r8(R8::A),
                self.get_r8(R8::B),
                self.get_r8(R8::C),
                self.get_r8(R8::D),
                self.get_r8(R8::E),
                self.get_r8(R8::H),
                self.get_r8(R8::L),
                self.get_r16(R16::BC),
                self.get_r16(R16::DE),
                self.get_r16(R16::HL),
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub enum R8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    F,
}

#[derive(Debug, Copy, Clone)]
pub enum R16 {
    BC,
    HL,
    DE,
    SP,
    PC,
    AF,
}


impl R8 {
    pub(crate) fn get_value(&self, gb: &GBState) -> u8 {
        gb.cpu.get_r8(*self)
    }
    pub(crate) fn set_value(&self, gb: &mut GBState, value: u8) {
        gb.cpu.set_r8(*self, value)
    }
    pub(crate) fn name(&self) -> &'static str {
        match self {
            R8::A => "A",
            R8::B => "B",
            R8::C => "C",
            R8::D => "D",
            R8::E => "E",
            R8::H => "H",
            R8::L => "L",
            R8::F => "F",
        }
    }
}

impl R16 {
    pub(crate) fn get_value(&self, gb: &GBState) -> u16 {
        gb.cpu.get_r16(*self)
    }
    pub(crate) fn set_value(&self, gb: &mut GBState, val: u16) {
        gb.cpu.set_r16(*self,val)
    }
    pub(crate) fn name(&self) -> &'static str {
        match self {
            R16::BC => "BC",
            R16::HL => "HL",
            R16::DE => "DE",
            R16::SP => "SP",
            R16::AF => "AF",
            R16::PC => "PC",
        }
    }
}


impl CPU {
    pub(crate) fn get_r8(&self, reg: R8) -> u8 {
        match reg {
            R8::A => self.a,
            R8::B => self.b,
            R8::C => self.c,
            R8::D => self.d,
            R8::E => self.e,
            R8::H => self.h,
            R8::L => self.l,
            R8::F => {
                let mut v = 0x00;
                v = set_bit(v, 7, self.r.zero);
                v = set_bit(v, 6, self.r.subn);
                v = set_bit(v, 5, self.r.half);
                v = set_bit(v, 4, self.r.carry);
                v
            }
        }
    }
    pub(crate) fn set_r8(&mut self, reg: R8, val: u8) {
        match reg {
            R8::A => self.a = val,
            R8::B => self.b = val,
            R8::C => self.c = val,
            R8::D => self.d = val,
            R8::E => self.e = val,
            R8::H => self.h = val,
            R8::L => self.l = val,
            R8::F => {
                self.r.zero = get_bit_as_bool(val, 7);
                self.r.subn = get_bit_as_bool(val, 6);
                self.r.half = get_bit_as_bool(val, 5);
                self.r.carry =get_bit_as_bool(val, 4);
            }
        }
    }

    pub(crate) fn get_r16(&self, reg: R16) -> u16 {
        match reg {
            R16::PC => self.pc,
            R16::SP => self.sp,
            R16::BC => (self.c as u16) + ((self.b as u16) << 8),
            R16::HL => (self.l as u16) + ((self.h as u16) << 8),
            R16::DE => (self.e as u16) + ((self.d as u16) << 8),
            R16::AF => {
                let a = self.get_r8(R8::A);
                let f = self.get_r8(R8::F);
                (f as u16) + ((a as u16) << 8)
            }
        }
    }
    pub(crate) fn set_r16(&mut self, reg: R16, val: u16) {
        match reg {
            R16::PC => self.pc = val,
            R16::SP => self.sp = val,
            R16::BC => {
                self.b = (val >> 8) as u8;
                self.c = (0x00FF & val) as u8;
            }
            R16::DE => {
                self.d = (val >> 8) as u8;
                self.e = (0x00FF & val) as u8;
            }
            R16::HL => {
                self.h = (val >> 8) as u8;
                self.l = (0x00FF & val) as u8;
            }
            R16::AF => {
                self.set_r8(R8::A, (val >> 8) as u8);
                self.set_r8(R8::F, (0x00ff & val) as u8);
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
            IME: false,
            halt: false
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
        // self.check_infinite_loop();
        self.pc = pc;
    }
    pub(crate) fn inc_pc(&mut self) {
        self.pc += 1;
    }
    pub fn get_sp(&self) -> u16 {
        self.sp
    }
    pub fn real_set_sp(&mut self, sp:u16)  {
        self.sp = sp;
    }
    pub(crate) fn dec_sp(&mut self) {
        self.sp -= 1;
    }
    pub(crate) fn inc_sp(&mut self) {
        self.sp += 1;
    }
    fn check_infinite_loop(&self) {
        if self.recent_pcs.len() >= 2 {
            let ins0 = self.recent_pcs[0];
            let ins1 = self.recent_pcs[1];
            if ins0 == ins1 {
                println!("possible infinite loop at {:04x}",ins0);
            }


        }
    }
}
