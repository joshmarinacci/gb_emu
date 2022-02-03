use crate::optest::R16;

pub struct Flags {
    pub zero:bool,
    pub carry:bool,
    pub half:bool,
    pub subn:bool,
}
pub struct CPU {
    pc:u16,
    sp:u16,
    a:u8,
    b:u8,
    c:u8,
    d:u8,
    e:u8,
    h:u8,
    l:u8,
    pub r:Flags,
}


pub enum CPUR8 {
    R8A,R8B,R8C,R8D,R8E,R8H,R8L
}
pub enum CPUR16 {
    BC, PC, SP, DE, HL
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
        }
    }

    pub(crate) fn get_r16(&self, reg: CPUR16) -> u16 {
        match reg {
            CPUR16::PC => self.pc,
            CPUR16::SP => self.sp,
            CPUR16::BC => (self.c as u16) + ((self.b as u16) << 8),
            CPUR16::DE => (self.e as u16) + ((self.d as u16) << 8),
            CPUR16::HL => (self.l as u16) + ((self.h as u16) << 8),
        }
    }
    pub(crate) fn set_r16(&mut self, reg:CPUR16, val:u16) {
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
        }
    }
}

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
                subn: false
            }
        }
    }
    pub fn get_pc(&self) -> u16 {
        self.pc
    }
    pub fn set_pc(&mut self, pc: u16) {
        self.pc = pc;
    }
    pub(crate) fn inc_pc(&mut self) {
        self.pc += 1;
    }
    pub(crate) fn get_sp(&self) -> u16 {
        self.sp
    }
    pub(crate) fn dec_sp(&mut self) {
        self.sp -= 1;
    }
    pub(crate) fn inc_sp(&mut self) {
        self.sp += 1;
    }
}
