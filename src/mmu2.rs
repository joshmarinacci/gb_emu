use std::collections::HashSet;
use crate::common::{get_bit_as_bool, MBC, set_bit};
use log::info;
use std::fs;
use crate::hardware::{LCDCRegister, MemRange, STATRegister};
use crate::mmu2::IORegister::{SCX, SCY};

/*
IO registers
- [ ] print when read or written to
- [ ] pretty print the names when matching their addresses
- [ ] be able to dump them all easily as a group in a loop (array of HW vec enums?) (func on the Hardware object?)
- [ ] make all access to the HW registers through functions, not direct sets.
- [ ] then test loading tetris again to see if it works.

 */

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum IORegister {
    JOYPAD_P1,
    SB,
    SC,
    DIV,
    TIMA,
    TMA,
    TAC,
    NR10,
    NR11,
    NR12,
    NR13,
    NR14,
    NR21,
    NR22,
    NR23,
    NR24,
    NR30,
    NR31,
    NR32,
    NR33,
    NR34,
    NR41,
    NR42,
    NR43,
    NR44,
    NR50,
    NR51,
    NR52,
    LCDC,
    STAT,
    SCY,
    SCX,
    LY,
    LYC,
    DMA,
    BGP,
    OBP0,
    OBP1,
    WY,
    WX,
    IE, // Interrupt Enable R/W
    IF, // Interrupt Flag   R/W
    DISABLE_BOOTROM,
}

impl IORegister {
    pub fn match_address(addr: u16) -> Option<IORegister> {
        match addr {
            0xFF00 => Some(IORegister::JOYPAD_P1),
            0xFF20 => Some(IORegister::NR41),
            0xFF21 => Some(IORegister::NR42),
            0xFF40 => Some(IORegister::LCDC),
            0xFF41 => Some(IORegister::STAT),
            0xFF42 => Some(IORegister::SCY),
            0xFF43 => Some(IORegister::SCX),
            0xFF44 => Some(IORegister::LY),
            0xFF45 => Some(IORegister::LYC),
            0xFF46 => Some(IORegister::DMA),
            0xFF47 => Some(IORegister::BGP),
            0xFF48 => Some(IORegister::OBP0),
            0xFF49 => Some(IORegister::OBP1),
            0xFF4A => Some(IORegister::WY),
            0xFF4B => Some(IORegister::WX),
            0xFF0F => Some(IORegister::IF),
            0xFFFF => Some(IORegister::IE),
            _ => None,
        }
    }
    pub fn match_name(name:&str) -> Option<IORegister> {
        let lname = name.to_lowercase();
        if IORegister::SCX.name().to_lowercase().eq(&lname) { return Some(IORegister::SCX); }
        if IORegister::SCY.name().to_lowercase().eq(&lname) { return Some(IORegister::SCY); }
        if IORegister::BGP.name().to_lowercase().eq(&lname) { return Some(IORegister::BGP); }
        if IORegister::DMA.name().to_lowercase().eq(&lname) { return Some(IORegister::DMA); }
        return None
    }
}

impl IORegister {
    pub fn get_addr(&self) -> u16 {
        match self {
            IORegister::DISABLE_BOOTROM => 0xFF50,
            IORegister::JOYPAD_P1 => 0xFF00,
            IORegister::SB => 0xFF01,
            IORegister::SC => 0xFF02,
            IORegister::DIV => 0xFF04,
            IORegister::TIMA => 0xFF05,
            IORegister::TMA => 0xFF06,
            IORegister::TAC => 0xFF07,
            IORegister::IF => 0xFF0F,
            IORegister::NR10 => 0xFF10,
            IORegister::NR11 => 0xFF11,
            IORegister::NR12 => 0xFF12,
            IORegister::NR13 => 0xFF13,
            IORegister::NR14 => 0xFF14,
            IORegister::NR21 => 0xFF16,
            IORegister::NR22 => 0xFF17,
            IORegister::NR23 => 0xFF18,
            IORegister::NR24 => 0xFF19,
            IORegister::NR30 => 0xFF1A,
            IORegister::NR31 => 0xFF1B,
            IORegister::NR32 => 0xFF1C,
            IORegister::NR33 => 0xFF1D,
            IORegister::NR34 => 0xFF1E,
            IORegister::NR41 => 0xFF20,
            IORegister::NR42 => 0xFF21,
            IORegister::NR43 => 0xFF22,
            IORegister::NR44 => 0xFF23,
            IORegister::NR50 => 0xFF24,
            IORegister::NR51 => 0xFF25,
            IORegister::NR52 => 0xFF26,
            IORegister::LCDC => 0xFF40,
            IORegister::STAT => 0xFF41,
            IORegister::SCY => 0xFF42,
            IORegister::SCX => 0xFF43,
            IORegister::LY  => 0xFF44,
            IORegister::LYC => 0xFF45,
            IORegister::DMA => 0xFF46,
            IORegister::BGP => 0xFF47,
            IORegister::OBP0 => 0xFF48,
            IORegister::OBP1 => 0xFF49,
            IORegister::WY => 0xFF4A,
            IORegister::WX => 0xFF4B,
            IORegister::IE => 0xFFFF,
        }
    }
    pub fn name(&self) -> &str {
        match self {
            IORegister::DISABLE_BOOTROM => "DISABLE_BOOTROM",
            IORegister::JOYPAD_P1 => "P1_Joypad",
            IORegister::SB   => "SB",
            IORegister::SC   => "SC",
            IORegister::DIV  => "DIV",
            IORegister::TIMA => "TIMA",
            IORegister::TMA  => "TMA",
            IORegister::TAC  => "TAC",
            IORegister::IF   => "IF",
            IORegister::NR10 => "NR10",
            IORegister::NR11 => "NR1",
            IORegister::NR12 => "NR1",
            IORegister::NR13 => "NR1",
            IORegister::NR14 => "NR1",
            IORegister::NR21 => "NR1",
            IORegister::NR22 => "NR1",
            IORegister::NR23 => "NR1",
            IORegister::NR24 => "NR1",
            IORegister::NR30 => "NR1",
            IORegister::NR31 => "NR1",
            IORegister::NR32 => "NR1",
            IORegister::NR33 => "NR1",
            IORegister::NR34 => "NR1",
            IORegister::NR41 => "NR41",
            IORegister::NR42 => "NR42",
            IORegister::NR43 => "NR1",
            IORegister::NR44 => "NR1",
            IORegister::NR50 => "NR1",
            IORegister::NR51 => "NR1",
            IORegister::NR52 => "NR1",
            IORegister::LCDC => "LCDC",
            IORegister::STAT => "STAT",
            IORegister::LY   => "LY",
            IORegister::LYC  => "LYC",
            IORegister::SCY  => "SCY",
            IORegister::SCX  => "SCX",
            IORegister::DMA  => "DMA",
            IORegister::BGP  => "BGP",
            IORegister::OBP0 => "OBP0",
            IORegister::OBP1 => "OBP1",
            IORegister::WY   => "WY",
            IORegister::WX   => "WX",
            IORegister::IE   => "IE",
        }
    }
}
pub struct MMU2 {
    cart_rom: Vec<u8>,
    boot_rom: Vec<u8>,
    mem: Vec<u8>,
    boot_rom_enabled: bool,
    pub joypad: Joypad,
    pub lcdc:LCDCRegister,
    pub stat:STATRegister,
    pub mbc: MBC,
    pub debug_registers:HashSet<IORegister>,
    pub last_write_address:u16,
}

impl MMU2 {
    pub(crate) fn init_empty(val: u8) -> MMU2 {
        let mut mem: Vec<u8> = vec![0x00; 0xFFFF + 1];
        mem.fill(val);
        MMU2 {
            cart_rom: vec![],
            boot_rom: vec![],
            mem,
            boot_rom_enabled: false,
            joypad: Joypad::init(),
            lcdc:LCDCRegister::init(),
            stat:STATRegister::init(),
            mbc: MBC::RomOnly(),
            debug_registers: Default::default(),
            last_write_address: 0
        }
    }
}

impl MMU2 {
    pub fn borrow_slice(&self, start: usize, end: usize) -> &[u8] {
        &self.mem[start..end]
    }
    pub fn borrow_range(&self, range:&MemRange) -> &[u8] {
        &self.mem[(range.start as usize) .. (range.end as usize)]
    }
}

pub enum JoypadReadMode {
    Action(),
    Direction(),
}

pub struct Joypad {
    pub a: bool,
    pub b: bool,
    pub select: bool,
    pub start: bool,
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
    pub readmode: JoypadReadMode,
}

impl Joypad {
    fn init() -> Joypad {
        Joypad {
            a: false,
            b: false,
            select: false,
            start: false,
            up: false,
            down: false,
            left: false,
            right: false,
            readmode: JoypadReadMode::Action(),
        }
    }
}

impl MMU2 {
    pub fn init(rom: &[u8], mbc: MBC) -> MMU2 {
        let mut mem: Vec<u8> = vec![0xD3; 0xFFFF + 1];
        mem.fill(0xD3);
        //copy over the cart rom
        for i in 0..rom.len() {
            mem[i] = rom[i];
        }
        MMU2 {
            boot_rom_enabled: false,
            boot_rom: fs::read("./resources/testroms/dmg_boot.bin").unwrap(),
            mem,
            cart_rom: rom.to_vec(),
            joypad: Joypad::init(),
            lcdc: LCDCRegister::init(),
            stat: STATRegister::init(),
            mbc,
            debug_registers: Default::default(),
            last_write_address: 0
        }
    }
    pub fn disable_bootrom(&mut self) {
        for i in 0..256 {
            self.mem[i] = self.cart_rom[i];
        }
        self.boot_rom_enabled = false;
    }
    pub fn enable_bootrom(&mut self) {
        //copy over the bios
        for i in 0..self.boot_rom.len() {
            self.mem[i] = self.boot_rom[i];
        }
        self.boot_rom_enabled = true;
    }

    pub fn read8(&self, addr: u16) -> u8 {
        if let Some(reg) = IORegister::match_address(addr) {
            match reg {
                IORegister::LCDC => self.lcdc.get(),
                IORegister::STAT => self.stat.get(),
                IORegister::JOYPAD_P1 => {
                    // 0 means pressed, 1 meanse released
                    match self.joypad.readmode {
                        JoypadReadMode::Action() => {
                            let mut val = 0xFF;
                            val = set_bit(val, 0, !self.joypad.a);
                            val = set_bit(val, 1, !self.joypad.b);
                            val = set_bit(val, 2, !self.joypad.select);
                            val = set_bit(val, 3, !self.joypad.start);
                            // println!("reading joypad: {:02x}", val);
                            val
                        }
                        JoypadReadMode::Direction() => {
                            let mut val = 0xFF;
                            val = set_bit(val, 0, !self.joypad.right);
                            val = set_bit(val, 1, !self.joypad.left);
                            val = set_bit(val, 2, !self.joypad.up);
                            val = set_bit(val, 3, !self.joypad.down);
                            // println!("reading joypad: {:02x}", val);
                            val
                        }
                    }
                }
                _ => self.mem[addr as usize],
            }
        } else {
            self.mem[addr as usize]
        }
    }
    pub fn read8_IO(&self, reg: &IORegister) -> u8 {
        self.read8(reg.get_addr())
    }
    pub fn write8(&mut self, addr: u16, val: u8) {
        self.last_write_address = addr;
        if let Some(en) = IORegister::match_address(addr) {
            // println!("reg {:?} <- {:02x}", en, val);
            if self.debug_registers.contains(&en) {
                println!("register {:?} set to {:02x} {:08b}",en,val, val);
            }
            match en {
                IORegister::DISABLE_BOOTROM => self.disable_bootrom(),
                IORegister::JOYPAD_P1 => {
                    // info!("writing to JOYPAD register {:08b}",val);
                    if !get_bit_as_bool(val, 5) {
                        // info!("Select Action Buttons");
                        self.joypad.readmode = JoypadReadMode::Action();
                    }
                    if !get_bit_as_bool(val, 4) {
                        // info!("Select Direction Buttons");
                        self.joypad.readmode = JoypadReadMode::Direction();
                    }
                }
                IORegister::SB => {
                    println!("wrote to the SB register {:08b} {:02x}", val, val)
                },
                IORegister::SC => {
                    if val == 0x81 {
                        let sbv = self.read8_IO(&IORegister::SB);
                        println!(
                            "print serial byte {:02x} {}",
                            sbv,
                            char::from_u32(sbv as u32).unwrap()
                        );
                    }
                }
                //writing to div resets it
                IORegister::DIV => self.mem[IORegister::DIV.get_addr() as usize] = 0,
                IORegister::DMA => self.dma_transfer(val),
                IORegister::IF => {
                    // println!("changing IF {:08b}",val);
                    let old = self.mem[addr as usize];
                    for bit in 0..5 {
                        if get_bit_as_bool(old,bit) != get_bit_as_bool(val,bit) {
                            let name = match bit {
                                0 => "VBLANK",
                                1 => "STAT",
                                2 => "TIMER",
                                3 => "SERIAL",
                                4 => "JOYPAD",
                                _ => {
                                    println!("tried to set an invalid bit to the IE register");
                                    "INVALID"
                                }
                            };
                            // println!("IF: changing {} to {}", name, get_bit_as_bool(val,bit));
                        }
                    }
                    self.mem[addr as usize] = val;
                }
                IORegister::IE => {
                    // println!("changing interrupts: {:08b}",val);
                    let old = self.mem[addr as usize];
                    for bit in 0..5 {
                        if get_bit_as_bool(old,bit) != get_bit_as_bool(val,bit) {
                            let name = match bit {
                                0 => "VBLANK",
                                1 => "STAT",
                                2 => "TIMER",
                                3 => "SERIAL",
                                4 => "JOYPAD",
                                _ => {
                                    println!("tried to set an invalid bit to the IE register");
                                    "INVALID"
                                }
                            };
                            // println!("IE: changing {} to {}", name, get_bit_as_bool(val,bit));
                        }
                    }
                    self.mem[addr as usize] = val;
                }
                IORegister::LCDC => self.lcdc.set(val),
                IORegister::STAT => self.stat.set(val),
                _ => {
                    //for registers we don't handle yet, just write as normal
                    self.mem[addr as usize] = val;
                }
            }
        } else {
            if addr >= 0x0000 && addr <= 0x7FFF {
                println!("writing to ROM. MCB type is {:?}", self.mbc);
            }
            if addr >= 0x0000 && addr <= 0x1FFF {
                println!("trying to enable external ram {:04x}, {:02x}",addr,val);
                return;
            }
            if addr >= 0x2000 && addr <= 0x3FFF {
                println!("ROM bank. switching low rom bank addr={:04x}, val={:02x}  bank number={:02x}",addr,val, (val & 0b0001_1111));
                return;
            }
            if addr >= 0x4000 && addr <= 0x5FFF {
                println!("ROM bank. switching high rom bank addr={:04x}, val={:02x}  number = {:02x}",addr,val, (val & 0b0000_0011));
                return;
            }
            if addr >= 0x6000 && addr <= 0x7FFF {
                println!("set mode addr={:04x}, val={:02x}, mode= {:02x}",addr,val, (val & 0b0000_0001));
                return;
            }
            self.mem[addr as usize] = val;
        }
    }
    pub(crate) fn write8_IO_raw(&mut self, reg: IORegister, value: u8) {
        self.mem[reg.get_addr() as usize] = value;
    }
    pub fn write8_IO(&mut self, reg: &IORegister, value: u8) {
        self.write8(reg.get_addr(),value);
    }
    pub fn read16(&self, addr: u16) -> u16 {
        let lo = self.mem[(addr + 0) as usize] as u16;
        let hi = self.mem[(addr + 1) as usize] as u16;
        (hi << 8) + lo
    }
    pub fn write16(&mut self, addr: u16, data: u16) {
        let hi = ((data & 0xFF00) >> 8) as u8;
        let lo = ((data & 0x00FF) >> 0) as u8;
        self.write8(addr+0,lo);
        self.write8(addr+1,hi);
    }
    fn dma_transfer(&mut self, val: u8) {
        // println!("DMA requested!");
        let src_addr = (val as u16) << 8;
        let src_addr_end = src_addr + 0xA0;
        // println!("transferring from src address {:04x}", src_addr);
        let dst_addr = 0xFE00;
        for n in 0..0xA0 {
            let byte = self.read8(src_addr + (n as u16));
            self.mem[(dst_addr + (n as u16)) as usize] = byte;
        }
    }

    pub fn set_IO_bit(&mut self, reg: &IORegister, bit: u8, tf: bool) {
        let val = self.read8_IO(reg);
        let val2 = set_bit(val,bit,tf);
        self.write8_IO(reg,val2);
    }
    pub fn get_IO_bit(&self, reg: &IORegister, bit:u8) -> bool {
        let val = self.read8_IO(reg);
        return get_bit_as_bool(val,bit)
    }
}
