use std::collections::HashSet;
use crate::common::{get_bit_as_bool, MBC, MemRange, set_bit};
use log::info;
use std::fs;
use crate::hardware::{IORegister, Joypad, JoypadReadMode, LCDCRegister, STATRegister};

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
                IORegister::SB => 0xFF, //always return FF so that game thinks there is no other player
                _ => self.mem[addr as usize],
            }
        } else {
            self.mem[addr as usize]
        }
    }
    pub fn read8_IO(&self, reg: IORegister) -> u8 {
        self.read8(reg as u16)
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
                        let sbv = self.read8_IO(IORegister::SB);
                        println!(
                            "print serial byte {:02x} {}",
                            sbv,
                            char::from_u32(sbv as u32).unwrap()
                        );
                    }
                }
                //writing to div resets it
                IORegister::DIV => self.mem[IORegister::DIV as u16 as usize] = 0,
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
        self.mem[reg as u16 as usize] = value;
    }
    pub fn write8_IO(&mut self, reg: IORegister, value: u8) {
        self.write8(reg as u16,value);
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

    pub fn set_IO_bit(&mut self, reg: IORegister, bit: u8, tf: bool) {
        let val = self.read8_IO(reg);
        let val2 = set_bit(val,bit,tf);
        self.write8_IO(reg,val2);
    }
    pub fn get_IO_bit(&self, reg: IORegister, bit:u8) -> bool {
        let val = self.read8_IO(reg);
        return get_bit_as_bool(val,bit)
    }
}
