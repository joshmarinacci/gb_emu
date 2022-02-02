use std::collections::HashMap;
use std::fs;
use std::path::Path;


/*
IO registers
- [ ] print when read or written to
- [ ] pretty print the names when matching their addresses
- [ ] be able to dump them all easily as a group in a loop (array of HW vec enums?) (func on the Hardware object?)
- [ ] make all access to the HW registers through functions, not direct sets.
- [ ] then test loading tetris again to see if it works.

 */

pub enum IORegister {
    LY,
    LCDC,
    STAT,
    SCY,
    SCX,
    DISABLE_BOOTROM,
}

impl IORegister {
    pub fn match_address(addr: u16) -> Option<IORegister> {
        match addr {
            0xFF44 => Some(IORegister::LY),
            _ => None
        }
    }
}

impl IORegister {
    pub fn get_addr(&self) -> u16 {
        match self {
            IORegister::LY    => 0xFF44,
            IORegister::LCDC  => 0xFF40,
            IORegister::STAT  => 0xFF41,
            IORegister::SCY   => 0xFF42,
            IORegister::SCX   => 0xFF43,
            IORegister::DISABLE_BOOTROM => 0xFF50,
        }
    }
    pub fn name(&self) -> &str {
        match self {
            IORegister::LY => "LY",
            IORegister::LCDC => "LCDC",
            IORegister::STAT => "STAT",
            IORegister::SCY => "SCY",
            IORegister::SCX => "SCX",
            IORegister::DISABLE_BOOTROM => "DISABLE_BOOTROM",
        }
    }
}
pub struct MMU2 {
    cart_rom:Vec<u8>,
    boot_rom:Vec<u8>,
    mem:Vec<u8>,
    boot_rom_enabled:bool,
}


impl MMU2 {
    pub fn init(rom: &[u8]) -> MMU2 {
        let mut data: Vec<u8> = vec![0x12; (0xFFFF + 1)];
        data.fill(0x12);
        //copy over the cart rom
        for i in 0..rom.len() {
            data[i] = rom[i];
        }
        MMU2 {
            boot_rom_enabled: false,
            boot_rom: fs::read("./resources/testroms/dmg_boot.bin").unwrap(),
            mem: data,
            cart_rom: rom.to_vec(),
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
        self.mem[addr as usize]
    }
    pub(crate) fn read8_IO(&self, reg:IORegister) -> u8 {
        self.mem[reg.get_addr() as usize]
    }
    pub fn write8(&mut self, addr:u16, val:u8) {
        if let Some(en) = IORegister::match_address(addr) {
            match en {
                IORegister::LY => {}
                IORegister::LCDC => {}
                IORegister::STAT => {}
                IORegister::SCY => {}
                IORegister::SCX => {}
                IORegister::DISABLE_BOOTROM => self.disable_bootrom()
            }
        } else {
            self.mem[addr as usize] = val;
        }
    }
    pub(crate) fn write8_IO(&mut self, reg: IORegister, value: u8) {
        self.mem[reg.get_addr() as usize] = value;
    }
    pub fn read16(&self, addr:u16) -> u16 {
        let lo = self.mem[(addr + 0) as usize] as u16;
        let hi = self.mem[(addr + 1) as usize] as u16;
        (hi << 8) + lo
    }
    pub fn write16(&mut self, addr:u16, data:u16) {
        let hi = ((data & 0xFF00) >> 8) as u8;
        let lo = ((data & 0x00FF) >> 0) as u8;
        self.mem[(addr + 0) as usize] = lo;
        self.mem[(addr + 1) as usize] = hi;
    }
}

