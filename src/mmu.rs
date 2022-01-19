use crate::bootrom::BOOT_ROM;

pub struct MMU {
    inbios:bool,
    bios:Vec<u8>,
    pub data:Vec<u8>,
    // rom:Vec<u8>,
    // wram:Vec<u8>,
    // eram:Vec<u8>,
    // zram:Vec<u8>,
}

impl MMU {
    pub fn init_with_bootrom() -> MMU {
        let mut data:Vec<u8> = vec![0; 66000];
        let bios = Vec::from(BOOT_ROM);
        for i in 0..bios.len() {
            data[i] = bios[i]
        }
        MMU {
            inbios: true,
            bios: bios,
            data: data,
        }
    }
    pub fn init_with_rom_no_header(rom:&Vec<u8>) -> MMU {
        let mut data:Vec<u8> = vec![0; 66000];
        let len = rom.len();
        for i in 0..len {
            data[i] = rom[i];
        }
        let bios = Vec::from(BOOT_ROM);
        MMU {
            inbios:false,
            bios:bios,
            data:data,
        }
    }
    pub fn read8(&self, addr:u16) -> u8 {
        // println!("reading from bios at location {:04x}",addr);
        if addr >= VRAM_START  && addr <= VRAM_END {
            println!("reading from vram {:04x}",addr);
        }
        self.data[addr as usize]
    }
    pub fn read16(&self, addr:u16) -> u16 {
        // println!("reading from bios at location {:04x}",addr);
        let b1 = self.read8(addr)   as u16;
        let b2 = self.read8(addr+1) as u16;
        return b1 + (b2 << 8);
    }
    pub fn write8(&mut self, addr:u16, val:u8) {
        if addr >= VRAM_START  && addr <= VRAM_END {
            println!("writing in VRAM {:04x}  {:x}", addr, val);
        }
        if addr == 0xFF40+0x40 {
            println!("writing to turn on the LCD Display");
        }
        if addr == 0xFF47 {
            println!("writing to special LCD register")
        }
        self.data[addr as usize] = val;
    }
}

const VRAM_START:u16 = 0x8000;
const VRAM_END:u16 = 0x9FFF;
