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
    pub fn init() -> MMU {
        MMU {
            inbios: true,
            bios: Vec::from(BOOT_ROM),
            // wram: vec![u8; 8192],
            // eram: vec![u8; 8192],
            // zram: vec![u8; 127],
            // rom: vec![u8; 0],
            data: vec![0;66000],
        }
    }
    pub fn read8(&self, addr:u16) -> u8 {
        // println!("reading from bios at location {:04x}",addr);
        self.bios[addr as usize]
    }
    pub fn read16(&self, addr:u16) -> u16 {
        // println!("reading from bios at location {:04x}",addr);
        let b1 = self.read8(addr)   as u16;
        let b2 = self.read8(addr+1) as u16;
        return b1 + (b2 << 8);
    }
    pub fn write8(&mut self, addr:u16, val:u8) {
        if addr >= 0x8000  && addr <= 0x9FFF {
            // println!("writing in VRAM {:04x}  {:x}", addr, val);
        }
        if addr == 0xFF47 {
            println!("writing to special LCD register")
        }
        self.data[addr as usize] = val;
    }
}
