use std::cmp::{max, min};
use crate::bootrom::BOOT_ROM;

pub struct Hardware {
    ly:u8,
}

impl Hardware {
    fn init() -> Hardware {
        Hardware {
            ly: 0,
        }
    }
    pub fn update(&mut self) {
        self.ly = self.ly + 1;
        if self.ly > 153 {
            self.ly = 0
        }
        println!("LY is {}",self.ly);
    }
}
pub struct MMU {
    inbios:bool,
    bios:Vec<u8>,
    pub data:Vec<u8>,
    // rom:Vec<u8>,
    // wram:Vec<u8>,
    // eram:Vec<u8>,
    // zram:Vec<u8>,

    lowest_used_iram:u16,
    highest_used_iram:u16,
    hardware:Hardware,
}

impl MMU {
    pub(crate) fn update(&mut self) {
        self.hardware.update();
    }
}

impl MMU {
    pub(crate) fn print_cram(&self)  {
        println!("----");
        println!("lowest and heighest {:04x} {:04x}",self.lowest_used_iram, self.highest_used_iram);
        let iram:&[u8] = &self.data[(self.lowest_used_iram as usize)..(self.highest_used_iram as usize)];
        let mut pc = self.lowest_used_iram;
        for ch in iram.chunks_exact(16) {
            println!("PC {:04x} = {:x?}",pc, ch);
            pc+=16;
        }
        // for i in  {
        //     print!("{:0x}",self.data[i as usize]);
        //     if i % 0x2F == 0 {
        //         println!("")
        //     }
        // }
        println!("----");
    }
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
            lowest_used_iram: INTERNAL_RAM_END,
            highest_used_iram: INTERNAL_RAM_START,
            hardware: Hardware::init(),
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
            lowest_used_iram: INTERNAL_RAM_END,
            highest_used_iram: INTERNAL_RAM_START,
            hardware: Hardware::init(),
        }
    }
    pub fn read8(&self, addr:u16) -> u8 {
        // println!("reading from bios at location {:04x}",addr);
        if addr >= VRAM_START  && addr <= VRAM_END {
            println!("reading from vram {:04x}",addr);
        }
        if addr == LY_LCDC_Y_COORD {
            // println!("reading LY LCDC_Y_COORD");
            return self.hardware.ly;
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
        if addr == SB_REGISTER {
            println!("wrote to the SB register {:04x}", val);
            panic!("halting");
        }
        if addr == SC_REGISTER {
            println!("wrote to the SC register {:04x}", val);
            panic!("halting");
        }
        if addr >= VRAM_START  && addr <= VRAM_END {
            println!("writing in VRAM {:04x}  {:x}", addr, val);
        }
        if addr == LCDC_LCDCONTROL {
            println!("writing to turn on the LCD Display");
        }
        if addr == STAT_LCDCONTROL {
            println!("writing to stat the LCD Display");
        }
        if addr == 0xFF47 {
            println!("writing to special LCD register")
        }
        if addr >= INTERNAL_RAM_START && addr <= INTERNAL_RAM_END {
            println!("writing to internal ram:  {:04x} := {:x}",addr, val);
            self.lowest_used_iram = min(self.lowest_used_iram,addr);
            self.highest_used_iram = max(self.highest_used_iram,addr);
        }
        self.data[addr as usize] = val;
    }
}

const VRAM_START:u16 = 0x8000;
const VRAM_END:u16 = 0x9FFF;

const INTERNAL_RAM_START:u16 = 0xC000;
const INTERNAL_RAM_END:u16 = 0xDFFF;

const P1_JOYPAD_INFO:u16 = 0xFF00; // P1
const SB_REGISTER:u16 = 0xFF01;
const SC_REGISTER:u16 = 0xFF02;
const DIV_REGISTER:u16 = 0xff04;
const TIMA_REGISTER:u16 = 0xff05;
const TMA_REGISTER:u16 = 0xff06;
const TAC_REGISTER:u16 = 0xff07;
const IF_INTERRUPT_FLAG:u16 = 0xff0f;
const NR10_SOUND:u16 = 0xFF10;
const NR11_SOUND:u16 = 0xFF11;
const NR12_SOUND:u16 = 0xFF12;
const NR13_SOUND:u16 = 0xFF13;
const NR14_SOUND:u16 = 0xFF14;
const NR16_SOUND:u16 = 0xFF16;
//more sound stuff

const LCDC_LCDCONTROL:u16 = 0xFF40;
const STAT_LCDCONTROL:u16 = 0xFF41;

const SCY_SCROLL_Y:u16 = 0xFF42;
const SCX_SCROLL_X:u16 = 0xFF43;

const LY_LCDC_Y_COORD:u16 = 0xFF44;
const LYC_LCDC_Y_COMPARE:u16 = 0xFF45;

const DMA:u16 = 0xFF46;
const BGP:u16 = 0xFF47;

const INTERRUPT_ENABLE:u16 = 0xffff;

