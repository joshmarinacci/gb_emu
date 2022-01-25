use std::cmp::{max, min};
use crate::bootrom::BOOT_ROM;
use crate::common::{get_bit, get_bit_as_bool};
use crate::Z80;

pub struct Hardware {
    pub SCY:u8,
    pub SCX:u8,
    pub LY:u8,
    pub LYC:u8,
    pub WY:u8,
    pub WX:u8,
    pub LCDC:u8,
    pub STAT:u8,
    pub IME:u8,
    pub BGP:u8,
    pub OBP0:u8,
    pub OBP1:u8,
    pub vblank_interrupt_enabled:bool,

    pub clock: i32,
    pub interrupt_ready: bool,
}

impl Hardware {
    fn init() -> Hardware {
        Hardware {
            SCY: 0,
            SCX: 0,
            LY: 0,
            LYC: 0,
            WY: 0,
            WX: 0,
            LCDC: 0,
            STAT: 0,
            IME: 0,
            BGP: 0,
            OBP0: 0,
            OBP1: 0,
            vblank_interrupt_enabled: false,
            clock: 0,
            interrupt_ready: false
        }
    }
    pub fn update(&mut self) {
        self.clock += 1;
        if self.clock % 20 == 0 {
            self.LY = self.LY + 1;
        }
        if self.LY > 153 {
            self.LY = 0;
        }
    }
}
pub struct MMU {
    inbios:bool,
    bios:Vec<u8>,
    pub data:Vec<u8>,
    lowest_used_iram:u16,
    highest_used_iram:u16,
    pub hardware:Hardware,
}

impl MMU {
    pub(crate) fn fetch_rom_bank_1(&self) -> &[u8] {
        &self.data[0x0100 .. 0x0200]
    }
    pub(crate) fn get_stack_16(&self) -> &[u8] { &self.data[0xFFF0 .. 0xFFFE] }
    pub(crate) fn get_current_bg_display_data(&self) -> &[u8] {
        let block = get_bit(self.hardware.LCDC,4);
        let (start,end) = match block {
            0 => (0x9800, 0x9BFF),
            1 => (0x9C00, 0x9FFF),
            _ => panic!("bad window tile map display select value")
        };
        &self.data[start .. end]
    }
    pub(crate) fn fetch_vram(&self) -> &[u8] {
        &self.data[(VRAM_START as usize)..(VRAM_END as usize)]
    }
    pub(crate) fn fetch_tiledata(&self) -> &[u8] {
        &self.data[0x8000 .. 0x97FF]
    }
    pub(crate) fn fetch_tiledata_block0(&self) -> &[u8] {
        &self.data[0x8000 .. 0x87FF]
    }
    pub(crate) fn fetch_tiledata_block1(&self) -> &[u8] {
        &self.data[0x8800 .. 0x8FFF]
    }
    pub(crate) fn fetch_tiledata_block2(&self) -> &[u8] {
        &self.data[0x9000 .. 0x97FF]
    }
    pub(crate) fn fetch_test_memory(&self) -> &[u8] {
        &self.data[(TEST_ADDR as usize) .. ((TEST_ADDR + 10) as usize)]
    }
    pub(crate) fn fetch_oram(&self) -> &[u8] {
        &self.data[0xFE00 .. 0xFE9F]
    }
}

impl MMU {
    pub(crate) fn update(&mut self, cpu: &mut Z80) {
        let old_LY = self.hardware.LY;
        self.hardware.update();
        if old_LY > 0 && self.hardware.LY == 0 {
            // println!("firing vblank interrupt {} {}",old_LY, self.hardware.LY);
            if self.hardware.IME > 0 && self.hardware.vblank_interrupt_enabled {
                // println!("really firing the vblank interrupt");
                cpu.dec_sp();
                cpu.dec_sp();
                // println!("writing pc {:04x} to sp {:04x} ",cpu.r.pc, cpu.r.sp);
                self.write16(cpu.r.sp, cpu.r.pc);
                cpu.set_pc(VBLANK_INTERRUPT_ADDR);
                // println!("Jumping to handler at addr {:04x}", cpu.get_pc());
            }
        }
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
    pub fn init(rom:&[u8]) -> MMU {
        let mut data:Vec<u8> = vec![0x12; 0xFFFF];
        data.fill(0x12);
        let bios = Vec::from(BOOT_ROM);
        //copy over the cart rom
        let len = rom.len();
        for i in 0..len {
            data[i] = rom[i];
        }
        MMU {
            inbios: true,
            bios,
            data,
            lowest_used_iram: INTERNAL_RAM_END,
            highest_used_iram: INTERNAL_RAM_START,
            hardware: Hardware::init(),
        }
    }
    pub(crate) fn overlay_boot(&mut self) {
        //copy over the bios
        for i in 0..self.bios.len() {
            self.data[i] = self.bios[i];
        }
    }
    pub fn read8(&self, addr:u16) -> u8 {
        // println!("reading from memory at location {:04x}",addr);
        if addr >= VRAM_START  && addr <= VRAM_END {
            // println!("reading from vram {:04x}",addr);
        }
        if addr == LY_LCDC_Y_COORD { return self.hardware.LY; }
        if addr == LYC_LCDC_Y_COMPARE { return self.hardware.LYC; }
        if addr == SCX_SCROLL_X { return self.hardware.SCX; }
        if addr == SCY_SCROLL_Y { return self.hardware.SCY; }
        if addr == STAT_LCDCONTROL {
            println!("reading STAT_LCDCONTROL register");
            return self.hardware.STAT;
        }
        self.data[addr as usize]
    }
    pub fn read16(&self, addr:u16) -> u16 {
        // println!("reading from bios at loca{tion {:04x}",addr);
        let hi = self.data[(addr + 1) as usize] as u16;
        let lo = self.data[(addr + 0) as usize] as u16;
        (hi << 8) + lo
    }
    pub fn write16(&mut self, addr:u16, data:u16) {
        // println!("writing to memory at location {:04x}",addr);
        let hi = ((data & 0xFF00) >> 8) as u8;
        let lo = ((data & 0x00FF) >> 0) as u8;
        self.data[(addr + 1) as usize] = hi;
        self.data[(addr + 0) as usize] = lo;
    }
    pub fn write8(&mut self, addr:u16, val:u8) {
        if addr < 0x8000 {
            println!("trying to write outside of RW memory {:04x} at addr {:04x}",val,addr);
            panic!("halting");
        }
        if addr == SB_REGISTER {
            println!("wrote to the SB register {:08b}", val);
            // panic!("halting");
        }
        if addr == SC_REGISTER {
            println!("wrote to the SC register {:08b}", val);
            // panic!("halting");
        }
        if addr == DIV_REGISTER {
            println!("wrote to the DIV register {:04x}", val);
            panic!("halting");
        }
        if addr == TIMA_REGISTER {
            println!("wrote to the TIMA register {:04x}", val);
            panic!("halting");
        }
        if addr == TMA_REGISTER {
            println!("wrote to the TMA register {:04x}", val);
            panic!("halting");
        }
        if addr == TAC_REGISTER {
            println!("wrote to the TAC register {:08b}", val);
            panic!("halting");
        }
        if addr == IF_INTERRUPT_FLAG {
            println!("wrote to the IF register {:08b}", val);
            if get_bit_as_bool(val,0) {
                println!("enabling vblank");
                self.hardware.vblank_interrupt_enabled = true;
            }
            // panic!("halting");
        }
        if addr >= VRAM_START  && addr <= VRAM_END {
            // println!("writing in VRAM {:04x}  {:x}", addr, val);
        }
        if addr == LCDC_LCDCONTROL {
            // println!("writing to turn on the LCD Display");
            println!("writing to LCDC register {:0b}",val);
            self.hardware.LCDC = val;
            // let b3 = get_bit(self.hardware.LCDC,3);
            // println!("bit 3 is now {}",b3);
            // dump_LCDC_bits(self.hardware.LCDC);
        }
        if addr == STAT_LCDCONTROL {
            println!("writing to STAT LCD register {:0b}",val);
            self.hardware.STAT = val;
            // panic!("halting");
        }
        if addr == DMA {
            println!("DMA requested!");
            panic!("halting");
        }
        if addr == INTERRUPT_ENABLE {
            println!("setting interrupt enable to {:08b}",val);
            if get_bit_as_bool(val,0) {
                println!("enabled vblank");
                self.hardware.vblank_interrupt_enabled = true;
            }
            return;
        }
        if addr == BGP {
            //this is the background color palette
            //https://gbdev.gg8.se/wiki/articles/Video_Display#FF47_-_BGP_-_BG_Palette_Data_.28R.2FW.29_-_Non_CGB_Mode_Only
            println!("writing to BGP LCD register {:0b}",val);
            self.hardware.BGP = val;
            dump_bgp_bits(val);
        }
        if addr == OBP0_ADDR  { self.hardware.OBP0 = val; }
        if addr == OBP1_ADDR  { self.hardware.OBP1 = val; }
        if addr == WX_ADDR  { self.hardware.WX = val; }
        if addr == WY_ADDR  { self.hardware.WY = val; }
        if addr == SCX_SCROLL_X { self.hardware.SCX = val; }
        if addr == SCY_SCROLL_Y { self.hardware.SCY = val; }
        if addr >= INTERNAL_RAM_START && addr <= INTERNAL_RAM_END {
            // println!("writing to internal ram:  {:04x} := {:02x}",addr, val);
            self.lowest_used_iram = min(self.lowest_used_iram,addr);
            self.highest_used_iram = max(self.highest_used_iram,addr);
        }
        self.data[addr as usize] = val;
    }
}

fn dump_bgp_bits(byt: u8) {
    for n in 0..8 {
        let b = get_bit(byt,n);
        let v:String = match n {
            0 => format!("data for dot data 00 = {}",b),
            1 => format!("data for dot data 00 = {}",b),
            2 => format!("data for dot data 01 = {}",b),
            3 => format!("data for dot data 01 = {}",b),
            4 => format!("data for dot data 10 = {}",b),
            5 => format!("data for dot data 10 = {}",b),
            6 => format!("data for dot data 11 = {}",b),
            7 => format!("data for dot data 11 = {}",b),
            _ => "Unknown so far".to_string(),
        };
        println!("LCDC: {}",v);
    }
}

fn dump_lcdc_bits(by: u8) {
    for n in 0..8 {
        let b = get_bit(by,n);
        let v:String = match n {
            0 => format!("BG/Window Display/Priority = {}",b),
            1 => format!("OBJ Display Enable = {}",b),
            2 => format!("OBJ Size (8x8 vs 8x16)= {}",b),
            3 => format!("BG Tile Map Display Select = {}",b),
            4 => format!("BG & Window Tile Data Select = {}",b),
            5 => format!("Window Display Enable = {}",b),
            6 => format!("Window Tile Map Display Select = {}",b),
            7 => format!("LCD Display Enable = {}",b),
            _ => "Unknown so far".to_string(),
        };
        println!("LCDC: {}",v);
    }
}

pub const VRAM_START:u16 = 0x8000;
pub const VRAM_END:u16 = 0x9FFF;

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
const WX_ADDR:u16 = 0xFF4A;
const WY_ADDR:u16 = 0xFF4B;
const OBP0_ADDR:u16 = 0xFF48;
const OBP1_ADDR:u16 = 0xFF49;
pub const TEST_ADDR:u16 = 0xA000;

const LY_LCDC_Y_COORD:u16 = 0xFF44;
const LYC_LCDC_Y_COMPARE:u16 = 0xFF45;

const DMA:u16 = 0xFF46;
const BGP:u16 = 0xFF47; // BG Palette Data (R/W)
const OBP0:u16 = 0xFF48; // Object Palette 0 Data (R/W)
const OBP1:u16 = 0xFF49; // Object Palette 1 Data (R/W)

const INTERRUPT_ENABLE:u16 = 0xffff;

const VBLANK_INTERRUPT_ADDR:u16 = 0x40;
