use std::cmp::{max, min};
use log::info;
use crate::bootrom::BOOT_ROM;
use crate::common::{get_bit, get_bit_as_bool};
use crate::Z80;

#[derive(Debug)]
pub enum InterruptType {
    VBlank,
    Timer,
    Serial,
    None,
}
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
    pub IE:u8,
    pub IF:u8,
    pub BGP:u8,
    pub OBP0:u8,
    pub OBP1:u8,

    pub SB:u8,

    pub TIMA:u8,
    pub TMA:u8,
    pub DIV:u8,
    pub TAC:u8,

    pub vblank_interrupt_enabled:bool,
    pub timer_interrupt_enabled:bool,
    pub lcdc_interrupt_enabled:bool,
    pub serial_interrupt_enabled:bool,
    pub transition_interrupt_enabled:bool,
    pub interrupt_just_returned:bool,
    pub active_interrupt:InterruptType,

    pub clock: i32,
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
            IE: 0,
            IF: 0,
            BGP: 0,
            OBP0: 0,
            OBP1: 0,
            vblank_interrupt_enabled: false,
            timer_interrupt_enabled: false,
            lcdc_interrupt_enabled: false,
            serial_interrupt_enabled: false,
            transition_interrupt_enabled: false,
            clock: 0,
            active_interrupt: InterruptType::None,
            interrupt_just_returned: false,
            TIMA:0,
            TMA:0,
            DIV:0,
            TAC:0,
            SB: 0
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
        let (v2, overflowed) = self.DIV.overflowing_add(1);
        if overflowed {
            // println!("div overflowed");
        }
        self.DIV = v2;
    }
}
pub struct MMU {
    inbios:bool,
    bios:Vec<u8>,
    pub data:Vec<u8>,
    lowest_used_iram:u16,
    highest_used_iram:u16,
    pub hardware:Hardware,
    pub refresh_requested:bool,
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

    pub(crate) fn update(&mut self, cpu: &mut Z80) {
        let old_LY = self.hardware.LY;
        self.hardware.update();

        if self.hardware.IME == 1  && self.hardware.interrupt_just_returned {
            self.hardware.interrupt_just_returned = false;
            match self.hardware.active_interrupt {
                InterruptType::VBlank => self.refresh_requested = true,
                InterruptType::Timer => {}
                InterruptType::Serial => {}
                InterruptType::None => {}
            }
            self.hardware.active_interrupt = InterruptType::None;
        }
        //vblank
        if old_LY > 0 && self.hardware.LY == 0 {
            if self.hardware.IME > 0 && self.hardware.vblank_interrupt_enabled {
                self.hardware.active_interrupt = InterruptType::VBlank;
                self.hardware.IME = 0;
                cpu.dec_sp();
                cpu.dec_sp();
                self.write16(cpu.r.sp, cpu.r.pc);
                cpu.set_pc(VBLANK_INTERRUPT_ADDR);
            } else {
                //otherwise just trigger a normal refresh?
                self.refresh_requested = true;
            }
        }

        // if timer is enabled
        if get_bit_as_bool(self.hardware.TAC,2) {
            if self.hardware.clock % 5 == 0 {
                let (val, overflowed) = self.hardware.TIMA.overflowing_add(1);
                self.hardware.TIMA = val;
                if overflowed {
                    self.hardware.TIMA = self.hardware.TMA;
                    if self.hardware.IME > 0 && self.hardware.timer_interrupt_enabled {
                        self.hardware.IME = 0;
                        self.hardware.active_interrupt = InterruptType::Timer;
                        cpu.dec_sp();
                        cpu.dec_sp();
                        self.write16(cpu.r.sp, cpu.r.pc);
                        cpu.set_pc(TIMER_INTERRUPT_ADDR);
                    }
                }
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
            refresh_requested: false
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
            // println!("reading STAT_LCDCONTROL register");
            return self.hardware.STAT;
        }
        if addr == INTERRUPT_ENABLE {
            return self.hardware.IE;
        }
        // println!("reading from address {:04x}",addr);
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
        // info!("writing {:02x} at {:04x}",val,addr);
        if addr < 0x8000 {
            if addr >= 0x2000 && addr <= 0x3FFF {
                info!("writing to ROM Bank Number {:04x}",val);
                return;
            }
            println!("trying to write outside of RW memory {:04x} at addr {:04x}",val,addr);
            panic!("halting");
        }
        if addr == P1_JOYPAD_INFO {
            info!("writing to JOYPAD register {:08b}",val);
            return;
        }

        if addr == SB_REGISTER {
            // info!("wrote to the SB register {:08b} {:02x}", val, val);
            self.hardware.SB = val;
            // panic!("halting");
            return;
        }
        if addr == SC_REGISTER {
            // info!("wrote to the SC register {:08b} {:02x}", val, val);
            if val == 0x81 {
                info!("print serial byte {:02x}", self.hardware.SB)
            }
            // panic!("halting");
            return;
        }
        if addr == DIV_REGISTER {
            info!("wrote to the DIV register {:04x}", val);
            self.hardware.DIV = 0;
            return;
        }
        if addr == TIMA_REGISTER {
            println!("wrote to the TIMA register {:04x}", val);
            panic!("halting");
        }
        if addr == TMA_REGISTER {
            info!("wrote to the TMA register {:04x}", val);
            self.hardware.TMA = val;
            return;
        }
        if addr == TAC_REGISTER {
            info!("wrote to the TAC register {:08b}", val);
            self.hardware.TAC = val;
            return;
        }
        if addr >= VRAM_START  && addr <= VRAM_END {
            // println!("writing in VRAM {:04x}  {:x}", addr, val);
        }
        if addr >= INTERNAL_RAM_HI_START && addr <= INTERNAL_RAM_HI_END {
            // println!("writing to hi ram {:04x} value = {:02x}",addr,val);
        }
        if addr == LCDC_LCDCONTROL {
            // println!("writing to turn on the LCD Display");
            info!("writing to LCDC register {:08b}",val);
            self.hardware.LCDC = val;
            // let b3 = get_bit(self.hardware.LCDC,3);
            // println!("bit 3 is now {}",b3);
            // dump_LCDC_bits(self.hardware.LCDC);
            return;
        }
        if addr == STAT_LCDCONTROL {
            info!("writing to STAT LCD register {:0b}",val);
            self.hardware.STAT = val;
            return;
            // panic!("halting");
        }
        if addr == DMA {
            // println!("DMA requested!");
            let src_addr = ((val as u16) << 8);
            let src_addr_end = src_addr + 0xA0;
            // println!("transferring from src address {:04x}",src_addr);
            let dst_addr = 0xFE00;
            // let src_data = &self.data[(src_addr as usize)..(src_addr_end as usize)];
            for n in 0..0xA0 {
                let byte = self.read8(src_addr + (n as u16));
                self.write8(dst_addr + (n as u16),byte);
            }
            info!("DMA transfer complete");
            return;
        }
        if addr == INTERRUPT_ENABLE {
            // println!("wrote to the IE register. {:08b}",val);
            self.hardware.vblank_interrupt_enabled = get_bit_as_bool(val,0);
            self.hardware.lcdc_interrupt_enabled = get_bit_as_bool(val,1);
            self.hardware.timer_interrupt_enabled = get_bit_as_bool(val,2);
            self.hardware.serial_interrupt_enabled = get_bit_as_bool(val, 3);
            self.hardware.transition_interrupt_enabled = get_bit_as_bool(val,4);
            self.hardware.IE = val;
            return;
        }
        if addr == IF_INTERRUPT_FLAG {
            self.hardware.IF = val;
            return;
        }
        if addr == BGP {
            // info!("writing to BGP LCD register {:0b}",val);
            self.hardware.BGP = val;
            // dump_bgp_bits(val);
            return;
        }
        if addr == OBP0_ADDR  { self.hardware.OBP0 = val; return; }
        if addr == OBP1_ADDR  { self.hardware.OBP1 = val; return; }
        if addr == WX_ADDR  { self.hardware.WX = val; return; }
        if addr == WY_ADDR  { self.hardware.WY = val; return; }
        if addr == SCX_SCROLL_X { self.hardware.SCX = val; return; }
        if addr == SCY_SCROLL_Y { self.hardware.SCY = val; return; }
        if addr >= INTERNAL_RAM_START && addr <= INTERNAL_RAM_END {
            // println!("writing to internal ram:  {:04x} := {:02x}",addr, val);
            self.lowest_used_iram = min(self.lowest_used_iram,addr);
            self.highest_used_iram = max(self.highest_used_iram,addr);
        }
        let arr = [
            NR10_SOUND, NR11_SOUND, NR12_SOUND, NR13_SOUND, NR14_SOUND,
            NR21_SOUND, NR22_SOUND, NR23_SOUND, NR24_SOUND,
            NR30_SOUND, NR31_SOUND, NR32_SOUND, NR33_SOUND, NR34_SOUND,
            NR42_SOUND, NR44_SOUND,
            NR50_SOUND, NR51_SOUND, NR52_SOUND];
        if arr.contains(&addr) {
            // println!("setting sound register {:04x} to {:02x}",addr,val);
            return;
        }
        if addr >= 0xFF00 && addr < 0xFF80 {
            println!("trying to write to the registers area {:04x} {:02}",addr, val);
            return;
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

const INTERNAL_RAM_HI_START:u16 = 0xFF80;
const INTERNAL_RAM_HI_END:u16   = 0xFFFE;

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
const NR21_SOUND:u16 = 0xFF16;
const NR22_SOUND:u16 = 0xFF17;
const NR23_SOUND:u16 = 0xFF18;
const NR24_SOUND:u16 = 0xFF19;

const NR30_SOUND:u16 = 0xFF1A;
const NR31_SOUND:u16 = 0xFF1B;
const NR32_SOUND:u16 = 0xFF1C;
const NR33_SOUND:u16 = 0xFF1D;
const NR34_SOUND:u16 = 0xFF1E;

const NR42_SOUND:u16 = 0xFF21;
const NR44_SOUND:u16 = 0xFF23;
const NR50_SOUND:u16 = 0xFF24;
const NR51_SOUND:u16 = 0xFF25;
const NR52_SOUND:u16 = 0xFF26;
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

const DMA:u16 = 0xFF46; // DMA Start and Transfer address
const BGP:u16 = 0xFF47; // BG Palette Data (R/W)
const OBP0:u16 = 0xFF48; // Object Palette 0 Data (R/W)
const OBP1:u16 = 0xFF49; // Object Palette 1 Data (R/W)

const INTERRUPT_ENABLE:u16 = 0xffff;

const VBLANK_INTERRUPT_ADDR:u16 = 0x40;
pub const TIMER_INTERRUPT_ADDR:u16  = 0x50;
const STAT_INTERRUPT_ADDR:u16   = 0x48;

/*

DMA:
* when dma memory register is written to, begin a DMA transfer
* lock access to all memory except for FF80 and above
* look up the argument to calculate the source address. every $100 from $0000-$F100
* calculate dest address in the OAM area ($FE00-$FE9F)
* copy over the memory
* set the correct status bits during DMA
* add function to dump the OAM area. 40 x 4B blocks. for each one is y,x,tileid, flags


* for Test2 app, sprites stored at 0xC000 and must be copied to OAM area to be drawn
* verify that OAM area contains the sprite info

* update drawing code to draw the sprites later using the OAM table.
* scan OAM table looking for sprites that intersect the current line. pick the first 10
* composite the sprite bits with the background tile bits
* test that I can actually see the sprite now.


* implement the joypad interrupts so that Test 2 can

 */
