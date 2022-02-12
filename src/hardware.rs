use crate::common::{get_bit, get_bit_as_bool, is_bit_set, MemRange, VerboseByte};

#[derive(Debug)]
pub struct LCDCRegister {
    value:u8,
    pub enabled:bool, // bit 7
    pub window_tilemap_select: MemRange, // bit 6
    pub window_display_enable:bool,  // bit 5
    pub bg_window_tiledata_select: MemRange, // bit 4
    pub signed_addressing:bool, // bit 4 as well
    pub bg_tilemap_select: MemRange, // bit 3
    pub sprite_size_big: bool, // bit 2
    pub sprite_enabled:bool,   // bit 1
    pub bg_enabled: bool, // bit 0
}

impl LCDCRegister {
    pub(crate) fn init() -> LCDCRegister {
        let mut lcd = LCDCRegister {
            value: 0,
            enabled: false,
            window_tilemap_select: MemRange { start: 0, end: 0 },
            window_display_enable: false,
            bg_window_tiledata_select: MemRange { start: 0, end: 0 },
            signed_addressing: false,
            bg_tilemap_select: MemRange { start: 0, end: 0 },
            sprite_size_big: false,
            sprite_enabled: false,
            bg_enabled: false
        };
        lcd.set(0);
        lcd
    }

    pub(crate) fn reset(&mut self) {
        self.set(0);
    }
    pub(crate) fn get(&self) -> u8 {
        self.value
    }

    pub(crate) fn set(&mut self, byte: u8) {
        // println!("setting LCDC to {:08b}", byte);
        self.value = byte;
        self.bg_enabled = is_bit_set(byte, 0);
        self.sprite_enabled = is_bit_set(byte, 1);
        self.sprite_size_big = is_bit_set(byte, 2);
        self.bg_tilemap_select = match is_bit_set(byte, 3) {
            true  => MemRange{start:0x9C00, end: 0x9fff},
            false => MemRange{start:0x9800, end: 0x9BFF},
        };
        self.bg_window_tiledata_select = match is_bit_set(byte, 4) {
            true  => MemRange{start:0x8000, end: 0x9000},
            false => MemRange{start:0x8800, end: 0x9800},
        };
        self.signed_addressing = !is_bit_set(byte, 4);
        self.window_display_enable = is_bit_set(byte, 5);
        self.window_tilemap_select = match is_bit_set(byte, 6) {
            true  => MemRange{ start: 0x9C00, end: 0x9FFF },
            false => MemRange{ start: 0x9800, end: 0x8BFF },
        };
        self.enabled = is_bit_set(byte, 7);
        // println!("set LCDC {:04x} {:?}",byte,self);
    }
}

#[derive(Debug)]
pub enum LCDMode {
    HBlank_0,
    VBlank_1,
    Searching_2,
    Transferring_3,
}

#[derive(Debug)]
pub struct STATRegister {
    value:u8,
    pub mode:LCDMode, // bits 0 and 1
    pub scanline_matching:bool, // bit 2 when LYC == LY
    pub hblank_interrupt_enabled:bool,
    pub vblank_interrupt_enabled:bool,
    pub sprite_interrupt_enabled:bool,
    pub scanline_match_interrupt_enabled:bool,
}


impl STATRegister {
    pub(crate) fn init() -> STATRegister {
        let mut lcd = STATRegister {
            value: 0,
            mode: LCDMode::HBlank_0,
            scanline_matching: false,
            hblank_interrupt_enabled: false,
            vblank_interrupt_enabled: false,
            sprite_interrupt_enabled: false,
            scanline_match_interrupt_enabled: false
        };
        lcd
    }
    pub(crate) fn set(&mut self, byte:u8) {
        // println!("setting STAT to {:08b}",byte);
        self.value = byte;
        //mode bits cannot be set by programs.
        //bits 0 and 1 represent the current mode. set by the PPU
        //bit 2 is for the LYC=LY. it is read only
        if is_bit_set(byte, 3) != self.hblank_interrupt_enabled {
            println!("STAT: changed hblank interrupt to {}", is_bit_set(byte, 3));
            self.hblank_interrupt_enabled = is_bit_set(byte, 3);
        }
        if is_bit_set(byte, 4) != self.hblank_interrupt_enabled {
            println!("STAT: changed hblank interrupt to {}", is_bit_set(byte, 4));
            self.vblank_interrupt_enabled = is_bit_set(byte, 4);
        }
        if is_bit_set(byte, 5) != self.hblank_interrupt_enabled {
            println!("STAT: changed sprite interrupt to {}", is_bit_set(byte, 5));
            self.vblank_interrupt_enabled = is_bit_set(byte, 5);
        }
        if is_bit_set(byte, 6) != self.hblank_interrupt_enabled {
            println!("STAT: changed coincidence interrupt to {}", is_bit_set(byte, 6));
            self.vblank_interrupt_enabled = is_bit_set(byte, 6);
        }
    }
    pub(crate) fn reset(&mut self) {
        self.set(0);
    }
    pub(crate) fn get(&self) -> u8 {
        // println!("reading stat register. mdoe is {:?}",self.mode);
        VerboseByte {
            b0: match self.mode {
                LCDMode::HBlank_0 => false,
                LCDMode::VBlank_1 => true,
                LCDMode::Searching_2 => false,
                LCDMode::Transferring_3 => true,
            },
            b1: match self.mode {
                LCDMode::HBlank_0 => false,
                LCDMode::VBlank_1 => false,
                LCDMode::Searching_2 => true,
                LCDMode::Transferring_3 => true,
            },
            b2: false,
            b3: false,
            b4: false,
            b5: false,
            b6: false,
            b7: false
        }.to_u8()
    }
}

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
    pub(crate) fn init() -> Joypad {
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
