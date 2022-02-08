use crate::common::{get_bit_as_bool, set_bit};

#[derive(Debug)]
pub struct MemRange {
    pub(crate) start:u16,
    pub(crate) end:u16,
}

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

fn is_bit_set(byte:u8, n:u8) -> bool {
    get_bit_as_bool(byte,n)
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

    pub(crate) fn get(&self) -> u8 {
        self.value
    }

    pub(crate) fn set(&mut self, byte: u8) {
        println!("setting LCDC to {:04x} {:08b}", byte, byte);
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
        self.window_display_enable = is_bit_set(byte,5);
        self.window_tilemap_select = match is_bit_set(byte,6) {
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
        println!("setting STAT to {:08b}",byte);
        self.value = byte;
        // self.vblank_interrupt_enabled = true;
        //mode bits cannot be set by programs.
        //bits 0 and 1 represent the current mode. set by the PPU
        //bit 2 is for the LYC=LY. it is read only

        if is_bit_set(byte,3) {
            println!("enabling hblank interrupts");
        } else {
            println!("disabling hblank interrupt")
        }
        self.hblank_interrupt_enabled = is_bit_set(byte,3);
        if is_bit_set(byte,4) {
            println!("enabling vblank interrupts");
        } else {
            println!("disabling vblank interrupt");
        }
        self.vblank_interrupt_enabled = is_bit_set(byte,4);
        if is_bit_set(byte,5) {
            println!("enabling sprite interrupt");
        } else {
            println!("disabling sprite interrupt");
        }
        self.sprite_interrupt_enabled = is_bit_set(byte,5);

        if is_bit_set(byte,6) {
            println!("requesting concincidnce flag");
        } else {
            println!("disabilng concidince flag");
        }

        // if is_bit_set(byte, 7) {
        //     println!("requeseting other flag");
        // } else {
        //     println!("diabing other flag");
        // }
        //writing directly to 0xFF44 (LY) by the program should reset it to 0.
        // panic!()
    }
    pub(crate) fn reset(&mut self, val: u8) {
        println!("wrote to STAT {:08b}", val);
        println!("resetting stat");
        self.value = 0;
        self.mode = LCDMode::HBlank_0;
        self.scanline_matching = false;
        self.hblank_interrupt_enabled = false;
        self.vblank_interrupt_enabled = false;
        self.sprite_interrupt_enabled = false;
        self.scanline_match_interrupt_enabled = false;
    }
    pub(crate) fn get(&self) -> u8 {
        println!("reading stat register. mdoe is {:?}",self.mode);
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

struct VerboseByte {
    b0:bool,
    b1:bool,
    b2:bool,
    b3:bool,
    b4:bool,
    b5:bool,
    b6:bool,
    b7:bool,
}

impl VerboseByte {
    pub(crate) fn to_u8(&self) -> u8 {
        let mut byte = 0;
        byte = set_bit(byte,0,self.b0);
        byte = set_bit(byte,1,self.b1);
        byte = set_bit(byte,2,self.b2);
        byte = set_bit(byte,3,self.b3);
        byte = set_bit(byte,4,self.b4);
        byte = set_bit(byte,5,self.b5);
        byte = set_bit(byte,6,self.b6);
        byte = set_bit(byte,7,self.b7);
        return byte;
    }
}