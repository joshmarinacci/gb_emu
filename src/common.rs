use std::fs;
use std::fs::File;
use std::io::BufWriter;
use std::io::Result;
use std::path::{Path, PathBuf};

pub struct RomFile {
    pub data: Vec<u8>,
    pub(crate) path: String,
}

pub fn get_bit(by: u8, n: u8) -> u8 {
    if by & (1 << n) != 0 {
        1
    } else {
        0
    }
}
pub fn get_bit_as_bool(by: u8, n: u8) -> bool {
    by & (1 << n) != 0
}
pub fn set_bit(by: u8, n: u8, on: bool) -> u8 {
    if on {
        let mask = match n {
            0 => 0b0000_0001,
            1 => 0b0000_0010,
            2 => 0b0000_0100,
            3 => 0b0000_1000,
            4 => 0b0001_0000,
            5 => 0b0010_0000,
            6 => 0b0100_0000,
            7 => 0b1000_0000,
            _ => {
                panic!("we can't look at bits higher than 7")
            }
        };
        by | mask
    } else {
        let mask = match n {
            0 => 0b1111_1110,
            1 => 0b1111_1101,
            2 => 0b1111_1011,
            3 => 0b1111_0111,
            4 => 0b1110_1111,
            5 => 0b1101_1111,
            6 => 0b1011_1111,
            7 => 0b0111_1111,
            _ => {
                panic!("we can't look at bits higher than 7")
            }
        };
        by & mask
    }
}

pub struct Bitmap {
    pub(crate) data: Vec<u8>,
    pub(crate) w: i32,
    pub(crate) h: i32,
}

impl Bitmap {
    pub(crate) fn clear_with(&mut self, r: u8, g: u8, b: u8) {
        for i in 0..self.w {
            for j in 0..self.h {
                self.set_pixel_rgb(i, j, r, g, b);
            }
        }
    }
}

impl Bitmap {
    pub fn write_to_file(&self, filename: &str) {
        let path = Path::new(filename);
        let file = File::create(path).unwrap();
        let w = BufWriter::new(file);

        let mut encoder = png::Encoder::new(w, self.w as u32, self.h as u32);
        encoder.set_color(png::ColorType::Rgba);
        encoder.set_depth(png::BitDepth::Eight);

        let mut writer = encoder.write_header().unwrap();
        writer.write_image_data(self.data.as_slice()).unwrap();
    }
}

impl Bitmap {
    pub(crate) fn set_pixel_rgb(&mut self, x: i32, y: i32, r: u8, g: u8, b: u8) {
        if x < 0 {
            return;
        }
        if y < 0 {
            return;
        }
        if x > self.w - 1 {
            return;
        }
        if y > self.h - 1 {
            return;
        }
        let n: usize = ((x + self.w * y) * 4) as usize;
        // println!("n is {}",n);
        self.data[n + 0] = r;
        self.data[n + 1] = g;
        self.data[n + 2] = b;
        self.data[n + 3] = 255;
    }
    pub(crate) fn get_pixel_rgb(&self, x: i32, y: i32) -> (u8, u8, u8) {
        let n: usize = ((x + self.w * y) * 4) as usize;
        (self.data[n + 0], self.data[n + 1], self.data[n + 2])
    }
}

impl Bitmap {
    pub fn init(w: i32, h: i32) -> Bitmap {
        let mut data: Vec<u8> = Vec::with_capacity((w * h * 4) as usize);
        data.resize((w * h * 4) as usize, 255);
        data.fill(255);
        println!("Length is {}", data.len());
        Bitmap { w, h, data }
    }
}

pub struct HWReg {
    pub(crate) addr: u16,
    pub(crate) name: &'static str,
    pub(crate) value: u8,
}
pub const LCDC: HWReg = HWReg {
    addr: 0xFF40,
    name: "LCDC",
    value: 0,
};
pub const STAT: HWReg = HWReg {
    addr: 0xFF41,
    name: "STAT",
    value: 0,
};
pub const LY: HWReg = HWReg {
    addr: 0xFF44,
    name: "LY",
    value: 0,
};
pub const SCY: HWReg = HWReg {
    addr: 0xFF42,
    name: "SCY",
    value: 0,
};
pub const SCX: HWReg = HWReg {
    addr: 0xFF43,
    name: "SCX",
    value: 0,
};

impl HWReg {
    pub fn register_from_addr(addr: u16) -> Option<HWReg> {
        if addr == LCDC.addr {
            return Some(LCDC);
        }
        if addr == STAT.addr {
            return Some(STAT);
        }
        if addr == LY.addr {
            return Some(LY);
        }
        if addr == SCX.addr {
            return Some(SCX);
        }
        if addr == SCY.addr {
            return Some(SCY);
        }
        return None;
    }
}

#[derive(Debug)]
pub enum JoyPadKey {
    A,
    B,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right,
}
#[derive(Debug)]
pub enum InputEvent {
    Press(JoyPadKey),
    Release(JoyPadKey),
    Stop(),
    JumpNextVBlank(),
}

pub fn load_romfile(pth: &PathBuf) -> Result<RomFile> {
    let pth2: String = pth.as_path().to_str().unwrap().parse().unwrap();
    let data: Vec<u8> = fs::read(pth)?;
    println!(
        "0x0104. start of Nintendo graphic {:02X} {:02X} (should be CE ED)",
        data[0x0104], data[0x0105]
    );
    print!("name = ");
    for ch in 0x0134..0x0142 {
        print!("{}", char::from_u32(data[ch] as u32).unwrap());
    }
    println!("   ");
    // println!("0x0134. start of name {:?}",data[0x0134..0x0142]);
    println!("0x0143 color or not {:02x}", data[0x0143]);
    println!("0x0146 SGB indicator {:02x}", data[0x0146]);
    println!("0x0147 cart type {:02x}", data[0x0147]);
    println!("0x0148 ROM size {:02x}", data[0x0148]);
    println!("0x0149 RAM size {:02x}", data[0x0149]);
    println!("0x014A dest code {:02x}", data[0x014A]);

    let cart_type = data[0x0147];
    match cart_type {
        0x0 => println!("ROM only. Great!"),
        0x1..=0x3 => println!("MBC1! Not supported!"),
        0x5 | 0x6 => println!("MBC2! Not supported!"),
        0x12 | 0x13 => println!("MBC3! Not supported!"),
        0x19 | 0x1A | 0x1B | 0x1C | 0x1D | 0x1E => println!("MBC5! Not supported!"),
        0x1F => println!("Bocket Camera, unsupported!"),
        0xFD => println!("Bocket Camera, unsupported!"),
        0xFE | 0xFF => println!("Hudson HuC, unsupported!"),
        _ => {
            println!("trying anyway");
        }
    }
    Ok(RomFile { data, path: pth2 })
}


pub fn print_ram(base:u16, ram: &Vec<u8>)  {
    for (n, chunk) in ram.chunks(16).enumerate() {
        let line_str: String = chunk.iter().map(|b| format!("{:02x} ", b)).collect();
        println!("{:04X} {}", ((n * 16) as u16 + base), line_str);
    }
}
