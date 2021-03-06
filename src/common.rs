use std::fs;
use std::fs::File;
use std::io::BufWriter;
use std::io::Result;
use std::path::{Path, PathBuf};

pub struct RomFile {
    pub data: Vec<u8>,
    pub path: String,
    pub mbc: MBC,
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
        // println!("Length is {}", data.len());
        Bitmap { w, h, data }
    }
}


#[derive(Debug)]
pub enum MBC {
    RomOnly(),
    MBC1(),
    MBC2(),
    MBC3(),
    MBC5(),
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
    Break(),
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
    let mbc = match cart_type {
        0x0 => MBC::RomOnly(),
        0x1..=0x3 => MBC::MBC1(),
        0x5 | 0x6 => MBC::MBC2(),
        0x12 | 0x13 => MBC::MBC3(),
        0x19 | 0x1A | 0x1B | 0x1C | 0x1D | 0x1E => MBC::MBC5(),
        0x1F => panic!("Bocket Camera, unsupported!"),
        0xFD => panic!("Bocket Camera, unsupported!"),
        0xFE | 0xFF => panic!("Hudson HuC, unsupported!"),
        _ => {
            panic!("unknown cart MBC type");
        }
    };
    Ok(RomFile { data, path: pth2, mbc:mbc, })
}


pub fn print_ram(base:u16, ram: &Vec<u8>)  {
    for (n, chunk) in ram.chunks(16).enumerate() {
        let line_str: String = chunk.iter().map(|b| format!("{:02x} ", b)).collect();
        println!("{:04X} {}", ((n * 16) as u16 + base), line_str);
    }
}

pub fn u8_as_i8(v: u8) -> i8 {
    v as i8
}

#[derive(Debug)]
pub struct MemRange {
    pub(crate) start:u16,
    pub(crate) end:u16,
}

pub fn is_bit_set(byte:u8, n:u8) -> bool {
    get_bit_as_bool(byte,n)
}

pub struct VerboseByte {
    pub(crate) b0:bool,
    pub(crate) b1:bool,
    pub(crate) b2:bool,
    pub(crate) b3:bool,
    pub(crate) b4:bool,
    pub(crate) b5:bool,
    pub(crate) b6:bool,
    pub(crate) b7:bool,
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
