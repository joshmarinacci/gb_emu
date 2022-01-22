use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

pub struct RomFile {
    pub(crate) data: Vec<u8>,
    pub(crate) path:String,
}

pub fn get_bit(by:u8, n:i32) -> u8 {
    if by & (1<<n) != 0 { 1 } else { 0 }
}


pub struct Bitmap {
    pub(crate) data:Vec<u8>,
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
    pub(crate) fn write_to_file(&self, filename: &str) {
        let path = Path::new(filename);
        let file = File::create(path).unwrap();
        let ref mut w = BufWriter::new(file);

        let mut encoder = png::Encoder::new(w, self.w as u32, self.h as u32);
        encoder.set_color(png::ColorType::Rgba);
        encoder.set_depth(png::BitDepth::Eight);

        let mut writer = encoder.write_header().unwrap();
        writer.write_image_data(self.data.as_slice()).unwrap();
    }
}

impl Bitmap {
    pub(crate) fn set_pixel_rgb(&mut self, x: i32, y: i32, r: u8, g:u8, b:u8)  {
        let n:usize = ((x + self.w*y)*4) as usize;
        // println!("n is {}",n);
        self.data[n+0] = r;
        self.data[n+1] = g;
        self.data[n+2] = b;
        self.data[n+3] = 255;
    }
}

impl Bitmap {
    pub fn init(w:i32,h:i32) -> Bitmap {
        let mut data:Vec<u8> = Vec::with_capacity((w * h * 4) as usize);
        data.resize((w * h * 4) as usize,255);
        data.fill(255);
        println!("Length is {}",data.len());
        Bitmap {
            w:w,
            h:h,
            data: data,
        }
    }
}
