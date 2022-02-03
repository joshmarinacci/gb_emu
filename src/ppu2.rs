use crate::common::{Bitmap, get_bit, get_bit_as_bool};
use crate::mmu2::{IORegister, MMU2};

pub struct PPU2 {
    pub backbuffer: Bitmap,//::init(256,256),
    vramdump: Bitmap,//::init(128,256),
}

impl PPU2 {
    pub fn init() -> PPU2 {
        PPU2 {
            backbuffer: Bitmap::init(256,256),
            vramdump: Bitmap::init(128,256),
        }
    }
    pub fn draw_full_screen(&mut self, mmu: &MMU2)  {
        // let window_enabled = get_bit_as_bool(lcdc, 5);
        let sprites_enabled = get_bit_as_bool(mmu.read8_IO(IORegister::LCDC), 1);
        // let sprites_enabled = true;
        let bg_enabled = true; //bg is always enabled
        // let sprite_big = get_bit_as_bool(screenstate.LCDC, 2);
        let mut bg_tilemap_start = 0x9800;
        let mut bg_tilemap_end = 0x9BFF;
        if get_bit_as_bool(mmu.read8_IO(IORegister::LCDC), 3) {
            bg_tilemap_start = 0x9C00;
            bg_tilemap_end = 0x9FFF;
        }
        println!("tilemap base address {:04x}", bg_tilemap_start);
        let bg_tilemap = mmu.borrow_slice(bg_tilemap_start,bg_tilemap_end+1);
        // let bg_tilemap = &mmu.data[bg_tilemap_start..(bg_tilemap_end + 1)];
        // let oam_table = &mmu.data[0xFE00..0xFEA0];

        let mut td1_start = 0x8800;
        let mut td1_end = 0x97FF;
        let unsigned_mode = get_bit_as_bool(mmu.read8_IO(IORegister::LCDC), 4);
        if unsigned_mode {
            td1_start = 0x8000;
            td1_end = 0x8FFF;
        }
        println!("LCDC is {:02x}",mmu.read8_IO(IORegister::LCDC));
        println!("tile data base address {:04x}", td1_start);
        let td1 = mmu.borrow_slice(td1_start,td1_end + 1);
        for (n, row) in bg_tilemap.chunks_exact(32).enumerate() {
            let line_str:String = row.iter()
                .map(|b|format!("{:02x}",b))
                .collect();
            println!("{:04x} {}",bg_tilemap_start+n*32, line_str);
        }

        println!("signed mode = {}", !unsigned_mode);
        if bg_enabled {
            let img = &mut self.backbuffer;
            let sx = 0;//mmu.hardware.SCX.value as usize;
            let sy = 0;//mmu.hardware.SCY.value as usize;
            let spacing = 8;
            for (y, row) in bg_tilemap.chunks_exact(32).enumerate() {
                if y > 0x10 {
                    continue;
                }
                for (x, tile_id) in row.iter().enumerate() {
                    if x > 0x13 {
                        continue;
                    }
                    let id = *tile_id;
                    if !unsigned_mode {
                        let id2 = i16::from(id as i8) + 128;
                        draw_tile_at(img, x * spacing + sx, y * spacing + sy, id2 as u8, td1, false);
                        // if (id == 0x56) {
                        //     println!("56  tile data = {:04x} - {:04x}", td1_start, td1_end);
                        //     draw_tile_at(img, x * spacing + sx, y * spacing + sy, id2 as u8, td1, true);
                        // }
                    } else {
                        draw_tile_at(img, x * spacing + sx, y * spacing + sy, id, td1, false);
                    }
                }
            }
        }
        // if sprites_enabled {
        //     for (i, atts) in oam_table.chunks_exact(4).enumerate() {
        //         let y = atts[0];
        //         let x = atts[1];
        //         let tile_id = atts[2];
        //         let flags = atts[3];
        //         if tile_id >= 0 && tile_id < 0xFF {
        //             // println!("   sprite at {}x{} id={:02x} flags={:08b}", x, y, tile_id, flags);
        //             if sprite_big {
        //                 println!("skipping big sprites");
        //             } else {
        //                 // println!("drawing sprite");
        //                 draw_tile_at(&mut screenstate.backbuffer, x as usize, y as usize, tile_id, td1, false);
        //             }
        //         }
        //     }
        // }
        // }
    }
}

    fn draw_tile_at(img: &mut Bitmap, x: usize, y: usize, tile_id: u8, tiledata: &[u8], print:bool) {
        let start:usize = ((tile_id as u16)*16) as usize;
        let stop:usize = start + 16;
        if print {
            println!("id {:02x} maps to addr {:04x} - {:04x}  final {:04x}", tile_id, start, stop, ((start as u16) + 0x8800));
        }
        let tile = &tiledata[start..stop];
        for (line,row) in tile.chunks_exact(2).enumerate() {
            for (n, color) in pixel_row_to_colors(row).iter().enumerate() {
                let (r,g,b) = match color {
                    0 => (50,50,50),
                    1 => (100,100,100),
                    2 => (0,0,0),
                    3 => (200,200,200),
                    _ => (255,0,255),
                };
                img.set_pixel_rgb((x+7 - n) as i32, (y + line) as i32, r, g, b);
            }
        }
    }
    fn pixel_row_to_colors(row: &[u8]) -> Vec<u8> {
        let b1 = row[0];
        let b2 = row[1];
        let mut colors:Vec<u8> = vec![];
        for n in 0..8 {
            let v1 = get_bit(b1, n);
            let v2 = get_bit(b2, n);
            colors.push((v1 << 1) | v2);
        }
        colors
    }
