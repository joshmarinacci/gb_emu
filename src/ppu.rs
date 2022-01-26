use std::io::Result;
use crate::{common, MMU};
use crate::common::{Bitmap, get_bit_as_bool};

fn draw_tile_at(img: &mut Bitmap, x: usize, y: usize, tile_id: &u8, tiledata: &[u8]) {
    let start:usize = ((*tile_id as u16)*16) as usize;
    let stop:usize = start + 16;
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
        let v1 = common::get_bit(b1, n);
        let v2 = common::get_bit(b2, n);
        colors.push((v1 << 1) | v2);
    }
    colors
}

pub fn draw_vram(mmu:&mut MMU, backbuffer: &mut Bitmap) -> Result<()> {
    let lcdc = mmu.hardware.LCDC;
    println!("bg and window enable/priority? {}",get_bit_as_bool(lcdc,0));
    println!("sprites displayed? {}",get_bit_as_bool(lcdc,1));
    println!("sprite size. 8x8 or 8x16? {}",get_bit_as_bool(lcdc,2));
    println!("bg tile map area  {}",get_bit_as_bool(lcdc,3));
    println!("bg tile data area? {}",get_bit_as_bool(lcdc,4));
    println!("window enable? {}",get_bit_as_bool(lcdc,5));
    println!("window tile map area? {}",get_bit_as_bool(lcdc,6));
    println!("LCD enable? {}",get_bit_as_bool(lcdc,7));

    let screen_on = get_bit_as_bool(lcdc, 7);
    let window_enabled = get_bit_as_bool(lcdc, 5);
    let sprites_enabled = true;
    let bg_enabled = true; //bg is always enabled
    let mut bg_tilemap_start = 0x9800;
    let mut bg_tilemap_end = 0x9BFF;
    if get_bit_as_bool(lcdc,3) {
        bg_tilemap_start = 0x9C00;
        bg_tilemap_end  = 0x9FFF;
    }
    let bg_tilemap = &mmu.data[bg_tilemap_start .. bg_tilemap_end];
    let oam_table = &mmu.data[0xFE00..0xFEA0];

    let mut low_data_start = 0x9000;
    let mut low_data_end = 0x97FF;
    if get_bit_as_bool(lcdc, 4) {
        low_data_start = 0x8000;
        low_data_end = 0x87FF;
    }
    let lo_data = &mmu.data[low_data_start..low_data_end];

    if screen_on {
        if bg_enabled {
            // println!("low data {:04x} {:04x}",low_data_start, low_data_end);
            // println!("tiledata = {:?}",lo_data);
            // println!("bg map {:04x} {:04x}",bg_tilemap_start, bg_tilemap_end);
            // println!("draw background. tilemap = {:?}", bg_tilemap);
            for (y, row) in bg_tilemap.chunks_exact(32).enumerate() {
                for (x, tile_id) in row.iter().enumerate() {
                    if *tile_id > 0 {
                        // println!("tile id is {}", tile_id);
                    }
                    draw_tile_at(backbuffer,
                                 x * 8 + (mmu.hardware.SCX as usize),
                                 y * 8 + (mmu.hardware.SCY as usize),
                                 tile_id,
                                 lo_data);
                }
            }
        }
        if sprites_enabled {
            for (i, atts) in oam_table.chunks_exact(4).enumerate() {
                println!("sprite atts {:?}",atts);
            }
        }
    }
    Ok(())
}
