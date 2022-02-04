use std::fs::OpenOptions;
use std::io::Write;
use crate::common::{get_bit, get_bit_as_bool, Bitmap, print_ram, set_bit};
use crate::mmu2::{IORegister, MMU2};
use crate::optest::GBState;

pub struct PPU2 {
    pub backbuffer: Bitmap,
    //::init(256,256),
    vramdump: Bitmap, //::init(128,256),
    pub count:u32,
    pub entered_vram:bool,
}

impl PPU2 {
    pub(crate) fn update(&mut self, mmu:&mut MMU2) {
        self.entered_vram = false;
        self.count += 1;
        if self.count % 500 == 0 {
            // update the scanline
            let  ly1 = mmu.read8_IO(IORegister::LY);
            // println!("v is {}",v);
            let mut ly2 = ly1+1;
            if ly1 >= 154 {
                ly2 = 0;
            }
            // println!("updating the scan line {} -> {}",ly1, ly2);

            // if entering vblank
            if ly1 == 143 && ly2 == 144 {
                // println!("Just entered vblank. start mode 1. fire interrupt.");
                let val = mmu.read8_IO(IORegister::IF);
                let val2 = set_bit(val,0,true);
                mmu.write8_IO(IORegister::IF,val2);
                mmu.write8_IO(IORegister::IE,0xFF);
                self.entered_vram = true;
            }
            // if exiting vblank
            if ly1 == 154 && ly2 == 0 {
                // println!("Just finished vblank. go back to mode 2. drawing screen.");
                self.draw_full_screen(mmu);
            }
            mmu.write8_IO(IORegister::LY,ly2);
        }
    }
}

impl PPU2 {
    pub fn init() -> PPU2 {
        PPU2 {
            backbuffer: Bitmap::init(256, 256),
            vramdump: Bitmap::init(128, 256),
            count: 0,
            entered_vram: false
        }
    }
    pub fn draw_full_screen(&mut self, mmu: &MMU2) {
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
        // println!("tilemap base address {:04x}", bg_tilemap_start);
        let bg_tilemap = mmu.borrow_slice(bg_tilemap_start, bg_tilemap_end + 1);
        // let bg_tilemap = &mmu.data[bg_tilemap_start..(bg_tilemap_end + 1)];
        // let oam_table = &mmu.data[0xFE00..0xFEA0];

        let mut td1_start = 0x8800;
        let mut td1_end = 0x97FF;
        let unsigned_mode = get_bit_as_bool(mmu.read8_IO(IORegister::LCDC), 4);
        if unsigned_mode {
            td1_start = 0x8000;
            td1_end = 0x8FFF;
        }
        // println!("LCDC is {:02x}", mmu.read8_IO(IORegister::LCDC));
        // println!("tile data base address {:04x}", td1_start);
        let td1 = mmu.borrow_slice(td1_start, td1_end + 1);
        // for (n, row) in bg_tilemap.chunks_exact(32).enumerate() {
        //     let line_str: String = row.iter().map(|b| format!("{:02x}", b)).collect();
        //     println!("{:04x} {}", bg_tilemap_start + n * 32, line_str);
        // }
        //
        // println!("signed mode = {}", !unsigned_mode);
        if bg_enabled {
            let img = &mut self.backbuffer;
            let sx = 0; //mmu.hardware.SCX.value as usize;
            let sy = 0; //mmu.hardware.SCY.value as usize;
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
                        draw_tile_at(
                            img,
                            x * spacing + sx,
                            y * spacing + sy,
                            id2 as u8,
                            td1,
                            false,
                        );
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

fn draw_tile_at(img: &mut Bitmap, x: usize, y: usize, tile_id: u8, tiledata: &[u8], print: bool) {
    let start: usize = ((tile_id as u16) * 16) as usize;
    let stop: usize = start + 16;
    if print {
        println!(
            "id {:02x} maps to addr {:04x} - {:04x}  final {:04x}",
            tile_id,
            start,
            stop,
            ((start as u16) + 0x8800)
        );
    }
    let tile = &tiledata[start..stop];
    for (line, row) in tile.chunks_exact(2).enumerate() {
        for (n, color) in pixel_row_to_colors(row).iter().enumerate() {
            let (r, g, b) = match color {
                0 => (50, 50, 50),
                1 => (100, 100, 100),
                2 => (0, 0, 0),
                3 => (200, 200, 200),
                _ => (255, 0, 255),
            };
            img.set_pixel_rgb((x + 7 - n) as i32, (y + line) as i32, r, g, b);
        }
    }
}

fn pixel_row_to_colors(row: &[u8]) -> Vec<u8> {
    let b1 = row[0];
    let b2 = row[1];
    let mut colors: Vec<u8> = vec![];
    for n in 0..8 {
        let v1 = get_bit(b1, n);
        let v2 = get_bit(b2, n);
        colors.push((v1 << 1) | v2);
    }
    colors
}

#[test]
fn test_vblank() {
    //init empty arram
    let mut rom:Vec<u8> = vec![0xd3; 0x200];
    // start at 0000.
    // SP <- fffe            LD SP u16     31 fe ff
    let arr = [0x31, 0xfe, 0xff].to_vec();
    let n = copy_at(&mut rom, 0x0000, arr);

    // put 40 into memory
    // A <- 40               LD A,n        3E 40
    // (hi + 80) <- A        LDH (n), A    E0 80
    let arr = [
        0x3E, 0x40,  // LD A,n
        0xE0, 0x80,  // LDH (n), A
        0xFB,
    ].to_vec();
    let n = copy_at(&mut rom, n, arr);


    // spin loop that waits for value to become 41
    // A <- (hi + 80)        LDH A,n       F0 80
    // A ==? 41              CP 41         FE 41
    // if not zero, jump back 5   JR NZ e  20 Fc
    // jump absolute to 20   JP 20      C3 20 00
    let arr = [
        0xF0, 0x80, // LDH A, n
        0xFE, 0x41, // CP A, 41
        0x20, 0xFC, // JR NZ e
        0xC3, 0x20, 0x00  // JP nn  Jump to x0020
    ].to_vec();
    let n = copy_at(&mut rom,n,arr);

    copy_at(&mut rom, 0x20,[
        0x00, //NO OP
        0x18, 0xFd, // JR e
    ].to_vec());


    // at 0x40
    // A <- 41            LD A,n        3E 41
    // (hi + 80) <- A     LDH (n),A     E0 80
    // return             RETI          D9
    copy_at(&mut rom, 0x40, [
        0x3E, 0x41, // LD A, 41
        0xE0, 0x80,  //LDH (hi+80),A
        0xD9,  // RETI
    ].to_vec());


    // at 100 do absolute jump to 0000
    // PC <- 0     JP nn          C3 00 00
    let n = copy_at(&mut rom, 0x100,[0xC3, 0x00, 0x00].to_vec());

    print_ram(0x0000,&rom);

    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .open("./vblank_unit_test.gb").unwrap();
    file.write_all(&rom).unwrap();
    println!("wrote out file");
    // let mut gb: GBState = GBState::make_test_context(&rom.to_vec());
    // gb.set_pc(0x0000); // start at 00000
    // while gb.get_pc() != 0x20 {
    //     println!("running");
    //     gb.execute();
    // }
}

fn copy_at(rom: &mut Vec<u8>, start: usize, data: Vec<u8>) -> usize {
    println!("copying to {}  n{}", start, data.len());
    for n in 0..data.len() {
        rom[start + n] = data[n];
    }
    return start + data.len();
}










