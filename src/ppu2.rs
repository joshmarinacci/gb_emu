use std::fs::OpenOptions;
use std::io::Write;
use std::sync::{Arc, Mutex};
use crate::common::{get_bit, get_bit_as_bool, Bitmap, print_ram, set_bit};
use crate::hardware::LCDMode;
use crate::mmu2::{IORegister, MMU2};
use crate::optest::GBState;

pub struct SSS {
    pub SCX: u8,
    pub SCY: u8,
    pub backbuffer: Bitmap,
    pub vramdump: Bitmap,
}
pub struct PPU2 {
    pub sss: Arc<Mutex<SSS>>,
    pub next_clock:u32,
    pub count:u32,
    pub entered_vram:bool,
    last_vblank:u32,
}

impl PPU2 {
    pub(crate) fn update(&mut self, mmu:&mut MMU2, clock: u32) {
        self.entered_vram = false;
        if clock > self.next_clock*4 {
            match mmu.stat.mode {
                LCDMode::HBlank => {
                    // println!("entering OAM search");
                    mmu.stat.mode = LCDMode::Searching;
                    //maybe trigger interrupt
                    let  ly1 = mmu.read8_IO(IORegister::LY);
                    let mut ly2 = ly1+1;
                    mmu.write8_IO(IORegister::LY,ly2);
                    // println!("incremented scanline ly {}",ly2);
                    self.next_clock += 80;
                    if ly2 >= 144 {
                        // println!("leaving normal, going to vblank");
                        // ly2 = 154;
                        mmu.stat.mode = LCDMode::VBlank;
                        //request vblank interrupt handler
                        let val = mmu.read8_IO(IORegister::IF);
                        let val2 = set_bit(val,0,true);
                        mmu.write8_IO(IORegister::IF,val2);
                        mmu.write8_IO(IORegister::IE,0xFF);
                        self.entered_vram = true;
                        //wait for 10 scan lines
                        self.next_clock += 456;
                    }
                },
                LCDMode::VBlank => {
                    // println!("still in vblank");
                    let  ly1 = mmu.read8_IO(IORegister::LY);
                    let mut ly2 = ly1+1;
                    if ly2 >= 154 {
                        // println!("time to end the vblank");
                        ly2 = 0;
                        self.draw_full_screen(mmu);
                        mmu.stat.mode = LCDMode::Searching;
                        self.entered_vram = false;
                        // println!("frame len {}",clock - self.last_vblank);
                        self.last_vblank = clock;
                    } else {
                        self.next_clock += 456;
                    }
                    mmu.write8_IO(IORegister::LY,ly2);
                }
                LCDMode::Searching => {
                    mmu.stat.mode = LCDMode::Transferring;
                    // println!("entering oam transfer");
                    //maybe trigger interrupt
                    self.next_clock += 172;
                }
                LCDMode::Transferring => {
                    mmu.stat.mode = LCDMode::HBlank;
                    // println!("entering hblank");
                    //maybe trigger interrupt
                    self.next_clock += (456-172-80);
                }
            }
        }
    }
}

impl PPU2 {
    pub fn init() -> PPU2 {
        PPU2 {
            sss: Arc::new(Mutex::new(SSS {
                SCX: 0,
                SCY: 0,
                backbuffer: Bitmap::init(256, 256),
                vramdump: Bitmap::init(128, 256),
            })),
            next_clock: 0,
            count:0,
            entered_vram: false,
            last_vblank: 0
        }
    }
    pub fn draw_full_screen(&mut self, mmu: &MMU2) {
        // println!("drawing the full screen");
        let mut sss = self.sss.lock().unwrap();
        sss.SCX = mmu.read8_IO(IORegister::SCX);
        sss.SCY = mmu.read8_IO(IORegister::SCY);

        let bg_tilemap = mmu.borrow_range(&mmu.lcdc.bg_tilemap_select);
        let oam_table = mmu.borrow_slice(0xFE00,0xFEA0);
        print_ram(0xFE00, &oam_table.to_vec());
        let tile_data = mmu.borrow_range(&mmu.lcdc.bg_window_tiledata_select);
        let sprite_data = mmu.borrow_slice(0x8000,0x9000);

        let sx = sss.SCX as usize;
        let sy = sss.SCY as usize;
        if mmu.lcdc.bg_enabled {
            let img = &mut sss.backbuffer;
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
                    if mmu.lcdc.signed_addressing {
                        let id2 = i16::from(id as i8) + 128;
                        draw_tile_at(
                            img,
                            x * spacing + sx,
                            y * spacing + sy,
                            id2 as u8,
                            tile_data,
                            false,
                        );
                    } else {
                        draw_tile_at(img, x * spacing + sx, y * spacing + sy, id, tile_data, false);
                    }
                }
            }
        }
        if mmu.lcdc.sprite_enabled {
            // println!("drawing sprites");
            let img = &mut sss.backbuffer;
            for (i, atts) in oam_table.chunks_exact(4).enumerate() {
                let y = atts[0];
                let x = atts[1];
                let tile_id = atts[2];
                let flags = atts[3];
                // println!("i {} id {}",i,tile_id);
                if tile_id >= 0 && tile_id < 0x80 {
                    //println!("{} {} {}",tile_id,y,x);
                    // println!("drawing sprite {}",tile_id);
                    // println!("   sprite at {}x{} id={:02x} flags={:08b}", x, y, tile_id, flags);
                    if mmu.lcdc.sprite_size_big {
                        // println!("skipping big sprites");
                    } else {
                        // println!("drawing sprite at {},{}",x,y);
                        // img.set_pixel_rgb(x as i32,y as i32,0,0,0);
                        // img.set_pixel_rgb((x+1) as i32,y as i32,0,0,0);
                        // img.set_pixel_rgb((x+1) as i32,(y+1) as i32,0,0,0);
                        draw_tile_at(img, x as usize, y as usize, tile_id, sprite_data, false);
                    }
                }
            }
        }

        //draw the vram
        {
            let tile_data = mmu.borrow_slice(0x8000,0x9800);
            let img = &mut sss.vramdump;
            for (n, tile) in tile_data.chunks_exact(16).enumerate() {
                let x = (n % 16) * 8;
                let y = (n / 16) * 8;
                for (line, row) in tile.chunks_exact(2).enumerate() {
                    for (n, color) in pixel_row_to_colors(row).iter().enumerate() {
                        let (r, g, b) = match color {
                            0 => (255, 255, 255),
                            1 => (220, 220, 220),
                            2 => (170, 170, 170),
                            3 => (50, 50, 50),
                            _ => (255, 0, 255),
                        };
                        img.set_pixel_rgb((x + 7 - n) as i32, (y + line) as i32, r, g, b);
                    }
                }
            }
        }
    }
}

fn draw_tile_at(img: &mut Bitmap, x: usize, y: usize, tile_id: u8, tiledata: &[u8], print: bool) {
    let start: usize = ((tile_id as u16) * 16) as usize;
    let stop: usize = start + 16;
    // if print {
    //     println!(
    //         "id {:02x} maps to addr {:04x} - {:04x}  final {:04x}",
    //         tile_id,
    //         start,
    //         stop,
    //         ((start as u16) + 0x8800)
    //     );
    // }
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
    copy_at(&mut rom,n,arr);

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
    copy_at(&mut rom, 0x100,[0xC3, 0x00, 0x00].to_vec());

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










