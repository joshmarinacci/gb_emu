use std::io::Result;
use std::sync::{Arc, Mutex, MutexGuard};
use std::sync::mpsc::{Receiver, Sender};
use crate::{common, MMU};
use crate::common::{Bitmap, get_bit_as_bool, set_bit};
use crate::debugger::InputEvent;
use crate::mmu::STAT_LCDCONTROL;
use crate::opcodes::u8_as_i8;

pub struct PPU {
    pub last_clock: u32,
    drawing:bool,
    pub wait_until:u32,
}
const dots_per_scanline:u32 = 456;
const mode_0_length:u32 = 208;
const mode_1_length:u32 = 4560;
const mode_2_length:u32 = 80;
const mode_3_length:u32 = 168;

impl PPU {
    // frame len  70224
    pub(crate) fn update(&mut self, mmu: &mut MMU, screen_state_mutext: &mut Arc<Mutex<ScreenState>>, clock: &mut u32, to_screen: &Sender<String>, receive_cpu: &Receiver<InputEvent>, screen_attached: bool) {
        if *clock < self.wait_until {
            // println!("not yet. {} < {}",*clock, self.wait_until);
            return;
        }
        //draw the current scanline
        let mut do_redraw = false;
        {
            let mut screen_state = screen_state_mutext.lock().unwrap();
            let prev_line = screen_state.current_scanline;
            screen_state.current_scanline += 1;
            screen_state.LY = screen_state.current_scanline;
            mmu.hardware.LY = screen_state.LY;
            mmu.hardware.STAT = set_bit(mmu.hardware.STAT,2,mmu.hardware.LY == mmu.hardware.LYC);
            if screen_state.current_scanline == 154 {
                // println!("end of the frame");
                self.draw_tile_data(mmu, &mut screen_state);
                self.draw_full_screen(mmu, &mut screen_state);
                self.draw_scanline_2(mmu,&mut screen_state, false);
                screen_state.current_scanline = 0;
            }
            screen_state.LCDC = mmu.hardware.LCDC;
            let screen_on = true;
            if screen_state.current_scanline < 0x90 {
                // println!("drawing scanline {}", screen_state.current_scanline);
                if screen_on {
                    // self.draw_scanline(mmu, &mut screen_state, clock);
                    self.draw_scanline_2(mmu,&mut screen_state, true);
                }
            } else {
                if prev_line == 0x8F && screen_state.current_scanline == 0x90 {
                    // self.do_vblank(&mut screen_state);
                    // println!("ppu triggered a vblank");
                    screen_state.vblank_triggered = true;
                    do_redraw = true;
                }
                self.draw_scanline_2(mmu,&mut screen_state, false);
            }
        }
        if screen_attached && do_redraw {
            // println!("telling real screen to refresh");
            to_screen.send(String::from("go"));
            if let Ok(str) = receive_cpu.recv() {
            } else {
                // println!("error coming back from cpu?");
            }
        }
    }
}

impl PPU {
    pub(crate) fn init() -> PPU {
        PPU {
            last_clock:0,
            drawing: false,
            wait_until: 0
        }
    }
}

pub struct ScreenState {
    pub backbuffer: Bitmap,
    pub vramdump: Bitmap,
    pub SCX: u8,
    pub SCY: u8,
    pub current_scanline: u8,
    pub LCDC:u8,
    pub STAT:u8,
    pub LY:u8,
    pub vblank_triggered:bool,
}

impl ScreenState {
    pub(crate) fn init() -> ScreenState {
        ScreenState {
            backbuffer: Bitmap::init(256,256),
            vramdump: Bitmap::init(128,256),
            SCX:0,
            SCY:0,
            current_scanline: 0,
            LCDC: 0,
            STAT: 0,
            LY: 0,
            vblank_triggered: false
        }
    }
}

fn draw_tile_at(img: &mut Bitmap, x: usize, y: usize, tile_id: u8, tiledata: &[u8]) {
    let start:usize = ((tile_id as u16)*16) as usize;
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

impl PPU {
    pub fn draw_full_screen(&mut self, mmu: &mut MMU, screenstate: &mut MutexGuard<ScreenState>) -> Result<()> {
        // println!("bg and window enable/priority? {}",get_bit_as_bool(lcdc,0));
        // println!("sprites displayed? {}",get_bit_as_bool(lcdc,1));
        // println!("sprite size. 8x8 or 8x16? {}",get_bit_as_bool(lcdc,2));
        // println!("bg tile map area  {}",get_bit_as_bool(lcdc,3));
        // println!("bg tile data area? {}",get_bit_as_bool(screenstate.LCDC,4));
        // println!("window enable? {}",get_bit_as_bool(lcdc,5));
        // println!("window tile map area? {}",get_bit_as_bool(lcdc,6));
        // println!("LCD enable? {}",get_bit_as_bool(lcdc,7));

        // let window_enabled = get_bit_as_bool(lcdc, 5);
        let sprites_enabled = get_bit_as_bool(mmu.hardware.LCDC, 1);
        // let sprites_enabled = true;
        let bg_enabled = true; //bg is always enabled
        let sprite_big = get_bit_as_bool(screenstate.LCDC, 2);
        let mut bg_tilemap_start = 0x9800;
        let mut bg_tilemap_end = 0x9BFF;
        if get_bit_as_bool(mmu.hardware.LCDC, 3) {
            bg_tilemap_start = 0x9C00;
            bg_tilemap_end = 0x9FFF;
        }
        let bg_tilemap = &mmu.data[bg_tilemap_start .. (bg_tilemap_end +1)];
        let oam_table = &mmu.data[0xFE00..0xFEA0];

        let mut td1_start = 0x8800;
        let mut td1_end = 0x97FF;
        let unsigned_mode = get_bit_as_bool(mmu.hardware.LCDC, 4);
        if get_bit_as_bool(mmu.hardware.LCDC, 4) {
            td1_start = 0x8000;
            td1_end = 0x8FFF;
        }
        let td1 = &mmu.data[td1_start .. (td1_end + 1)];
        for row in bg_tilemap.chunks_exact(32) {
            println!("{:?}",row);
        }

            if bg_enabled {
                let img = &mut screenstate.backbuffer;
                let sx = 0;//mmu.hardware.SCX as usize;
                let sy = 0;//mmu.hardware.SCY as usize;
                for (y, row) in bg_tilemap.chunks_exact(32).enumerate() {
                    for (x, tile_id) in row.iter().enumerate() {
                        let id = *tile_id;
                        if !unsigned_mode {
                            let id2 = i16::from(id as i8) + 128;
                            draw_tile_at(img, x * 9 + sx, y * 9 + sy, id2 as u8, td1);
                        } else {
                            draw_tile_at(img, x * 9 + sx, y * 9 + sy, id, td1);
                        }
                    }
                }
            }
            if sprites_enabled {
                for (i, atts) in oam_table.chunks_exact(4).enumerate() {
                    let y = atts[0];
                    let x = atts[1];
                    let tile_id = atts[2];
                    let flags = atts[3];
                    if tile_id >= 0 && tile_id < 0xFF {
                        // println!("   sprite at {}x{} id={:02x} flags={:08b}", x, y, tile_id, flags);
                        if sprite_big {
                            println!("skipping big sprites");
                        } else {
                            // println!("drawing sprite");
                            draw_tile_at(&mut screenstate.backbuffer, x as usize, y as usize, tile_id, td1);
                        }
                    }
                }
            }
        // }
        Ok(())
    }
    fn draw_tile_data(&mut self, mmu: &mut MMU, screenstate:&mut ScreenState) {
        let tile_data = &mmu.data[0x8000 .. 0x97FF];
        let img = &mut screenstate.vramdump;
        // println!("drawing tile data {}",tile_data.len());
        for (n, tile) in tile_data.chunks_exact(16).enumerate() {
            // println!("tile num {}",n);
            let x = (n % 16) * 8;
            let y = (n/16) * 8;
            for (line, row) in tile.chunks_exact(2).enumerate() {
                for (n, color) in pixel_row_to_colors(row).iter().enumerate() {
                    let (r,g,b) = match color {
                        0 => (255,255,255),
                        1 => (220,220,220),
                        2 => (170,170,170),
                        3 => (50,50,50),
                        _ => (255,0,255),
                    };
                    img.set_pixel_rgb((x+7 - n) as i32, (y + line) as i32, r, g, b);
                }
            }
        }
    }
    fn draw_scanline_2(&mut self, mmu: &mut MMU, screenstate: &mut MutexGuard<ScreenState>, draw: bool)  {
        // println!("entering mode 2 ");
        // println!("current LY is {:02}", screenstate.LY);
        // println!("LCDC reg is {:02}", screenstate.LCDC);
        // println!("STAT reg is {:02}", screenstate.STAT);
        //search for sprites on current line
        // println!("searching for sprites on the current line");
        // println!("locking OAM memory");

        self.wait_until += mode_2_length;
        // println!("entering mode 3");
        // println!("locking all vram ");
        self.wait_until += mode_3_length;//* mode 3 is 168 to 291 dots long depending on the sprite count

        // println!("entering mode 0 / hblank");
        self.wait_until += mode_0_length;
        // println!("cpu can access everything");

    }
    fn do_vblank(&mut self, screenstate: &mut MutexGuard<ScreenState>) {
        // println!("entering mode 1");
        // println!("all memory is available");
        self.wait_until += mode_1_length;//4560
    }
}

fn fill_tile_at(img: &mut Bitmap, x: usize, y: usize) {
    for j in 0..8 {
        for i in 0..8 {
            img.set_pixel_rgb((x+i) as i32, (y + j) as i32, 0,0,0);
        }
    }
}

