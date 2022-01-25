use std::time::Duration;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::{Texture, TextureAccess, WindowCanvas};
use sdl2::Sdl;
use crate::common::Bitmap;

pub struct Screen {
    canvas:WindowCanvas,
    texture:Texture,
    context: Sdl,
}

impl Screen {
    pub fn init(w:i32,h:i32) -> Screen {
        let sdl_context = sdl2::init().unwrap();
        let window = sdl_context.video().unwrap()
            .window("rust-sdl2 demo: Video", 256 * 2, 256 * 2)
            .position_centered()
            .opengl()
            .build()
            .map_err(|e| e.to_string()).unwrap();
        let canvas:WindowCanvas = window.into_canvas().software().build().map_err(|e| e.to_string()).unwrap();
        let tex = canvas.texture_creator().create_texture(PixelFormatEnum::ARGB8888, TextureAccess::Target, w as u32, h as u32).unwrap();
        Screen {
            context:sdl_context,
            canvas,
            texture:tex,
        }
    }
    pub fn update_screen(&mut self, backbuffer:&Bitmap) {
        //handle any pending inputs
        while true {
            if let Some(event) = self.context.event_pump().unwrap().poll_event() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => {
                        println!("quitting");
                    },
                    _ => {
                        println!("othe event {:?}", event);
                    }
                }
            } else {
                println!("done with events");
                break;
            }
        }

        self.canvas.with_texture_canvas(&mut self.texture, |can| {
            for i in 0..backbuffer.w {
                for j in 0..backbuffer.h {
                    let n: usize = ((j * backbuffer.w + i) * 4) as usize;
                    //let px = img.get_pixel_32argb(i,j);
                    // let ve = img.get_pixel_vec_argb(i as u32,j as u32);
                    let (r, g, b) = backbuffer.get_pixel_rgb(i, j);
                    // println!("rgb {},{},{}",r,g,b);
                    // let col = Color::RGBA(ve[1],ve[2],ve[3], ve[0]);
                    can.set_draw_color(sdl2::pixels::Color::RGBA(r, g, b, 255));
                    can.fill_rect(Rect::new(i as i32, j as i32, 1, 1));
                }
            }
        }).unwrap();

        self.canvas.copy(&self.texture, None,
                           Rect::new(0, 0,
                                     (backbuffer.w * 2) as u32,
                                     (backbuffer.h * 2) as u32));

        self.canvas.present();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}
