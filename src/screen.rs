use std::sync::mpsc::Sender;
use std::sync::Mutex;
use std::time::Duration;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::{Texture, TextureAccess, WindowCanvas};
use sdl2::Sdl;
use crate::common::Bitmap;
use crate::debugger::{InputEvent, JoyPadKey};
use crate::debugger::InputEvent::{Press, Release};
use crate::debugger::JoyPadKey::A;

pub struct Screen {
    canvas:WindowCanvas,
    texture:Texture,
    context: Sdl,
}

impl Screen {
    pub(crate) fn process_input(&self, to_cpu: &Sender<InputEvent>) -> bool {
        // println!("screen: processsing input");
        while true {
            if let Some(event) = self.context.event_pump().unwrap().poll_event() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => {
                        println!("quitting");
                        to_cpu.send(InputEvent::Stop());
                        return false;
                    },
                    Event::KeyDown {keycode:Some(Keycode::Z),..} => { to_cpu.send(Press(JoyPadKey::A));  }
                    Event::KeyUp {keycode:Some(Keycode::Z),..} => {  to_cpu.send(Release(JoyPadKey::A)); }
                    Event::KeyDown {keycode:Some(Keycode::X),..} => { to_cpu.send(Press(JoyPadKey::B));  }
                    Event::KeyUp {keycode:Some(Keycode::X),..} => {  to_cpu.send(Release(JoyPadKey::B)); }
                    Event::KeyDown {keycode:Some(Keycode::Space),..} => { to_cpu.send(Press(JoyPadKey::Select));  }
                    Event::KeyUp {keycode:Some(Keycode::Space),..} => {  to_cpu.send(Release(JoyPadKey::Select)); }
                    Event::KeyDown {keycode:Some(Keycode::Return),..} =>  {to_cpu.send(Press(JoyPadKey::Start));},
                    Event::KeyUp {keycode:Some(Keycode::Return),..} =>  {to_cpu.send(Release(JoyPadKey::Start));},
                    Event::KeyDown {keycode:Some(Keycode::Left),..} => { to_cpu.send(Press(JoyPadKey::Left));  }
                    Event::KeyUp {keycode:Some(Keycode::Left),..} => {  to_cpu.send(Release(JoyPadKey::Left)); }
                    Event::KeyDown {keycode:Some(Keycode::Right),..} => { to_cpu.send(Press(JoyPadKey::Right));  }
                    Event::KeyUp {keycode:Some(Keycode::Right),..} => {  to_cpu.send(Release(JoyPadKey::Right)); }
                    Event::KeyDown {keycode:Some(Keycode::Up),..} => { to_cpu.send(Press(JoyPadKey::Up));  }
                    Event::KeyUp {keycode:Some(Keycode::Up),..} => {  to_cpu.send(Release(JoyPadKey::Up)); }
                    Event::KeyDown {keycode:Some(Keycode::Down),..} => { to_cpu.send(Press(JoyPadKey::Down));  }
                    Event::KeyUp {keycode:Some(Keycode::Down),..} => {  to_cpu.send(Release(JoyPadKey::Down)); }
                    _ => {
                        // println!("othe event {:?}", event);
                    }
                }
            } else {
                // println!("done with events");
                break;
            }
        }
        return true;
    }
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
    pub fn update_screen(&mut self, backbuffer_m: &Mutex<Bitmap>)  {
        //handle any pending inputs
        {
            let backbuffer = backbuffer_m.lock().unwrap();
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

        }
        self.canvas.present();
        ::std::thread::sleep(Duration::from_millis(1000/60));
    }
}
