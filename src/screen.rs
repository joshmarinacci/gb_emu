use std::sync::mpsc::Sender;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::{Texture, TextureAccess, WindowCanvas};
use sdl2::Sdl;
use crate::common::{Bitmap, InputEvent, JoyPadKey};
use crate::common::InputEvent::{JumpNextVBlank, Press, Release};
use crate::ppu::ScreenState;

#[derive(Debug)]
pub struct ScreenSettings {
    pub x:i32,
    pub y:i32,
    pub scale:f32,
    pub enabled:bool,
}
pub struct Screen {
    canvas:WindowCanvas,
    texture:Texture,
    context: Sdl,
    pub scale: f32,
    pub tex2: Texture,
}

impl Screen {
    pub(crate) fn process_input(&self, to_cpu: &Sender<InputEvent>) -> bool {
        // println!("screen: processsing input");
        loop {
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
                    Event::KeyDown {keycode:Some(Keycode::V),..} => { to_cpu.send(JumpNextVBlank());  }
                    Event::KeyUp {keycode:Some(Keycode::V),..} => {  to_cpu.send(JumpNextVBlank()); }
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
    pub fn init(settings: &ScreenSettings) -> Screen {
        println!("using scale {}", settings.scale);
        let win_w:u32 = (settings.scale * (256.0 + 128.0)).floor() as u32;
        let win_h:u32 = (settings.scale * 256.0).floor() as u32;
        let sdl_context = sdl2::init().unwrap();
        let window = sdl_context.video().unwrap()
            .window("rust-sdl2 demo: Video", win_w, win_h)
            .position(settings.x,settings.y)//ition_centered()
            .opengl()
            .build()
            .map_err(|e| e.to_string()).unwrap();
        let canvas:WindowCanvas = window.into_canvas().software().build().map_err(|e| e.to_string()).unwrap();
        let tex = canvas.texture_creator()
            .create_texture(PixelFormatEnum::ARGB8888, TextureAccess::Target, 256,256).unwrap();
        let tex2 = canvas.texture_creator()
            .create_texture(PixelFormatEnum::ARGB8888, TextureAccess::Target, 128,256).unwrap();
        Screen {
            context:sdl_context,
            canvas,
            texture:tex,
            tex2:tex2,
            scale:settings.scale,
        }
    }
    pub fn update_screen(&mut self, screenstate_mutex: &Arc<Mutex<ScreenState>>)  {
        //handle any pending inputs
        {
            let screenstate = screenstate_mutex.lock().unwrap();
            // println!("current scanline {}", screenstate.current_scanline);
            copy_texture(&mut self.canvas, &mut self.texture, &screenstate.backbuffer, self.scale, 0,0);
            copy_texture(&mut self.canvas, &mut self.tex2, &screenstate.vramdump, self.scale, 256,0);
        }
        self.canvas.present();
        ::std::thread::sleep(Duration::from_millis(1000/60));
    }
}

fn copy_texture(canvas: &mut WindowCanvas, texture: &mut Texture, bitmap: &Bitmap, scale:f32, x: i32, y: i32) {
    canvas.with_texture_canvas(texture, |can| {
        for i in 0..bitmap.w {
            for j in 0..bitmap.h {
                let n: usize = ((j * bitmap.w + i) * 4) as usize;
                //let px = img.get_pixel_32argb(i,j);
                // let ve = img.get_pixel_vec_argb(i as u32,j as u32);
                let (r, g, b) = bitmap.get_pixel_rgb(i, j);
                // println!("rgb {},{},{}",r,g,b);
                // let col = Color::RGBA(ve[1],ve[2],ve[3], ve[0]);
                can.set_draw_color(sdl2::pixels::Color::RGBA(r, g, b, 255));
                can.fill_rect(Rect::new(i as i32, j as i32, 1, 1));
            }
        }
    }).unwrap();
    let w = (scale * bitmap.w as f32).floor() as u32;
    let h = (scale * bitmap.h as f32).floor() as u32;
    let xx = (scale*x as f32).floor() as i32;
    let yy = (scale*y as f32).floor() as i32;
    canvas.copy(texture, None, Rect::new(xx, yy, w,h));
}
