use std::{io, thread};
use console::{Color, Style, Term};
use io::Result;
use std::sync::{Arc, Mutex, MutexGuard};
use console::Color::White;
use log::{debug, info};
use Load::Load_R_u8;
use crate::{common, fetch_opcode_from_memory, MMU, opcodes, Z80};
use crate::common::{Bitmap, get_bit_as_bool, RomFile};
use crate::mmu::TEST_ADDR;
use crate::opcodes::{Compare, DoubleRegister, Instr, Jump, Load, lookup_opcode, Math, RegisterName, Special, u8_as_i8};
use crate::opcodes::Compare::CP_A_r;
use crate::opcodes::DoubleRegister::BC;
use crate::opcodes::Instr::{LoadInstr, SpecialInstr};
use crate::opcodes::RegisterName::{A, B, C, D};
use crate::screen::Screen;

struct Ctx {
    cpu:Z80,
    mmu:MMU,
    clock:u32,
    running:bool,
    interactive:bool,
    cart: Option<RomFile>,
    full_registers_visible: bool,
    test_memory_visible: bool,
    needs_redraw:bool,
}

impl Ctx {
    fn make_test_context(rom: &[u8]) -> Ctx {
        Ctx {
            cpu:Z80::init(),
            mmu: MMU::init(rom),
            clock: 0,
            running: true,
            interactive: false,
            cart: None,
            full_registers_visible: false,
            test_memory_visible: false,
            needs_redraw: false,
        }
    }
}

impl Ctx {
    pub(crate) fn execute(&mut self, term: &mut Term, verbose: bool) -> Result<()>{
        self.needs_redraw = false;
        let (opcode, _off) = fetch_opcode_from_memory(&self.cpu, &self.mmu);
        if verbose {
            term.write_line(&format!("--PC at {:04x}  op {:0x}", self.cpu.r.pc, opcode))?;
        }
        if let Some(instr) = lookup_opcode(opcode) {
            self.execute_instruction(&instr);
        } else {
            term.write_line(&format!("current cycle {}", self.clock))?;
            println!("unknown op code {:04x}",opcode);
            println!("current memory is PC {:04x} ",self.cpu.get_pc());
            panic!("unknown op code");
        }
        let old_ly = self.mmu.hardware.LY;
        self.mmu.update(&mut self.cpu);
        if old_ly != 0 && self.mmu.hardware.LY == 0 {
            // println!("vsync");
            self.needs_redraw = true;
            // self.draw_screen();
        }
        if self.cpu.get_pc() == 0x0040 {
            // println!("Jumped to vblank handler");
        }
        self.clock+=1;
        Ok(())
    }

    fn execute_instruction(&mut self, inst: &Instr) {
        match inst {
            Instr::SpecialInstr(special)  => opcodes::execute_special_instructions(&mut self.cpu, &mut self.mmu, special),
            Instr::LoadInstr(load)         => opcodes::execute_load_instructions(&mut self.cpu, &mut self.mmu, load),
            Instr::CompareInst(comp)   => self.execute_compare_instructions(comp),
            Instr::MathInst(math)         => self.execute_math_instructions(math),
            Instr::JumpInstr(jump)         => self.execute_jump_instructions(jump),
        }
    }
    fn inc_pc(&mut self, n: i32) {
        let (pc, overflowed) = self.cpu.r.pc.overflowing_add(n as u16);
        if overflowed {
            self.mmu.print_cram();
            panic!("PC overflowed memory");
        }
        self.cpu.r.pc = pc;
    }
    fn set_pc(&mut self, addr: u16) {
        // println!("jumping to {:04x}",addr);
        self.cpu.r.pc = addr;
    }
    fn execute_compare_instructions(&mut self, comp:&Compare) {
        match comp {
            Compare::CP_A_n() => {
                self.inc_pc(1);
                let src_v = self.mmu.read8(self.cpu.r.pc);
                self.inc_pc(1);

                let dst_v = self.cpu.r.get_u8reg(&A);
                let result = src_v.wrapping_sub(dst_v);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
                self.cpu.r.subtract_n_flag = true;
                self.cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
            }
            Compare::CP_A_r(r) => {
                self.inc_pc(1);
                let src_v = self.cpu.r.get_u8reg(r);
                let dst_v = self.cpu.r.get_u8reg(&A);
                let result = dst_v.wrapping_sub(src_v);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
                self.cpu.r.subtract_n_flag = true;
                self.cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
            }
            Compare::CP_A_addr(r) => {
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(r);
                let src_v = self.mmu.read8(addr);
                let dst_v = self.cpu.r.get_u8reg(&A);
                let result = dst_v.wrapping_sub(src_v);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
                self.cpu.r.subtract_n_flag = true;
                self.cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
            }
        }
    }
    fn execute_math_instructions(&mut self, math: &Math) {
        match math {
            Math::XOR_A_r(r) => {
                self.inc_pc(1);
                let res = self.cpu.r.get_u8reg(&A) ^ self.cpu.r.get_u8reg(r);
                self.cpu.r.zero_flag = res == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = false;
                self.cpu.r.carry_flag = false;
                self.cpu.r.set_u8reg(&A,res);
            }
            Math::XOR_A_u8() => {
                self.inc_pc(1);
                let n = self.mmu.read8(self.cpu.r.pc);
                let res = self.cpu.r.get_u8reg(&A) ^ n;
                self.cpu.r.zero_flag = res == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = false;
                self.cpu.r.carry_flag = false;
                self.cpu.r.set_u8reg(&A,res);
            }
            Math::XOR_A_addr(rr) => {
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(rr);
                let val = self.mmu.read8(addr);
                let res = self.cpu.r.get_u8reg(&A) ^ val;
                self.cpu.r.zero_flag = res == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = false;
                self.cpu.r.carry_flag = false;
                self.cpu.r.set_u8reg(&A,res);
            }
            Math::OR_A_r(r) => {
                self.inc_pc(1);
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) | self.cpu.r.get_u8reg(r));
                self.cpu.r.zero_flag = self.cpu.r.get_u8reg(&A) == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = false;
                self.cpu.r.carry_flag = false;
            }
            Math::OR_A_addr(rr) => {
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(rr);
                let val = self.mmu.read8(addr);
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) | val);
                self.cpu.r.zero_flag = self.cpu.r.get_u8reg(&A) == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = false;
                self.cpu.r.carry_flag = false;
            }
            Math::AND_A_r(r) => {
                self.inc_pc(1);
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) & self.cpu.r.get_u8reg(r));
                self.cpu.r.zero_flag = self.cpu.r.get_u8reg(&A) == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = true;
                self.cpu.r.carry_flag = false;
            }
            Math::AND_A_u8() => {
                self.inc_pc(1);
                let n = self.mmu.read8(self.cpu.r.pc);
                self.inc_pc(1);
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) & n);
                self.cpu.r.zero_flag = self.cpu.r.get_u8reg(&A) == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = true;
                self.cpu.r.carry_flag = false;
            }
            Math::AND_A_addr(rr) => {
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(rr);
                let val = self.mmu.read8(addr);
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) & val);
                self.cpu.r.zero_flag = self.cpu.r.get_u8reg(&A) == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = true;
                self.cpu.r.carry_flag = false;
            }
            Math::ADD_R_u8(r) => {
                self.inc_pc(1);
                let v1 = self.cpu.r.get_u8reg(r);
                let v2 = self.mmu.read8(self.cpu.r.pc);
                let result = v1.wrapping_add(v2);
                self.inc_pc(1);
                self.cpu.r.set_u8reg(r, v2);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = (v1 & 0xF) + (v2 & 0xF) > 0xFF;
                self.cpu.r.carry_flag = (v1 as u16) + (v2 as u16) > 0xFF;
            }
            Math::ADD_R_R(dst, src) => {
                self.inc_pc(1);
                let dst_v = self.cpu.r.get_u8reg(dst);
                let src_v = self.cpu.r.get_u8reg(src);
                let result = dst_v.wrapping_add(src_v);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (dst_v & 0x0F) + (src_v & 0x0f) > 0xF;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.carry_flag = (dst_v as u16) + (src_v as u16) > 0xFF;
                self.cpu.r.set_u8reg(dst, result);
            }
            Math::ADD_RR_RR(dst,src) => {
                self.inc_pc(1);
                let src_v = self.cpu.r.get_u16reg(src);
                let dst_v = self.cpu.r.get_u16reg(dst);
                let result = dst_v.wrapping_add(src_v);
                self.cpu.r.subtract_n_flag = false;
                //dont modify the zero flag
                self.cpu.r.half_flag = (dst_v & 0x07FF) + (src_v & 0x07FF) > 0x07FF;
                self.cpu.r.carry_flag = (dst_v) > (0xFFFF - src_v);
                self.cpu.r.set_u16reg(dst,result);
            }
            Math::ADD_A_addr(rr) => {
                self.inc_pc(1);
                let dst_v = self.cpu.r.get_u8reg(&A);
                let addr = self.cpu.r.get_u16reg(rr);
                let src_v = self.mmu.read8(addr);
                let result = dst_v.wrapping_add(src_v);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (dst_v & 0x0F) + (src_v & 0x0f) > 0xF;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.carry_flag = (dst_v as u16) + (src_v as u16) > 0xFF;
                self.cpu.r.set_u8reg(&A, result);
            }
            Math::SUB_R_R(dst, src) => {
                self.inc_pc(1);
                let dst_v = self.cpu.r.get_u8reg(dst);
                let src_v = self.cpu.r.get_u8reg(src);
                let result = dst_v.wrapping_sub(src_v);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
                self.cpu.r.subtract_n_flag = true;
                self.cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
                self.cpu.r.set_u8reg(dst, result);
            }
            Math::SUB_A_addr(rr) => {
                self.inc_pc(1);
                let dst_v = self.cpu.r.get_u8reg(&A);
                let addr = self.cpu.r.get_u16reg(rr);
                let src_v = self.mmu.read8(addr);
                let result = dst_v.wrapping_sub(src_v);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (dst_v & 0x0F) < (src_v & 0x0f);
                self.cpu.r.subtract_n_flag = true;
                self.cpu.r.carry_flag = (dst_v as u16) < (src_v as u16);
                self.cpu.r.set_u8reg(&A, result);
            }
            Math::Inc_r(dst_r) => {
                self.inc_pc(1);
                let src_v = self.cpu.r.get_u8reg(dst_r);
                let result = src_v.wrapping_add(1);
                self.cpu.r.set_u8reg(dst_r, result);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (result & 0x0F) + 1 > 0x0F;
                self.cpu.r.subtract_n_flag = false;
            }
            Math::Dec_r(r) => {
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(r);
                let result = val.wrapping_sub(1);
                self.cpu.r.set_u8reg(r, result);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (result & 0x0F) == 0;
                self.cpu.r.subtract_n_flag = true;
            }
            Math::Inc_rr(rr) => {
                self.inc_pc(1);
                let val = self.cpu.r.get_u16reg(rr);
                let result = val.wrapping_add(1);
                self.cpu.r.set_u16reg(rr, result);
            }
            Math::Dec_rr(rr) => {
                self.inc_pc(1);
                let mut val = self.cpu.r.get_u16reg(rr);
                val -= 1;
                self.cpu.r.set_u16reg(rr, val);
            }
            Math::BIT(b,r) => {
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(r);
                self.inc_pc(1);
                self.cpu.r.zero_flag = !get_bit_as_bool(val, *b);
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = true;
            }
            Math::BITR2(b,r) => {
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(r);
                let val = self.mmu.read16(addr);
                let mask = match b {
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
                self.cpu.r.zero_flag = !((val & mask) > 0);
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = true;
            }
            Math::RLC(r) => {
                self.inc_pc(1);
                let r_val = self.cpu.r.get_u8reg(r);
                let carry_flag = r_val & 0x80 == 0x80;
                let f_val = (r_val << 1) | if carry_flag { 1 } else { 0 };
                self.cpu.r.half_flag = false;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.zero_flag = f_val == 0;
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(r,f_val);
                self.inc_pc(1);
            }
            Math::RLCA() => {
                self.inc_pc(1);
                let r_val = self.cpu.r.get_u8reg(&A);
                let carry_flag = r_val & 0x80 == 0x80;
                let f_val = (r_val << 1) | if carry_flag { 1 } else { 0 };
                self.cpu.r.half_flag = false;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.zero_flag = f_val == 0;
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(&A,f_val);
            }
            Math::RL(r) => {
                self.inc_pc(1);
                let r_val = self.cpu.r.get_u8reg(r);
                let carry_flag = r_val & 0x80 == 0x80;
                let f_val = (r_val << 1) | (if self.cpu.r.carry_flag { 1 } else { 0 });
                self.cpu.r.half_flag = false;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.zero_flag = f_val == 0;
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(r, f_val);
                self.inc_pc(1);
            }
            Math::RLA() => {
                self.inc_pc(1);
                let r_val = self.cpu.r.get_u8reg(&A);
                let carry_flag = r_val & 0x80 == 0x80;
                let f_val = (r_val << 1) | (if self.cpu.r.carry_flag { 1 } else { 0 });
                self.cpu.r.half_flag = false;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.zero_flag = f_val == 0;
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(&A, f_val);
            }
            Math::RRC(r) => {
                self.inc_pc(1);
                let r_val = self.cpu.r.get_u8reg(r);
                let carry_flag = r_val & 0x01 == 0x01;
                let f_val = (r_val >> 1) | (if carry_flag {0x80} else { 0x00 });
                self.cpu.r.half_flag = false;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.zero_flag = f_val == 0;
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(r, f_val);
                self.inc_pc(1);
            }
            Math::RRCA() => {
                self.inc_pc(1);
                let r_val = self.cpu.r.get_u8reg(&A);
                let carry_flag = r_val & 0x01 == 0x01;
                let f_val = (r_val >> 1) | (if carry_flag {0x80} else { 0x00 });
                self.cpu.r.half_flag = false;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.zero_flag = f_val == 0;
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(&A, f_val);
            }
            Math::RR(r) => {
                self.inc_pc(1);
                let r_val = self.cpu.r.get_u8reg(r);
                let carry_flag = r_val & 0x01 == 0x01;
                let f_val = (r_val >> 1) | (if self.cpu.r.carry_flag { 0x80 } else { 0x00 });
                self.cpu.r.half_flag = false;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.zero_flag = f_val == 0;
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(r, f_val);
                self.inc_pc(1);
            }
            Math::RRA() => {
                self.inc_pc(1);
                let r_val = self.cpu.r.get_u8reg(&A);
                let carry_flag = r_val & 0x01 == 0x01;
                let f_val = (r_val >> 1) | (if self.cpu.r.carry_flag { 0x80 } else { 0x00 });
                self.cpu.r.half_flag = false;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.zero_flag = f_val == 0;
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(&A, f_val);
            }
            Math::SLA(r) => {
                self.inc_pc(1);
                let n = self.cpu.r.get_u8reg(r);
                let v = self.cpu.r.get_u8reg(&A);
                let (v2,carry) = v.overflowing_shl(n as u32);
                self.cpu.r.set_u8reg(&A,v2);
                self.cpu.r.zero_flag = v2 == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = false;
                self.cpu.r.carry_flag = carry;
                self.inc_pc(1);
            }
        }
    }
    fn execute_jump_instructions(&mut self, jump: &Jump) {
        match jump {
            Jump::Absolute_u16() => {
                let addr = self.mmu.read16(self.cpu.r.pc + 1);
                self.set_pc(addr);
                // println!("abs jump to {:04x}",addr);
                debug!("Abs Jump to {:04x}",addr);
            },
            Jump::Relative_cond_carry_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                // println!("carry flag is set to {}",self.cpu.r.carry_flag);
                if self.cpu.r.carry_flag { self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16); }
            },
            Jump::Relative_cond_notcarry_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                if !self.cpu.r.carry_flag { self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16); }
            }
            Jump::Relative_cond_zero_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                if self.cpu.r.zero_flag { self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16); }
            }
            Jump::Relative_cond_notzero_i8() => {
                self.cpu.inc_pc();
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                if !self.cpu.r.zero_flag { self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16); }
            },
            Jump::Absolute_cond_notzero_u16() => {
                self.inc_pc(1);
                let dst = self.mmu.read16(self.cpu.r.pc);
                self.inc_pc(1);
                self.inc_pc(1);
                if !self.cpu.r.zero_flag {
                    self.set_pc(dst);
                }
            },
            Jump::Relative_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16);
            }
        }
    }
    fn inc_sp(&mut self) {
        let (sp, overflowed) = self.cpu.r.sp.overflowing_add(1);
        if overflowed {
            self.mmu.print_cram();
            panic!("SPC overflowed memory");
        }
        self.cpu.r.sp = sp;
    }
    fn dec_sp(&mut self) {
        let (sp, overflowed) = self.cpu.r.sp.overflowing_sub(1);
        if overflowed {
            self.mmu.print_cram();
            panic!("SPC overflowed memory");
        }
        self.cpu.r.sp = sp;
    }
    fn draw_screen(&mut self, backbuffer: &mut Bitmap) {
        // let mut backbuffer:Bitmap = &backbuffer_lock.lock().unwrap();
        backbuffer.clear_with(0,0,0);
        self.draw_vram(backbuffer);
    }
}

pub fn start_debugger(cpu: Z80, mmu: MMU, cart: Option<RomFile>,
                      fast_forward: u32,
                      screen: bool,
                      breakpoint:u16,
                      verbose:bool,
                      interactive:bool) -> Result<()> {
    let backbuffer_bitmap = Bitmap::init(256,256);
    let backbuffer = Arc::new(Mutex::new(backbuffer_bitmap));
    let mut ctx = Ctx {
        cpu, mmu, clock:0,
        running:true,
        interactive,
        cart,
        full_registers_visible:false,
        test_memory_visible: false,
        needs_redraw: false
    };
    let mut term = Term::stdout();

    println!("fast forwarding: {}",fast_forward);
    for n in 0..fast_forward {
        ctx.execute(&mut term, false)?;
    }

    let mut screen = Screen::init(256, 256);

    let bb2 = backbuffer.clone();
    let hand = thread::spawn(move | |
        {
        while ctx.running {
            if breakpoint > 0 && (ctx.cpu.r.pc == breakpoint) {
                println!("done hit the breakpoint");
                ctx.interactive = true;
            }
            if ctx.interactive {
                step_forward(&mut ctx, &mut term).unwrap();
            } else {
                ctx.execute(&mut term, verbose).unwrap();
            }
            if ctx.needs_redraw {
                let mut bb = bb2.lock().unwrap();
                ctx.draw_screen(&mut bb);
                // println!("redrew to back buffer");
            }
        }
    });
    while true {
        let should_continue = screen.update_screen(&backbuffer);
        if !should_continue {
            break;
        }
    }
    // hand.join();
    Ok(())
}

fn step_forward(ctx: &mut Ctx, term: &mut Term)  -> Result<()>{
    let border = Style::new().bg(Color::Magenta).black();
    // term.clear_screen()?;
    if let Some(cart) = &ctx.cart {
        term.write_line(&format!("executing rom {}", cart.path))?;
    }
    term.write_line(&format!("clock {}",&ctx.clock))?;
    term.write_line(&border.apply_to("========================================").to_string())?;

    // print the current memory
    let start = ctx.cpu.r.pc;
    let back:i32 = 2;
    for n in 0..5 {
        let iv = (start as i32) + n - back;
        // println!("n is {} {}",n,iv);
        if iv < 0 {
            term.write_line("----")?;
            continue;
        }
        let addr = iv as u16;
        // println!("address is {:04x}",addr);
        let prefix = if addr == start { " *"} else {"  "};
        let data = ctx.mmu.read8(addr);
        term.write_line(&format!("{} {:04x}  {:02x}",prefix,addr,data))?;
    }

    {
        // print the registers
        let reg_style = Style::new().bg(White).red().underlined();
        term.write_line(&format!("PC: {:04x}  SP:{:04x}", ctx.cpu.r.pc, ctx.cpu.r.sp))?;
        let regs_u8 = format!("A:{:02x}  B:{:02x}  C:{:02x}  D:{:02x}  E:{:02x}  H:{:02x}  L:{:02x} ",
                              ctx.cpu.r.a,
                              ctx.cpu.r.b,
                              ctx.cpu.r.c,
                              ctx.cpu.r.d,
                              ctx.cpu.r.e,
                              ctx.cpu.r.h,
                              ctx.cpu.r.l,
        );
        term.write_line(&reg_style.apply_to(regs_u8).to_string())?;
        let regs_u16 = format!("BC:{:04x}  DE:{:04x}  HL:{:04x}",
                               ctx.cpu.r.get_bc(),
                               ctx.cpu.r.get_de(),
                               ctx.cpu.r.get_hl(),
        );
        term.write_line(&regs_u16)?;
        let flag_style = Style::new().blue();
        term.write_line(&format!("flags Z:{}   N:{}  H:{}  C:{}",
                                 flag_style.apply_to(ctx.cpu.r.zero_flag),
                                 flag_style.apply_to(ctx.cpu.r.subtract_n_flag),
                                 flag_style.apply_to(ctx.cpu.r.half_flag),
                                 flag_style.apply_to(ctx.cpu.r.carry_flag),
        ))?;
        // term.write_line(&format!(" Screen: LY {:02x}",ctx.mmu.hardware.LY))?;
    }
    if ctx.full_registers_visible {
        show_full_hardware_registers(term,ctx)?;
    }
    if ctx.test_memory_visible {
        show_test_memory(term, ctx)?;
    }

    // print info about the next opcode
    let primary = Style::new().green().bold();
    let bold = Style::new().reverse().red();
    let italic = Style::new().italic();
    let (opcode, _instr_len) = fetch_opcode(&ctx.cpu, &ctx.mmu);
    let op = lookup_opcode(opcode);
    if let Some(ld) = op {
        term.write_line(&primary.apply_to(&format!("${:04x} : {}: {}",
                                                   ctx.cpu.r.pc,
                                                   bold.apply_to(format!("{:02X}",opcode)),
                                                   italic.apply_to(opcodes::lookup_opcode_info(ld)),
        )).to_string())?;
    } else {
        println!("current cycle {}", ctx.clock);
        panic!("unknown op code")
    }


    let commands = Style::new().reverse();
    term.write_line(&commands.apply_to(&format!("j=step, J=step 16, r=hdw reg, v=vram dump s=screen dump c=cart_dump o=objram dump")).to_string())?;
    let ch = term.read_char().unwrap();
    match ch{
        'r' => ctx.full_registers_visible = !ctx.full_registers_visible,
        'j' => ctx.execute(term,false)?,
        'J' => {
            term.write_line("doing 16 instructions")?;
            for n in 0..16 {
                ctx.execute(term, false)?;
            }
        },
        'u' => {
            term.write_line("doing 256 instructions")?;
            for n in 0..256 {
                ctx.execute(term, false)?;
            }
        },
        'c' => dump_cart_rom(term, ctx)?,
        'v' => dump_vram(term, ctx)?,
        'o' => dump_oram(term, ctx)?,
        's' => dump_screen(term, ctx)?,
        'i' => dump_interrupts(term, ctx)?,
        't' => ctx.test_memory_visible = !ctx.test_memory_visible,
        'm' => dump_all_memory(term,ctx)?,
        'b' => {
            ctx.interactive = false;
            ctx.execute(term,false)?;
        },
        _ => {}
    };
    Ok(())
}

fn dump_all_memory(term: &Term, ctx: &Ctx) -> Result<()> {
    for (n,chunk) in ctx.mmu.data.chunks(32*2).enumerate() {
        let line_str:String = chunk.iter().map(|b|format!("{:02x}",b)).collect();
        let addr:u32 = (n * 32 * 2) as u32;
        // println!("addr {:04x}",addr);
        match addr {
            0x0000 => println!("start: cart fixed"),
            0x4000 => println!("cart: switchable"),
            0x8000 => println!("8 KiB Video RAM        (VRAM)\n  Tile Data Block 0"),
            0x8800 => println!("8 KiB Video RAM        (VRAM)\n  Tile Data Block 1"),
            0x9000 => println!("8 KiB Video RAM        (VRAM)\n  Tile Data Block 2"),
            0xA000 => println!("8 KiB External RAM"),
            0xC000 => println!("4 KiB Work RAM         (WRAM)"),
            0xD000 => println!("4 KiB Work RAM         (WRAM)"),
            0xE000 => println!("mirror of C000~DDFF"),
            0xFE00 => println!("Sprite attribute table (OAM)"),
            0xFEA0 => println!("not usable"),
            0xFF00 => println!("I/O Registers"),
            0xFF80 => println!("High RAM"),
            _ => {}
        }
        term.write_line(&format!("{:04x}  {}", addr, line_str))?;
    }
    Ok(())
}

fn dump_interrupts(term: &Term, ctx: &Ctx) -> Result<()>{
    let reg = Style::new().bg(Color::Cyan).red().bold();
    term.write_line(&reg.apply_to("INTERRUPTS").to_string())?;
    term.write_line(&format!("vblank = {}",ctx.mmu.hardware.vblank_interrupt_enabled))?;
    Ok(())
}

fn dump_oram(term: &Term, ctx: &Ctx) -> Result<()>{
    term.write_line("object memory")?;
    let data = ctx.mmu.fetch_oram();
    for line in data.chunks(32) {
        let line_str:String = line.iter()
            .map(|b|format!("{:02x}",b))
            .collect();
        term.write_line(&line_str)?;
    }
    Ok(())
}

fn show_test_memory(term: &Term, ctx: &Ctx) -> Result<()>{
    term.write_line(&format!("memory at {:04X}",TEST_ADDR))?;
    let data = ctx.mmu.fetch_test_memory();
    for line in data.chunks(32) {
        let line_str:String = line.iter()
            .map(|b|format!("{:02x}",b))
            .collect();
        term.write_line(&line_str)?;
    }

    Ok(())
}

fn dump_cart_rom(term: &Term, ctx: &Ctx) -> Result<()>  {
    term.write_line("cart rom starting at 0x0100")?;
    let rom = ctx.mmu.fetch_rom_bank_1();
    print_memory_to_console(rom,term,ctx)?;
    Ok(())
}

impl Ctx {
    fn draw_vram(&mut self, backbuffer: &mut Bitmap) -> Result<()> {
        let lcdc = self.mmu.hardware.LCDC;
        // println!("bg and window enable/priority? {}",get_bit_as_bool(lcdc,0));
        // println!("sprites displayed? {}",get_bit_as_bool(lcdc,1));
        // println!("sprite size. 8x8 or 8x16? {}",get_bit_as_bool(lcdc,2));
        // println!("bg tile map area  {}",get_bit_as_bool(lcdc,3));
        // println!("bg tile data area? {}",get_bit_as_bool(lcdc,4));
        // println!("window enable? {}",get_bit_as_bool(lcdc,5));
        // println!("window tile map area? {}",get_bit_as_bool(lcdc,6));
        // println!("LCD enable? {}",get_bit_as_bool(lcdc,7));

        let screen_on = get_bit_as_bool(lcdc, 7);
        let window_enabled = get_bit_as_bool(lcdc, 5);
        let bg_enabled = true; //bg is always enabled
        let mut bg_tilemap_start = 0x9800;
        let mut bg_tilemap_end = 0x9BFF;
        if get_bit_as_bool(lcdc,3) {
            bg_tilemap_start = 0x9C00;
            bg_tilemap_end  = 0x9FFF;
        }
        let bg_tilemap = &self.mmu.data[bg_tilemap_start .. bg_tilemap_end];

        let mut low_data_start = 0x9000;
        let mut low_data_end = 0x97FF;
        if get_bit_as_bool(lcdc, 4) {
            low_data_start = 0x8000;
            low_data_end = 0x87FF;
        }
        let lo_data = &self.mmu.data[low_data_start..low_data_end];

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
                                     x * 8 + (self.mmu.hardware.SCX as usize),
                                     y * 8 + (self.mmu.hardware.SCY as usize),
                                     tile_id,
                                     lo_data);
                    }
                }
            }
        }
        Ok(())
    }
}

fn dump_vram(term: &Term, ctx: &mut Ctx) -> Result<()>{
    // ctx.draw_vram();
    // ctx.backbuffer.write_to_file("vram.png");
    Ok(())
}

fn print_memory_to_console(mem: &[u8], term: &Term, ctx: &Ctx) ->Result<()>{
    for line in mem.chunks(64) {
        let line_str:String = line.iter()
            .map(|b|format!("{:02x}",b))
            .collect();
        term.write_line(&line_str)?;
    }
    Ok(())
}

fn dump_screen(term: &Term, ctx: &Ctx) -> Result<()>{
    let mut buf = Bitmap::init(32*8,32*8); // actual window buffer is 256x256
    buf.clear_with(0,0,0);
    let vram:&[u8] = ctx.mmu.get_current_bg_display_data();
    let tiledata:&[u8] = ctx.mmu.fetch_tiledata_block2();
    // println!("vram len is {}",vram.len());
    for (y,row) in vram.chunks_exact(32).enumerate() {
        for (x, tile_id) in row.iter().enumerate() {
            draw_tile_at(&mut buf, x*8, y*8, tile_id, tiledata);
        }
        let hex_tile = row.iter().map(|b| format!("{:02x} ", b)).collect::<String>();
        println!("{}",hex_tile);
    }
    buf.write_to_file("./screen.png");
    term.write_line(&format!("wrote out to 'screen.png'"))?;
    Ok(())
}

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

fn show_full_hardware_registers(term: &Term, ctx: &Ctx) -> Result<()>{
    let reg = Style::new().bg(Color::Cyan).red().bold();
    term.write_line(&format!("registers are {} ",reg.apply_to("cool")))?;
    term.write_line(&reg.apply_to(&format!("LY   {:02x}",ctx.mmu.hardware.LY)).to_string())?;
    term.write_line(&format!("LYC  {}",ctx.mmu.hardware.LYC))?;
    term.write_line(&format!("SCX  {}",ctx.mmu.hardware.SCX))?;
    term.write_line(&format!("SCY  {}",ctx.mmu.hardware.SCY))?;
    term.write_line(&format!("LCDC {:8b}",ctx.mmu.hardware.LCDC))?;
    term.write_line(&format!("STAT {:8b}",ctx.mmu.hardware.STAT))?;
    term.write_line(&format!("BGP  {:8b}",ctx.mmu.hardware.BGP))?;
    term.write_line(&format!("OBP0 {:8b}",ctx.mmu.hardware.OBP0))?;
    term.write_line(&format!("OBP1 {:8b}",ctx.mmu.hardware.OBP1))?;
    term.write_line(&format!("WY   {:8b}",ctx.mmu.hardware.WY))?;
    term.write_line(&format!("WX   {:8b}",ctx.mmu.hardware.WX))?;
    Ok(())
}

fn fetch_opcode(cpu: &Z80, mmu: &MMU) -> (u16,u16) {
    let pc = cpu.r.pc;
    // println!("pc is {}",pc);
    let fb:u8 = mmu.read8(pc);
    // println!("fb is {:x}",fb);
    if fb == 0xcb {
        let sb:u8 = mmu.read8(pc+1);
        (0xcb00 | sb as u16,2)
    } else {
        (fb as u16, 1)
    }
}


impl Ctx {
    pub(crate) fn execute_test(&mut self) {
        let (opcode, _off) = fetch_opcode_from_memory(&self.cpu, &self.mmu);
        if let Some(instr) = lookup_opcode(opcode) {
            println!("executing {}  PC {:04x}  SP {:04x}  {:02x}  {:?}",  self.clock,
                     self.cpu.r.pc, self.cpu.r.sp, opcode, instr);
            self.execute_instruction(&instr);
            print_registers(&self.cpu, &self.mmu);
        } else {
            println!("current cycle {}", self.clock);
            println!("unknown op code {:04x}",opcode);
            panic!("unknown op code");
        }
        self.mmu.update(&mut self.cpu);
        self.clock+=1;
    }
}

fn print_registers(cpu: &Z80, mmu:&MMU) {
    println!("A {:02x}", cpu.r.get_u8reg(&RegisterName::A));
    println!("B {:02x}", cpu.r.get_u8reg(&RegisterName::B));
    println!("LY {:02x} ", mmu.hardware.LY);
    println!("PC {:02x} ", cpu.get_pc());
    println!("SP {:02x} ", cpu.get_sp());
}

#[test]
fn test_write_register_A() {
    let rom:[u8;2] = [0x3e,0x05]; // LD A, 0x05
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    ctx.execute_test();
    assert_eq!(ctx.cpu.r.get_u8reg(&A),0x05);
}
#[test]
fn test_write_register_D() {
    let rom:[u8;2] = [0x16,0x05]; // LD D, 0x05
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    ctx.execute_test();
    assert_eq!(ctx.cpu.r.get_u8reg(&D),0x05);
}
#[test]
fn test_write_register_DE() {
    let rom:[u8;2] = [0x16,0x05]; // LD D, 0x05
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    ctx.execute_test();
    assert_eq!(ctx.cpu.r.get_u8reg(&D),0x05);
}

#[test]
fn test_push_pop() {
    let rom:[u8;10] = [
        0x31, 0xfe, 0xff, // LD SP u16, set the stack pointer to fffe
        0x06, 0x04, // LD B, 04
        0xC5, // PUSH BC
        0xCB, 0x11, // RL C
        0x17,// RLA
        0xC1, // POP BC
    ];
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());

    println!("stack {:?} ",ctx.mmu.get_stack_16());
    ctx.execute_test(); // set SP to fffe
    ctx.execute_test(); // LD B, 04
    println!("stack {:?} ",ctx.mmu.get_stack_16());
    assert_eq!(ctx.cpu.r.sp,0xfffe);
    println!("BC {:04x}",ctx.cpu.r.get_u16reg(&BC));
    println!("B {:02x}",ctx.cpu.r.get_u8reg(&B));
    println!("C {:02x}",ctx.cpu.r.get_u8reg(&C));
    assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
    assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
    assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);
    // println!("stack {:?} ",ctx.mmu.get_stack_16());
    println!("B {:02x} C {:02x} BC {:04x}",ctx.cpu.r.get_u8reg(&B), ctx.cpu.r.get_u8reg(&C), ctx.cpu.r.get_u16reg(&BC));

    println!("pushing BC");
    ctx.execute_test(); // PUSH BC
    assert_eq!(ctx.cpu.r.sp,0xfffe-2);
    assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
    assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
    assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);
    println!("stack {:?} ",ctx.mmu.get_stack_16());

    ctx.execute_test(); // RL C
    ctx.execute_test(); // RLA

    println!("POPPING BC");
    ctx.execute_test(); // POP BC
    println!("stack {:?} ",ctx.mmu.get_stack_16());
    println!("B {:02x} C {:02x} BC {:04x}",ctx.cpu.r.get_u8reg(&B), ctx.cpu.r.get_u8reg(&C), ctx.cpu.r.get_u16reg(&BC));
    assert_eq!(ctx.cpu.r.sp,0xfffe);
    assert_eq!(ctx.cpu.r.get_u16reg(&BC),0x0400);
    assert_eq!(ctx.cpu.r.get_u8reg(&B),0x04);
    assert_eq!(ctx.cpu.r.get_u8reg(&C),0x00);

}

#[test]
fn test_vblank_interrupt() {
    let mut rom = vec![
        // start at 0x0000
        op(Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::SP))), // LD SP 16 init SP to 0xFFFE
        0xFE, 0xFF,
        //register handler?
        //enable interrupts

        // enable vblank interrupts by setting 0xFFF0 to 0x01
        op(Instr::LoadInstr(Load_R_u8(B))), // LD B, 0x01
        0x01,
        // LD HL, 0xFF0F
        op(Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::HL))),
        0x0F,
        0xFF,
        op(Instr::LoadInstr(Load::Load_addr_R2_r(DoubleRegister::HL,RegisterName::B))), // write 0x01 to the IE register

        //turn on interrupts
        op(Instr::SpecialInstr(Special::EnableInterrupts())), // EI code

        // set B to 65
        op(Instr::LoadInstr(Load_R_u8(B))), // LD B 0x65
        0x65, //02

        op(Instr::LoadInstr(Load::Load_R_R(RegisterName::A,RegisterName::B))), //copy B to A
        op(Instr::CompareInst(Compare::CP_A_n())), //spin loop looking for A is 66
        0x66, // u8 66
        op(Instr::JumpInstr(Jump::Relative_cond_notzero_i8())),// JR NZ jump back two spaces if not zero yet//05
        0xfb, // -2


        op(Instr::LoadInstr(Load_R_u8(B))), //if got here then test is over, set B to 67
        0x67,
    ];

    println!("length is {}",rom.len());
    for i in rom.len() .. 0x40 {
        rom.push(op(Instr::SpecialInstr(Special::NOOP())));
    }
    println!("length is {}",rom.len());
    // add the vblank handler
    rom.push(op(Instr::LoadInstr(Load_R_u8(B)))); // set B to 66
    rom.push(0x66);
    rom.push(op(Instr::SpecialInstr(Special::RETI())));
    println!("length is {}",rom.len());


    // test loops until B is 67
    let mut ctx:Ctx = Ctx::make_test_context(&rom.to_vec());
    print_registers(&ctx.cpu, &ctx.mmu);
    while ctx.cpu.r.get_u8reg(&B) != 0x67 {
        ctx.execute_test();
        // if instruction counter reaches some high number then we clearly got stuck
        if ctx.clock > 3090 {
            panic!("stuck inside a loop!");
        }
        // if ctx.mmu.hardware.LY == 0x99 {
        //     panic!("hit the vblank");
        // }
    }
    println!("reached the end");
}

fn op(ins: Instr) -> u8 {
    println!("lookup opcode for instruction {:?}",ins);
    match ins {
        Instr::LoadInstr(Load::Load_R_u8(RegisterName::B)) => 0x06,
        Instr::LoadInstr(Load::Load_R_R(RegisterName::A,RegisterName::B)) => 0x78,
        Instr::CompareInst(Compare::CP_A_n()) => 0xFE,
        Instr::JumpInstr(Jump::Relative_cond_notzero_i8()) => 0x20,
        Instr::SpecialInstr(Special::EnableInterrupts()) => 0xFB,
        Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::HL)) => 0x21,
        Instr::LoadInstr(Load::Load_R2_U16(DoubleRegister::SP)) => 0x31,
        Instr::LoadInstr(Load::Load_addr_R2_r(DoubleRegister::HL, B)) => 0x70,
        Instr::SpecialInstr(Special::NOOP()) => 0x00,
        Instr::SpecialInstr(Special::RETI()) => 0xD9,
        _ => panic!("unknown instruction to parse"),
    }
}
