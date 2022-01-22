use std::io;
use console::{Color, style, Style, Term};
use io::Result;
use std::fs::File;
use std::io::{BufWriter, Error};
use std::path::Path;
use console::Color::{Black, White};
use serde_json::Value;
use crate::{Cli, common, fetch_opcode_from_memory, MMU, Z80};
use crate::common::{Bitmap, RomFile};
use crate::opcodes::{Compare, Instr, Jump, Load, lookup_opcode, Math, Special, u8_as_i8};
use crate::opcodes::RegisterName::A;

struct Ctx {
    cpu:Z80,
    mmu:MMU,
    opcodes:Value,
    clock:u32,
    running:bool,
}

impl Ctx {
    pub(crate) fn execute(&mut self, term: &mut Term) {
        // term.write_line(&format!("PC at {:04x}", self.cpu.r.pc));
        let (opcode, off) = fetch_opcode_from_memory(&mut self.cpu, &mut self.mmu);
        // term.write_line(&format!("--PC at {:04x}  op {:0x}", self.cpu.r.pc, opcode));
        if let Some(instr) = lookup_opcode(opcode) {
            self.execute_instruction(&instr);
        } else {
            println!("current cycle {}", self.clock);
            println!("unknown op code {:04x}",opcode);
            panic!("unknown op code");
        }
        self.mmu.update();
        self.clock+=1;
    }
    fn execute_instruction(&mut self, inst: &Instr) {
        match inst {
            Instr::Special(special)  => self.execute_special_instructions(special),
            Instr::Load(load)         => self.execute_load_instructions(load),
            Instr::Compare(comp)   => self.execute_compare_instructions(comp),
            Instr::Math(math)         => self.execute_math_instructions(math),
            Instr::Jump(jump)         => self.execute_jump_instructions(jump),
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
}

impl Ctx {
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
                self.inc_pc(1);

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
            Math::OR_A_r(r) => {
                self.inc_pc(1);
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) | self.cpu.r.get_u8reg(r));
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
            Math::ADD_R_u8(r) => {
                self.inc_pc(1);
                let v1 = self.cpu.r.get_u8reg(r);
                let v2 = self.mmu.read8(self.cpu.r.pc);
                let result = v1.wrapping_add(v2);
                self.inc_pc(1);
                self.cpu.r.set_u8reg(r, v2);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = ((v1 & 0xF) + (v2 & 0xF) > 0xFF);
                self.cpu.r.carry_flag = ((v1 as u16) + (v2 as u16) > 0xFF);
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
            Math::Inc_r(dst_r) => {
                self.inc_pc(1);
                let src_v = self.cpu.r.get_u8reg(dst_r);
                let result = src_v.wrapping_add(1);
                self.cpu.r.set_u8reg(dst_r, result);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = ((result & 0x0F) + 1 > 0x0F);
                self.cpu.r.subtract_n_flag = false;
            }
            Math::Dec_r(r) => {
                self.inc_pc(1);
                let mut val = self.cpu.r.get_u8reg(r);
                let result = val.wrapping_sub(1);
                self.cpu.r.set_u8reg(r, result);
                self.cpu.r.zero_flag = result == 0;
                self.cpu.r.half_flag = (result & 0x0F) == 0;
                self.cpu.r.subtract_n_flag = true;
            }
            Math::Inc_rr(rr) => {
                self.inc_pc(1);
                let mut val = self.cpu.r.get_u16reg(rr);
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
                self.cpu.r.zero_flag = (f_val == 0);
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
                self.cpu.r.zero_flag = (f_val == 0);
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
                self.cpu.r.zero_flag = (f_val == 0);
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
                self.cpu.r.zero_flag = (f_val == 0);
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
                self.cpu.r.zero_flag = (f_val == 0);
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
                self.cpu.r.zero_flag = (f_val == 0);
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
                self.cpu.r.zero_flag = (f_val == 0);
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
                self.cpu.r.zero_flag = (f_val == 0);
                self.cpu.r.carry_flag = carry_flag;
                self.cpu.r.set_u8reg(&A, f_val);
            }
        }
    }
    fn execute_jump_instructions(&mut self, jump: &Jump) {
        match jump {
            Jump::JumpAbsolute_u16() => {
                let addr = self.mmu.read16(self.cpu.r.pc + 1);
                self.set_pc(addr);
            },
            Jump::JumpRelative_cond_carry_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                // println!("carry flag is set to {}",self.cpu.r.carry_flag);
                if self.cpu.r.carry_flag { self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16); }
            },
            Jump::JumpRelative_cond_notcarry_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                if !self.cpu.r.carry_flag { self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16); }
            }
            Jump::JumpRelative_cond_zero_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                if self.cpu.r.zero_flag { self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16); }
            }
            Jump::JumpRelative_cond_notzero_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                if !self.cpu.r.zero_flag { self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16); }
            },
            Jump::JumpRelative_i8() => {
                self.inc_pc(1);
                let e = u8_as_i8(self.mmu.read8(self.cpu.r.pc));
                self.inc_pc(1);
                self.set_pc((((self.cpu.r.pc) as i32) + e as i32) as u16);
            }
        }
    }
    fn execute_load_instructions(&mut self, load: &Load) {
        match load {
            Load::Load_r_u8(r) => {
                // load immediate u8 into the register
                self.inc_pc(1);
                let val = self.mmu.read8(self.cpu.r.pc);
                self.cpu.r.set_u8reg(r, val);
                self.inc_pc(1);
            }
            // put the memory address 0xFF00 + n into register r
            Load::Load_high_r_u8(r) => {
                // println!("running LDH");
                let n = self.mmu.read8(self.cpu.r.pc+1);
                let addr = (0xFF00 as u16) + (n as u16);
                self.cpu.r.a = self.mmu.read8(addr);
                // println!("assigned content of mem:{:x} value {:x}, to A",addr,self.cpu.r.a);
                self.inc_pc(2);
            }
            Load::Load_high_u8_r(r) => {
                self.inc_pc(1);
                let e = self.mmu.read8(self.cpu.r.pc);
                self.inc_pc(1);
                let addr = (0xFF00 as u16) + (e as u16);
                let val = self.cpu.r.get_u8reg(r);
                self.mmu.write8(addr, val);
            },
            //LD (FF00+C),A    put contents of A into address 0xF00+C
            Load::Load_high_r_r(off,r) => {
                self.inc_pc(1);
                let v = self.cpu.r.get_u8reg(r);
                let addr = (0xFF00 as u16) + (self.cpu.r.get_u8reg(off) as u16);
                // println!("copying value x{:02x} from register {} to address ${:04x}", v, r, addr);
                self.mmu.write8(addr,v);
                self.inc_pc(1);
            }
            Load::Load_r_r(dst, src) => {
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(src);
                self.cpu.r.set_u8reg(dst,val);
            }

            Load::Load_R2_u16(rr) => {
                self.inc_pc(1);
                let val = self.mmu.read16(self.cpu.r.pc);
                self.inc_pc(1);
                self.inc_pc(1);
                self.cpu.r.set_u16reg(rr,val);
                // println!("copied immediate value {:04x} into register {}",val,rr)
            }
            Load::Load_R_addr_R2(r,rr) => {
                // load to the 8bit register A, data from the address in the 16 bit register
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(rr);
                let val = self.mmu.read8(addr);
                self.cpu.r.set_u8reg(r,val);
                // println!("copied value {:02x} from address {:04x} determined from register {} into register A",val,addr,rr);
            }
            Load::Load_A_addr_R2_inc(rr) => {
                // load to the 8bit register A, data from the address in the 16 bit register, then increment that register
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(rr);
                let val = self.mmu.read8(addr);
                self.cpu.r.set_u8reg(&A,val);
                // println!("copied value {:02x} from address {:04x} determined from register {} into register A",val,addr,rr);
                let (v2,bool) = self.cpu.r.get_u16reg(rr).overflowing_add(1);
                self.cpu.r.set_u16reg(rr,v2);
            },
            Load::Load_addr_R2_A_inc(rr) => {
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(&A);
                let addr = self.cpu.r.get_u16reg(rr);
                self.mmu.write8(addr,val);
                self.cpu.r.set_hl(self.cpu.r.get_hl()+1);
            }
            Load::Load_addr_R2_A_dec(rr) => {
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(&A);
                let addr = self.cpu.r.get_u16reg(rr);
                self.mmu.write8(addr,val);
                self.cpu.r.set_hl(self.cpu.r.get_hl()-1);
            }

            Load::Load_addr_R2_A(rr,r) => {
                //copy contents of A to memory pointed to by RR
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(r);
                let addr = self.cpu.r.get_u16reg(rr);
                self.mmu.write8(addr,val);
            }

            Load::Load_addr_u16_A() => {
                self.inc_pc(1);
                let addr = self.mmu.read16(self.cpu.r.pc);
                let val = self.cpu.r.get_u8reg(&A);
                self.mmu.write8(addr,val);
                self.inc_pc(2);
            }

            Load::Load_addr_u16_R2(rr) => {
                self.inc_pc(1);
                let addr = self.mmu.read16(self.cpu.r.pc);
                let val = self.cpu.r.get_u16reg(rr);
                self.mmu.write16(addr,val);
                self.inc_pc(2);
            }
        }
    }
    fn execute_special_instructions(&mut self, special: &Special) {
        match special {
            Special::DisableInterrupts() => {
                self.inc_pc(1);
                self.mmu.hardware.IME = 0;
            }
            Special::NOOP() => {
                self.inc_pc(1);
            }
            Special::HALT() => {
                self.inc_pc(1);
                self.running = false;
            }
            Special::STOP() => {
                self.inc_pc(1);
                self.mmu.hardware.IME = 0;
            }
            Special::CALL_u16() => {
                self.inc_pc(1);
                let addr = self.mmu.read16(self.cpu.r.pc);
                self.inc_pc(1);
                self.inc_pc(1);
                self.dec_sp();
                self.mmu.write16(self.cpu.r.sp, self.cpu.r.pc);
                self.dec_sp();
                self.set_pc(addr);
            }
            Special::PUSH(rr) => {
                self.inc_pc(1);
                let value = self.cpu.r.get_u16reg(rr);
                self.mmu.write16(self.cpu.r.sp, value);
                self.dec_sp();
                self.dec_sp();
            }
            Special::POP(rr) => {
                self.inc_pc(1);
                let value = self.mmu.read16(self.cpu.r.sp);
                self.inc_sp();
                self.inc_sp();
                self.cpu.r.set_u16reg(rr,value);
            }
            Special::RET() => {
                self.inc_pc(1);
                let addr = self.mmu.read16(self.cpu.r.sp);
                self.inc_sp();
                self.inc_sp();
                self.set_pc(addr);
            }
            Special::RETI() => {
                self.inc_pc(1);
                let addr = self.mmu.read16(self.cpu.r.sp);
                self.inc_sp();
                self.inc_sp();
                self.set_pc(addr);
                self.mmu.hardware.IME = 1;
            }
            Special::RETZ() => {
                self.inc_pc(1);
                if self.cpu.r.zero_flag {
                    let addr = self.mmu.read16(self.cpu.r.sp);
                    self.inc_sp();
                    self.inc_sp();
                    self.set_pc(addr);
                }
            }
            Special::RST(h) => {
                self.inc_pc(1);
                self.mmu.write16(self.cpu.r.sp, self.cpu.r.pc);
                self.set_pc((0x0000 | h) as u16);
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
}

pub fn start_debugger_loop(cpu: Z80, mmu: MMU, opcodes: Value, cart: Option<RomFile>, fast_forward: u32) -> Result<()> {
    let mut ctx = Ctx { cpu, mmu, opcodes, clock:0, running:true};
    let mut term = Term::stdout();
    while ctx.running {
        ctx.execute(&mut term);
    }
    Ok(())
}

pub fn start_debugger(cpu: Z80, mmu: MMU, opcodes: Value, cart: Option<RomFile>, fast_forward: u32) -> Result<()> {
    let mut ctx = Ctx { cpu, mmu, opcodes, clock:0, running:true};
    let mut term = Term::stdout();
    let mut full_registers_visible = false;

    for n in 0..fast_forward {
        ctx.execute(&mut term);
    }

    let border = Style::new().bg(Color::Magenta).black();
    loop {
        // term.clear_screen()?;
        if let Some(cart) = &cart {
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
            let prefix = if(addr == start) { " *"} else {"  "};
            let data = ctx.mmu.read8(addr);
            term.write_line(&format!("{} {:04x}  {:02x}",prefix,addr,data));
        }

        {
            // print the registers
            let reg_style = Style::new().bg(White).red().underlined();
            term.write_line(&format!("PC: {:04x}", ctx.cpu.r.pc))?;
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
        if full_registers_visible {
            show_full_hardware_registers(&term,&ctx);
        }

        // print info about the next opcode
        let primary = Style::new().green().bold();
        let bold = Style::new().reverse().red();
        let italic = Style::new().italic();
        let (opcode, instr_len) = fetch_opcode(&mut ctx.cpu, &mut ctx.mmu);
        let op = lookup_opcode(opcode);
        if let Some(ld) = op {
            term.write_line(&primary.apply_to(&format!("${:04x} : {}: {}",
                                                       ctx.cpu.r.pc,
                                                       bold.apply_to(format!("{:02X}",opcode)),
                                                       italic.apply_to(lookup_opcode_info(ld)),
            )).to_string())?;
        } else {
            println!("current cycle {}", ctx.clock);
            panic!("unknown op code")
        }


        let commands = Style::new().reverse();
        term.write_line(&commands.apply_to(&format!("j=step, J=step 16, r=hdw reg, v=vram dump s=screen dump")).to_string())?;
        let ch = term.read_char()?;
        if ch == 'r' {
            full_registers_visible = !full_registers_visible;
        }
        if ch == 'J' {
            term.write_line(&format!("doing 16 instructions"))?;
            for n in 0..16 {
                ctx.execute(&mut term);
            }
        }
        if ch == 'j' {
            ctx.execute(&mut term)
        }
        if ch == 'v' { dump_vram(&term,&ctx); }
        if ch == 's' { dump_screen(&term,&ctx); }
    }

    Ok(())
}

fn dump_vram(term: &Term, ctx: &Ctx) {
    let vram:&[u8] = ctx.mmu.fetch_tiledata_block3();
    const tile_count:usize = 127;
    let mut buf = Bitmap::init(16*8,16);

    let mut tc = 0;
    for tile in vram.chunks_exact(16) {
        // term.write_line(&format!("chunk is {}",chunk.len()));
        let hex_tile = tile
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect::<String>();
        // term.write_line(&hex_tile);
        let mut rc = 0;
        for row in tile.chunks_exact(2) {
            // let addr = 0x9000 + tc*16;
            // let hex = format!("{:02x}{:02x}",row[0],row[1]);
            // let pixels = pixel_row_to_string(row);
            // println!("row count {}", pixel_row_to_colors(row).len());
            // let n = ((tc * 8 + rc * buf.width())*4) as usize;
            let mut pc = 0;
            for color in pixel_row_to_colors(row) {
                let px = (tc*8+pc);
                let py = rc;
                match color {
                    0 => buf.set_pixel_rgb(px,py, 50,50,50),
                    1 => buf.set_pixel_rgb(px,py,100,100,100),
                    2 => buf.set_pixel_rgb(px,py,0,0,0),
                    3 => buf.set_pixel_rgb(px,py,200,200,200),
                    _ => {

                    }
                }
                pc += 1;
            }
            // term.write_line(&format!("{:04x}  {}  {}",addr, hex,pixels));
            rc += 1;
        }
        // term.write_line(&"");
        tc += 1;
    }

    let path = Path::new(r"vram.png");
    let file = File::create(path).unwrap();
    let ref mut w = BufWriter::new(file);

    let mut encoder = png::Encoder::new(w, buf.w as u32, buf.h as u32);
    encoder.set_color(png::ColorType::Rgba);
    encoder.set_depth(png::BitDepth::Eight);

    let mut writer = encoder.write_header().unwrap();
    println!("tile count is {}",tile_count);
    writer.write_image_data(buf.data.as_slice()).unwrap();
    println!("wrote out to 'vram.png'");
}

fn dump_screen(term: &Term, ctx: &Ctx) {
    let mut buf = Bitmap::init(32*8,32*8); // actual window buffer is 256x256
    buf.clear_with(0,0,0);
    let vram:&[u8] = ctx.mmu.get_current_bg_display_data();
    let tiledata:&[u8] = ctx.mmu.fetch_tiledata_block3();
    // println!("vram len is {}",vram.len());
    for (y,row) in vram.chunks_exact(32).enumerate() {
        for (x, tile_id) in row.iter().enumerate() {
            draw_tile_at(&mut buf, x*8, y*8, tile_id, tiledata);
        }
        let hex_tile = row.iter().map(|b| format!("{:02x} ", b)).collect::<String>();
        println!("{}",hex_tile);
    }
    buf.write_to_file("./screen.png");
    println!("wrote out to 'screen.png'");
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

fn pixel_row_to_string(row: &[u8]) -> String {
    let b1 = row[0];
    let b2 = row[1];
    let mut colors:Vec<u8> = vec![];
    for n in 0..7 {
        let v1 = common::get_bit(b1, n);
        let v2 = common::get_bit(b2, n);
        colors.push((v1 << 1) | v2);
    }
    return colors.iter().map(|c|format!("{}",c)).collect();
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
    return colors;
}

fn show_full_hardware_registers(term: &Term, ctx: &Ctx) -> Result<()>{
    let reg = Style::new().bg(Color::Cyan).red().bold();
    term.write_line(&format!("registers are {} ",reg.apply_to("cool")));
    term.write_line(&reg.apply_to(&format!("LY   {}",ctx.mmu.hardware.LY)).to_string())?;
    term.write_line(&format!("LYC  {}",ctx.mmu.hardware.LYC))?;
    term.write_line(&format!("SCX  {}",ctx.mmu.hardware.SCX))?;
    term.write_line(&format!("SCY  {}",ctx.mmu.hardware.SCY))?;
    term.write_line(&format!("LCDC {:8b}",ctx.mmu.hardware.LCDC))?;
    Ok(())
}

fn lookup_opcode_info(op: Instr) -> String {
    match op {
        Instr::Load(Load::Load_r_u8(r)) => format!("LD {},n -- Load register from immediate u8",r),
        Instr::Load(Load::Load_high_r_u8(r)) => format!("LDH {},(n) -- Load High: put contents of 0xFF00 + u8 into register {}",r,r),
        Instr::Load(Load::Load_high_u8_r(r)) => format!("LDH (n),{} -- Load High at u8 address with contents of {}",r,r),
        Instr::Load(Load::Load_high_r_r(off, src)) => format!("LD (FF00 + {},{}  -- Load High: put contents of register {} into memory of 0xFF00 + {} ",off,src,src,off),
        Instr::Load(Load::Load_R2_u16(rr)) => format!("LD {} u16 -- Load immediate u16 into register {}",rr,rr),
        Instr::Load(Load::Load_R_addr_R2(r,rr)) => format!("LD {}, ({}) -- load data pointed to by {} into {}",r,rr,rr,r),
        Instr::Load(Load::Load_addr_R2_A(rr,r)) => format!("LD ({}),{} -- load contents of {} into memory pointed to by {}",rr,r,r,rr),
        Instr::Load(Load::Load_addr_R2_A_inc(rr)) => format!("LD ({}+), A -- load contents of A into memory pointed to by {}, then increment {}",rr,rr,rr),
        Instr::Load(Load::Load_addr_R2_A_dec(rr)) => format!("LD ({}-), A -- load contents of A into memory pointed to by {}, then decrement {}",rr,rr,rr),
        Instr::Load(Load::Load_A_addr_R2_inc(rr)) => format!("LD A (HL+) -- load contents of memory pointed to by {} into A, then increment {}",rr,rr),
        Instr::Load(Load::Load_r_r(dst,src)) => format!("LD {},{} -- copy {} to {}",dst,src,src,dst),
        Instr::Load(Load::Load_addr_u16_A()) => format!("LD (nn),A -- load A into memory at address from immediate u16"),
        Instr::Load(Load::Load_addr_u16_R2(rr)) => format!("LD (nn),{} -- load {} into memory at address from immediate u16",rr,rr),


        Instr::Jump(Jump::JumpAbsolute_u16()) => format!("JP nn -- Jump unconditionally to absolute address"),
        Instr::Jump(Jump::JumpRelative_i8()) => format!("JR e --   Jump relative to signed offset"),
        Instr::Jump(Jump::JumpRelative_cond_carry_i8()) => format!("JR C,e -- Jump relative if Carry Flag set"),
        Instr::Jump(Jump::JumpRelative_cond_notcarry_i8()) => format!("JR NC,e -- Jump relative if not Carry flag set"),
        Instr::Jump(Jump::JumpRelative_cond_zero_i8()) => format!("JR Z,e -- Jump relative if Zero flag set"),
        Instr::Jump(Jump::JumpRelative_cond_notzero_i8()) => format!("JR NZ,e -- Jump relative if not Zero flag set"),
        Instr::Compare(Compare::CP_A_n()) => format!("CP A,n  -- Compare A with u8 n. sets flags"),
        Instr::Compare(Compare::CP_A_r(r)) => format!("CP A,{} -- Compare A with {}. sets flags",r,r),

        Instr::Math(Math::XOR_A_r(r)) => format!("XOR A, {}  -- Xor A with {}, store in A", r, r),
        Instr::Math(Math::OR_A_r(r))  => format!("OR A, {}   -- OR A with {}, store in A",r,r),
        Instr::Math(Math::AND_A_r(r)) => format!("AND A, {}  -- AND A with {}, store in A",r,r),
        Instr::Math(Math::ADD_R_u8(r)) => format!("ADD {} u8 -- add immediate u8 to register {}",r,r),
        Instr::Math(Math::ADD_R_R(dst,src)) => format!("ADD {} {} -- add {} to {}, store result in {}",dst,src,src,dst,dst),
        Instr::Math(Math::SUB_R_R(dst, src)) => format!("SUB {} {} -- subtract {} from {}, store result in {}",dst,src,src,dst,dst),

        Instr::Math(Math::Inc_rr(rr)) => format!("INC {} -- Increment register {}",rr,rr),
        Instr::Math(Math::Dec_rr(rr)) => format!("DEC {} -- Decrement register {}",rr,rr),
        Instr::Math(Math::Inc_r(r)) => format!("INC {} -- Increment register {}. sets flags",r,r),
        Instr::Math(Math::Dec_r(r)) => format!("DEC {} -- Decrement register {}. sets flags",r,r),

        Instr::Math(Math::BIT(bit, r)) => format!("BIT {:0x}, {}",bit,r),
        Instr::Math(Math::BITR2(bit, r)) => format!("BIT {:0x}, ({})",bit,r),
        Instr::Math(Math::RLC(r)) => format!("RLC {} --- rotate register {} .old bit 7 to carry flag. sets flags", r, r),
        Instr::Math(Math::RL(r)) => format!("RL {} -- Rotate {} left through Carry flag. sets flags",r,r),
        Instr::Math(Math::RRC(r)) => format!("RRC {} -- Rotate {} right, Old bit 0 to carry flag. sets flags.",r,r),
        Instr::Math(Math::RR(r)) => format!("RR {} -- Rotate {} right through carry flag. sets flags",r,r),
        Instr::Math(Math::RLA()) => format!("RLA -- rotate A left through carry flag. Same as RL A"),
        Instr::Math(Math::RLCA()) => format!("RLCA -- rotate A left. same as RLC A "),
        Instr::Math(Math::RRA()) => format!("RRA -- Rotate A right. Same as RR A"),
        Instr::Math(Math::RRCA()) => format!("RRCA -- Rotate A right, Same as RRC A"),

        Instr::Special(Special::DisableInterrupts()) => format!("DI -- disable interrupts"),
        Instr::Special(Special::NOOP()) => format!("NOOP -- do nothing"),
        Instr::Special(Special::STOP()) => format!("STOP -- stop interrupts?"),
        Instr::Special(Special::CALL_u16()) => format!("CALL u16 -- save next addr to the stack, then jump to the specified address"),
        Instr::Special(Special::PUSH(rr)) => format!("PUSH {} -- push contents of register {} to the stack",rr,rr),
        Instr::Special(Special::POP(rr)) => format!("POP {} -- pop off stack, back to register {}",rr,rr),
        Instr::Special(Special::RET()) => format!("RET -- pop two bytes from the stack and jump to that address"),
        Instr::Special(Special::RETI()) => format!("RET -- pop two bytes from the stack and jump to that address, plus enable interrupts"),
        Instr::Special(Special::RST(h)) => format!("RST {:02x} -- put present address onto stack, jump to address {:02x}", h, h),
        Instr::Special(Special::RETZ()) => format!("RET Z  -- return of zflag is set"),
        Instr::Special(Special::HALT()) => format!("HALT -- completely stop the emulator"),
    }
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
