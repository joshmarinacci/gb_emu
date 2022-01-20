use std::io;
use console::Term;
use io::Result;
use serde_json::Value;
use crate::{A, execute, fetch_opcode_from_memory, MMU, RegisterName, Z80};
use crate::common::RomFile;
use crate::opcodes::{Compare, decode, Instr, Jump, Load, lookup_opcode, Math, Special, u8_as_i8};

struct Ctx {
    cpu:Z80,
    mmu:MMU,
    opcodes:Value,
    cart:RomFile,
}

impl Ctx {
    pub(crate) fn execute(&mut self, term: &mut Term) {
        // term.write_line(&format!("PC at {:04x}", self.cpu.r.pc));
        let (opcode, off) = fetch_opcode_from_memory(&mut self.cpu, &mut self.mmu);
        term.write_line(&format!("--PC at {:04x}  op {:0x}", self.cpu.r.pc, opcode));
        if let Some(instr) = lookup_opcode(opcode) {
            self.execute_instruction(&instr);
        } else {
            println!("unknown op code {:04x}",opcode);
            panic!("unknown op code");
        }
        self.mmu.update();
    }
    fn execute_instruction(&mut self, inst: &Instr) {
        println!("executing");
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
        println!("jumping to {:04x}",addr);
        self.cpu.r.pc = addr;
    }
}

impl Ctx {
    fn execute_compare_instructions(&mut self, comp:&Compare) {
        match comp {
            Compare::CP_A_n() => {
                let n = self.mmu.read8(self.cpu.r.pc + 1);
                // println!("comparing A:{:x} to n:{:x}",self.cpu.r.a,n);
                self.cpu.r.zero_flag = self.cpu.r.a == n;
                self.cpu.r.carry_flag = self.cpu.r.a < n;
                self.cpu.r.subtract_n_flag = true;
                self.inc_pc(2);
            }
            Compare::CP_A_r(r) => {
                todo!()
            }
        }
    }
    fn execute_math_instructions(&mut self, math: &Math) {
        match math {
            Math::XOR_A_r(r) => {
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) ^ self.cpu.r.get_u8reg(r));
                if self.cpu.r.get_u8reg(&A) == 0 { self.cpu.r.zero_flag = true; }
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = true;
                self.cpu.r.carry_flag = false;
                self.inc_pc(1);
            }
            Math::OR_A_r(r) => {
                self.inc_pc(1);
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) | self.cpu.r.get_u8reg(r));
                if self.cpu.r.get_u8reg(&A) == 0 { self.cpu.r.zero_flag = true; }
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = false;
                self.cpu.r.carry_flag = false;
            }
            Math::AND_A_r(r) => {
                self.inc_pc(1);
                self.cpu.r.set_u8reg(&A, self.cpu.r.get_u8reg(&A) & self.cpu.r.get_u8reg(r));
                if self.cpu.r.get_u8reg(&A) == 0 { self.cpu.r.zero_flag = true; }
                self.cpu.r.subtract_n_flag = false;
                self.cpu.r.half_flag = true;
                self.cpu.r.carry_flag = false;
            }
            Math::Inc_r(r) => {
                self.inc_pc(1);
                let mut val = self.cpu.r.get_u8reg(r);
                val += 1;
                self.cpu.r.set_u8reg(r, val);
                if val == 0 { self.cpu.r.zero_flag = true; }
                self.cpu.r.subtract_n_flag = false;
            }
            Math::Dec_r(r) => {
                self.inc_pc(1);
                let mut val = self.cpu.r.get_u8reg(r);
                val -= 1;
                self.cpu.r.set_u8reg(r, val);
                if val == 0 { self.cpu.r.zero_flag = true; }
                self.cpu.r.subtract_n_flag = true;
            }
            Math::Inc_rr(rr) => {
                self.inc_pc(1);
                let mut val = self.cpu.r.get_u16reg(rr);
                val += 1;
                self.cpu.r.set_u16reg(rr, val);
            }
            Math::Dec_rr(rr) => {
                self.inc_pc(1);
                let mut val = self.cpu.r.get_u16reg(rr);
                val -= 1;
                self.cpu.r.set_u16reg(rr, val);
            }
        }
    }
    fn execute_jump_instructions(&mut self, jump: &Jump) {
        match jump {
            Jump::JumpAbsolute_u16() => {
                // println!("jumping to absolute address");
                let addr = self.mmu.read16(self.cpu.r.pc + 1);
                // println!("jumping to address {:04x}",addr);
                self.set_pc(addr);
            },
            Jump::JumpRelative_cond_carry_u8() => {
                self.inc_pc(1);
                let e = self.mmu.read8(self.cpu.r.pc);
                self.inc_pc(1);
                // println!("carry flag is set to {}",self.cpu.r.carry_flag);
                if self.cpu.r.carry_flag {
                    let addr = (((self.cpu.r.pc) as i32) + (u8_as_i8(e) as i32));
                    // println!("jump address is {:04x}",(addr as u16));
                    self.set_pc(addr as u16);
                } else {}
            },
            Jump::JumpRelative_cond_notzero_u8() => {
                self.inc_pc(1);
                let e = self.mmu.read8(self.cpu.r.pc);
                self.inc_pc(1);
                let addr = (self.cpu.r.pc as i32) + (e as i8 as i32);
                if !self.cpu.r.zero_flag {
                    println!("jumping to {:04x}",addr);
                    self.set_pc(addr as u16);
                }
            },
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
                let e =self.mmu.read8(self.cpu.r.pc);
                let addr = (0xFF00 as u16) + (e as u16);
                let v = self.cpu.r.get_u8reg(r);
                println!("copying value x{:02x} from register {} to address ${:04x}", v, r, addr);
                self.mmu.write8(addr,v);
                self.inc_pc(1);
            },
            Load::Load_r_r(dst, src) => {
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(src);
                self.cpu.r.set_u8reg(dst,val);
            }

            Load::Load_R2_u16(rr) => {
                self.inc_pc(1);
                let val = self.mmu.read16(self.cpu.r.pc);
                self.cpu.r.set_u16reg(rr,val);
                self.inc_pc(2);
                println!("copied immediate value {:04x} into register {}",val,rr)
            }
            Load::Load_r_addr_R2(rr) => {
                // load to the 8bit register A, data from the address in the 16 bit register
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(rr);
                let val = self.mmu.read8(addr);
                self.cpu.r.set_u8reg(&A,val);
                println!("copied value {:02x} from address {:04x} determined from register {} into register A",val,addr,rr);
            }
            Load::Load_A_addr_R2_inc(rr) => {
                // load to the 8bit register A, data from the address in the 16 bit register, then increment that register
                self.inc_pc(1);
                let addr = self.cpu.r.get_u16reg(rr);
                let val = self.mmu.read8(addr);
                self.cpu.r.set_u8reg(&A,val);
                println!("copied value {:02x} from address {:04x} determined from register {} into register A",val,addr,rr);
                self.cpu.r.set_u16reg(rr,self.cpu.r.get_u16reg(rr)+1);
            },
            Load::Load_addr_R2_A_inc(rr) => {
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(&A);
                let addr = self.cpu.r.get_u16reg(rr);
                self.mmu.write8(addr,val);
                self.cpu.r.set_hl(self.cpu.r.get_hl()+1);
            }

            Load::Load_addr_R2_A(rr) => {
                //copy contents of A to memory pointed to by RR
                self.inc_pc(1);
                let val = self.cpu.r.get_u8reg(&A);
                let addr = self.cpu.r.get_u16reg(rr);
                self.mmu.write8(addr,val);
            }
        }
    }
    fn execute_special_instructions(&mut self, special: &Special) {
        match special {
            Special::DisableInterrupts() => {
                println!("pretending to disable iterrupts");
                self.inc_pc(1);
            }
            Special::NOOP() => {
                self.inc_pc(1);
            }
            Special::STOP() => {
                self.inc_pc(1);
                println!("stopping interrupts");
            }
        }
    }
}

pub fn start_debugger(cpu: Z80, mmu: MMU, opcodes: Value, cart: RomFile) -> Result<()> {
    let mut ctx = Ctx { cpu, mmu, opcodes, cart};
    let mut term = Term::stdout();

    loop {
        term.clear_screen()?;
        term.write_line(&format!("executing rom {}", ctx.cart.path))?;
        term.write_line(&format!("op count {}",&ctx.cpu.ops.ops.len()))?;
        term.write_line(&format!("========="))?;

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
            term.write_line(&regs_u8)?;
            let regs_u16 = format!("BC:{:04x}  DE:{:04x}  HL:{:04x}",
                                   ctx.cpu.r.get_bc(),
                                   ctx.cpu.r.get_de(),
                                   ctx.cpu.r.get_hl(),
            );
            term.write_line(&regs_u16)?;
            term.write_line(&format!(" flags Z:{}   N:{}  C:{}",
                                     ctx.cpu.r.zero_flag,
                                     ctx.cpu.r.subtract_n_flag,
                                     ctx.cpu.r.carry_flag))?;
            term.write_line(&format!(" Screen: LY {:02x}",ctx.mmu.hardware.ly))?;
        }

        // print info about the next opcode
        let (opcode, instr_len) = fetch_opcode(&mut ctx.cpu, &mut ctx.mmu);
        let op = lookup_opcode(opcode);
        if let Some(ld) = op {
            term.write_line(&format!("${:04x} : {:02x}: {}",
                                     ctx.cpu.r.pc,
                                     opcode,
                                     lookup_opcode_info(ld)))?;
        } else {
            panic!("unknown op code")
        }


        term.write_line(&format!("next: j  back: k"))?;
        let ch = term.read_char()?;
        if ch == 'J' {
            term.write_line(&format!("doing 16 instructions"))?;
            for n in 0..16 {
                ctx.execute(&mut term);
            }
        }
        if ch == 'j' {
            ctx.execute(&mut term)
        }
    }


/*
- [ ] prints current instruction, previous 3, next three
- [ ] prints PC address, current op, abbr of the current op, analyzed version of the op (longer name, parsed args, etc), will it actually jump, etc.
- [ ] prints current status of registers
- [ ] prints current status of hardware: scroll Y, LCD on or off,
- [ ] step forward
- [ ] step backware (needs undo ability, somehow)
- [ ] jump forward until this loop exits. needs to know it’s in a loop somehow. so execute until this jump condition becomes true or false?
- [ ] how to read keyboard input without enter key
- [ ] need a curses lib for drawing the ui?
 */

    Ok(())
}

fn lookup_opcode_info(op: Instr) -> String {
    match op {
        Instr::Load(Load::Load_r_u8(r)) => format!("LD {},n -- Load register from immediate u8",r),
        Instr::Load(Load::Load_high_r_u8(r)) => format!("LDH {},(n) -- Load High: put contents of 0xFF00 + u8 into register {}",r,r),
        Instr::Load(Load::Load_high_u8_r(r)) => format!("LDH (n),{} -- Load High at u8 address with contents of {}",r,r),
        Instr::Load(Load::Load_R2_u16(rr)) => format!("LD {} u16 -- Load immediate u16 into register {}",rr,rr),
        Instr::Load(Load::Load_r_addr_R2(rr)) => format!("LD A, ({}) -- load data pointed to by {} into A",rr,rr),
        Instr::Load(Load::Load_addr_R2_A(rr)) => format!("LD (rr),A -- load contents of A into memory pointed to by {}",rr),
        Instr::Load(Load::Load_addr_R2_A_inc(rr)) => format!("LD ({}+), A -- load contents of A into memory pointed to by {}, then increment {}",rr,rr,rr),
        Instr::Load(Load::Load_A_addr_R2_inc(rr)) => format!("LD A (HL+) -- load contents of memory pointed to by {} into A, then increment {}",rr,rr),
        // Load (HL+), A, copy contents of A into memory at HL, then INC HL


        Instr::Jump(Jump::JumpAbsolute_u16()) => format!("JP nn -- Jump unconditionally to absolute address"),
        Instr::Jump(Jump::JumpRelative_cond_carry_u8()) => format!("JR cc,e -- Jump relative if Carry Flag set"),
        Instr::Jump(Jump::JumpRelative_cond_notzero_u8()) => format!("JR NZ,e -- Jump relative if Not Zero flag set"),
        Instr::Compare(Compare::CP_A_n()) => format!("CP A,n  -- Compare A with u8 n. sets flags"),
        Instr::Compare(Compare::CP_A_r(r)) => format!("CP A,{} -- Compare A with {}. sets flags",r,r),
        Instr::Math(Math::XOR_A_r(r)) => format!("XOR A, {}  -- Xor A with {}, store in A", r, r),
        Instr::Math(Math::OR_A_r(r))  => format!("OR A, {}   -- OR A with {}, store in A",r,r),
        Instr::Math(Math::AND_A_r(r)) => format!("AND A, {}  -- AND A with {}, store in A",r,r),

        Instr::Math(Math::Inc_rr(rr)) => format!("INC {} -- Increment register {}. sets flags",rr,rr),
        Instr::Math(Math::Dec_rr(rr)) => format!("DEC {} -- Decrement register {}. sets flags",rr,rr),
        Instr::Math(Math::Inc_r(r)) => format!("INC {} -- Increment register {}. sets flags",r,r),
        Instr::Math(Math::Dec_r(r)) => format!("DEC {} -- Decrement register {}. sets flags",r,r),
        Instr::Load(Load::Load_r_r(dst,src)) => format!("LD {},{} -- copy {} to {}",dst,src,src,dst),

        Instr::Special(Special::DisableInterrupts()) => format!("DI -- disable interrupts"),
        Instr::Special(Special::NOOP()) => format!("NOOP -- do nothing"),
        Instr::Special(Special::STOP()) => format!("STOP -- stop interrupts?"),
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
