use std::io;
use console::Term;
use io::Result;
use serde_json::Value;
use crate::{execute, fetch_opcode_from_memory, MMU, Z80};
use crate::common::RomFile;
use crate::opcodes::{Compare, decode, Instr, Jump, Load, lookup_opcode, Math, Special};

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
        let (off,size_of_inst) = decode(opcode, off, &mut self.cpu, &mut self.mmu, &self.opcodes);
        // println!("off is {}",off);
        let (v2, went_over) = self.cpu.r.pc.overflowing_add(off as u16);
        if went_over {
            self.mmu.print_cram();
            panic!("PC overflowed memory");
        }
        self.cpu.r.pc = v2;
        self.mmu.update();
    }
}

pub fn start_debugger(cpu: Z80, mmu: MMU, opcodes: Value, cart: RomFile) -> Result<()> {
    let mut ctx = Ctx { cpu, mmu, opcodes, cart};
    let mut term = Term::stdout();

    loop {
        // term.clear_screen()?;
        term.write_line(&format!("executing rom {}", ctx.cart.path))?;
        term.write_line(&format!("op count {}",&ctx.cpu.ops.ops.len()))?;
        term.write_line(&format!("========="))?;

        // print the current memory
        let start = ctx.cpu.r.pc;
        let back:i32 = 2;
        for n in 0..5 {
            let iv = (start as i32) + n - back;
            // println!("n is {} {}",n,iv);
            if iv < 0 { continue; }
            let addr = iv as u16;
            // println!("address is {:04x}",addr);
            let prefix = if(addr == start) { " *"} else {"  "};
            let data = ctx.mmu.read8(addr);
            term.write_line(&format!("{} {:04x}  {:02x}",prefix,addr,data));
        }

        {
            // print the registers
            term.write_line(&format!("PC={:04x}", ctx.cpu.r.pc))?;
            term.write_line(&format!(" A={:02x}", ctx.cpu.r.a))?;
            term.write_line(&format!(" B={:02x}", ctx.cpu.r.b))?;
            term.write_line(&format!(" C={:02x}", ctx.cpu.r.c))?;
            term.write_line(&format!(" D={:02x}", ctx.cpu.r.d))?;
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
            term.write_line(&format!("${:04x} : {:0x}: {}",
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
        Instr::Special(Special::DisableInterrupts()) => format!("DI -- disable interrupts"),
        Instr::Load(Load::Load_high_r_u8(r)) => format!("LDH {},(n) -- Load High with {} + immediate u8",r,r),
        Instr::Jump(Jump::JumpAbsolute_u16()) => format!("JP nn -- Jump unconditionally to absolute address"),
        Instr::Jump(Jump::JumpRelative_cond_carry_u8()) => format!("JR cc,e -- Jump relative if Carry Flag set"),
        Instr::Compare(Compare::CP_A_n()) => format!("CP A,n  -- Compare A with u8 n. sets flags"),
        Instr::Compare(Compare::CP_A_r(r)) => format!("CP A,{} -- Compare A with {}. sets flags",r,r),
        Instr::Math(Math::Xor_A_r(r)) => format!("Xor A, {} -- Xor A with {}, store in A",r,r),
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
