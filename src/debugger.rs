use std::io;
use console::Term;
use io::Result;
use serde_json::Value;
use crate::{execute, fetch_opcode_from_memory, MMU, Z80};
use crate::common::RomFile;
use crate::opcodes::decode;

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
        let (opcode, instr_len) = fetch_opcode(&mut ctx.cpu, &mut ctx.mmu);
        term.write_line(&format!("PC ${:04x} op {:0x}", ctx.cpu.r.pc, opcode))?;

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

        term.write_line(&format!("next: j  back: k"))?;
        let ch = term.read_char()?;
        if ch == 'j' {
            ctx.cpu.r.pc += 1;
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
