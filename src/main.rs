use std::ops::BitXorAssign;

#[derive(Debug)]
struct Z80_registers {
    // 8 bit registers
    a:u8,
    b:u8,
    c:u8,
    d:u8,
    e:u8,

    // f:u8, // flag register
    zero_flag:bool,

    h:u8,
    l:u8,

    // 16 bit registers
    pc:u16, // program counter
    sp:u16, // stack pointer

    m:u8,
    t:u8,
    ime:u8,
}

impl Z80_registers {
    pub(crate) fn get_hl(&self) -> u16 {
        (self.l as u16) + ((self.h as u16) << 8)
    }
    pub(crate) fn set_hl(&mut self, val:u16) {
        self.h = (val >> 8) as u8;
        self.l = (0x00FF & val) as u8;
    }
}

#[derive(Debug)]
struct Z80 {
    clock_m: u16,
    clock_t: u16,
    r: Z80_registers,
    // halt:u8,
    // stop:u8,
    halt: bool,
}
impl Z80 {
    fn init() -> Z80 {
        Z80 {
            clock_m:0,
            clock_t:0,
            halt:false,
            // stop:0,
            r: Z80_registers {
                a: 0,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                zero_flag: false,
                h: 0,
                l: 0,
                pc: 0,
                sp: 0,
                m: 0,
                t: 0,
                ime: 0
            }
        }
    }
    fn reset(&mut self) {
        self.r.a = 0;
        self.r.b = 0;
        self.r.c = 0;
        self.r.d = 0;
        self.r.e = 0;

        self.r.zero_flag = false;
        self.r.h = 0;
        self.r.l = 0;

        self.r.sp = 0;
        self.r.pc = 0;

        self.r.m = 0;                       // set timing
        self.r.t = 0;
    }
}

/*
// Add E to A, leaving result in A (ADD A, E)
fn add_r_e (z80:&mut Z80) {
    z80.registers.a += z80.registers.e;   // perform addition
    z80.registers.f = 0;                  // clear the flags
    if (z80.registers.a & 255) == 0 {     // check for zero
        z80.registers.f |= 0x80;
    }
    if z80.registers.a > 255 {            // check for carry
        z80.registers.f |= 0x10;
    }
    z80.registers.a &= 255;               // mask to 8 bits
    z80.registers.m = 1;                      // time taken
    z80.registers.t = 4;
}

// Compare B to A, setting flags (CP A, B)
fn cp_r_b(z80:&mut Z80) {
    let mut i = z80.registers.a; //temp copy of a
    i -= z80.registers.b; // subtract B
    z80.registers.f |= 0x40;    // set the subtraction flag
    if (i & 255) == 0 {     //check for zero
        z80.registers.f |= 0x10;
    }
    if i < 0 {
        z80.registers.f |= 0x10;  // check fo runderflow
    }
    z80.registers.m = 1;
    z80.registers.t = 4;
}


// No-operation (NOP)
fn nop(z80:&mut Z80) {
    z80.registers.m = 1;
    z80.registers.t = 4;
}

// push registers B and C to the stack
fn push_b_c(z80:&mut Z80, mmu:&mut MMU) {
    z80.registers.sp -= 1;                     // drop through the stack
    mmu.wb(z80.registers.sp, z80.registers.b); // write B
    z80.registers.sp -= 1;                     // drop through the stack
    mmu.wb(z80.registers.sp, z80.registers.c); // write C
    z80.registers.m = 3;                       // set timing
    z80.registers.t = 12;
}

// Pop registers H and L off the stack (POP HL)
fn pop_hl(z80:&mut Z80, mmu:&mut MMU) {
    z80.registers.l = mmu.rb(z80.registers.sp); // read L
    z80.registers.sp += 1;                      // move back up the stack
    z80.registers.h = mmu.rb(z80.registers.sp); // read h
    z80.registers.sp += 1;                      // move back up the stack
    z80.registers.m = 3;                        // set timing
    z80.registers.t = 12;
}

// Read a byte from absolute location into A (LD A, addr)
fn lda_mm(z80:&mut Z80, mmu:&mut MMU) {
    let addr = mmu.rw(z80.registers.pc);       // get address from instruction
    z80.registers.pc += 2;                     // advance the program counter
    z80.registers.a = mmu.rb(addr);            //read from the address
    z80.registers.m = 4;                       // set timing
    z80.registers.t = 16;
}

impl Z80 {
    fn reset(&mut self) {
    self.registers.a = 0;
    self.registers.b = 0;
    self.registers.c = 0;
    self.registers.d = 0;

    z80.registers.e = 0;
    z80.registers.h = 0;
    z80.registers.l = 0;
    z80.registers.f = 0;

    z80.registers.sp = 0;
    z80.registers.pc = 0;

    z80.registers.m = 0;                       // set timing
    z80.registers.t = 0;
}

*/

pub struct MMU {
    inbios:bool,
    bios:Vec<u8>,
    data:Vec<u8>,
    // rom:Vec<u8>,
    // wram:Vec<u8>,
    // eram:Vec<u8>,
    // zram:Vec<u8>,
}

impl MMU {
    fn init() -> MMU {
        MMU {
            inbios: true,
            bios: Vec::from(BOOT_ROM),
            // wram: vec![u8; 8192],
            // eram: vec![u8; 8192],
            // zram: vec![u8; 127],
            // rom: vec![u8; 0],
            data: vec![0;66000],
        }
    }
    fn read8(&self, addr:u16) -> u8 {
        // println!("reading from bios at location {:04x}",addr);
        self.bios[addr as usize]
    }
    fn read16(&self, addr:u16) -> u16 {
        // println!("reading from bios at location {:04x}",addr);
        let b1 = self.read8(addr)   as u16;
        let b2 = self.read8(addr+1) as u16;
        return b1 + (b2 << 8);
    }
    fn write8(&mut self, addr:u16, val:u8) {
        self.data[addr as usize] = val;
    }
}


const BOOT_ROM:&[u8] = &[
    0x31, //0x0000 LD SP 16
    0xFE, 0xFF, //memory for the previous instruction
    0xAF, //0x0003 XOR A
    0x21, 0xFF, 0x9F, //0x0004 LD HL 16
    0x32, //0x0007 LD(HL-),A
    0xCB, 0x7C, //0x0008  prefix - BIT 7, H
    0x20, 0xFB, // 0x000A JRNZ.+0xfb, jump back to 0x0007
    0x21, 0x26, 0xFF, //0x000C  LD HL 16
    0x0E, 0x11, //0x000F  LD C 0x11
    0x3E, 0x80, //0x0011  LD A 0x80
    0x32, // 0x0013       LD(HL-),A
    0xE2, // 0x0014       LD(C), A
    0x0C, // 0x0015       INC C, increment the C register
    0x3E, 0xF3,  // 0x0016  LD A, 0xF3
    0xE2, 0x32,  // 0x0018 LD (C),A
    0x3E,  // 0x0019 LD (HL-) A
    0x77,  // 0x001a LD A
    0x77, 0x3E, 0xFC, 0xE0,
    0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
    0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06, 0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,
    0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99, 0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,
    0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64, 0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
    0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90, 0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,
    0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62, 0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,
    0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xF2, 0xF0, 0x42, 0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
    0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04, 0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,
    0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9, 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
    0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
    0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
    0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E, 0x3c, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x4C,
    0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13, 0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,
    0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xFB, 0x86, 0x20, 0xFE, 0x3E, 0x01, 0xE0, 0x50
];


fn fetch_opcode_from_memory(cpu:&mut Z80, mmu:&mut MMU) -> (u16,u16) {
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
// NOOP
// fn op_0000(arg:u16, cpu:&mut Z80, mmu:&mut MMU) -> (usize, usize) {
//     (4,1)
// }
// LD (HL),nn -> load to
// LD (HL),n -> load to memory at address in HL from arg n
// fn op_0036(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
//     mmu.write16(cpu.r.get_hl(),arg);
//     cpu.r.sp += 2; //increment PC
//     (3,12)
// }
fn decode(code:u16, arg:u16, cpu:&mut Z80, mmu:&mut MMU) -> (usize, usize) {
    println!("PC{:04x}: OP {:04x}: {}",cpu.r.pc,code, op_to_name(code));
    match code {
        // 0x0000 => op_0000(arg,cpu,mmu),
        0x00_31 => op_0031_LD_SP(arg,cpu,mmu),
        0x00_AF => op_00AF_XOR_A(arg,cpu,mmu),
        0x00_21 => op_0021_LD_HL(arg,cpu,mmu),
        0x00_32 => op_0032_LD_HLm_A(arg,cpu,mmu),
        0xCB_7C => op_CB76_BIT_7_H(arg,cpu,mmu),
        0x00_20 => op_0020_JR_NZ_r8(arg,cpu,mmu),
        0x00_0e => op_000e_LD_C(arg,cpu,mmu),
        0x00_3e => op_003e_LD_A(arg,cpu,mmu),
        0x00_e2 => op_00e2_LD_CA(arg,cpu,mmu),
        0x00_0c => op_000c_INC_C(arg,cpu,mmu),
        0x00_77 => op_0077_LD_HL_A(arg,cpu,mmu),
        // 0x0021 => op_0021(arg,cpu,mmu),
        _ => {
            panic!("unknown op code {:04x}:  {:?}",code, cpu);
        }
    }
}

fn op_to_name(op: u16) -> &'static str {
    match op {
        0x00_31 => "LD SP",
        0x00_AF => "XOR A",
        0x00_21 => "LD HL",
        0x00_32 => "LD (HL-) A",
        0xCB_7C => "BIT 7 H",
        0x00_20 => "JR NZ.+",
        0x00_0e => "LD C",
        0x00_3e => "LD A",
        0x00_e2 => "LD (C),A",
        0x00_0c => "INC C",
        0x00_77 => "LD (HL) A",
        _ => {
            panic!("unknown op code {:04x}:",op);
        }
    }
}


fn main() {
    let mut cpu = Z80::init();
    let mut mmu = MMU::init();
    cpu.reset();

    //by following along from
    // https://realboyemulator.wordpress.com/2013/01/03/a-look-at-the-game-boy-bootstrap-let-the-fun-begin/

    // starting at the beginning of the boot rom
    // 0x0000 -> 0x31, 0xFE, 0xFF,  -> LD SP, $0xFFFE
    assert_eq!(cpu.r.pc,0);
    execute(&mut cpu,&mut mmu);
    assert_eq!(cpu.r.sp,0xFFFE);
    assert_eq!(cpu.r.pc,3);

    // 0x0003 -> 0xAF -> XOR A
    execute(&mut cpu, &mut mmu);
    assert_eq!(cpu.r.a,0);

    // 0x0004 -> 0x21, 0xFF, 0x9F -> LD HL 16
    // load 0x9FFF into the HL register
    execute(&mut cpu, &mut mmu);
    assert_eq!(cpu.r.get_hl(),0x9FFF);

    // 0x0007 -> LD (HL-),A
    // load register A to the memory address pointed to by HL
    // meaning, write 0 to 0x9FFF
    // then decrement HL
    execute(&mut cpu, &mut mmu);
    assert_eq!(mmu.data[0x9FFF],0);
    assert_eq!(cpu.r.get_hl(),0x9FFE);

    // 0x0008 -> BIT 7, H
    // test MSB of H, set or clear ZERO flag
    execute(&mut cpu, &mut mmu);
    // zero flag should be cleared
    assert_eq!(cpu.r.zero_flag,false);

    //0x000A -> JRNZ .+0xfb
    //jump if not zero to the address 0xFB relative to the current address
    // 0xFB should be interpreted as signed
    // so jump to 0x000C - 0x0005 = 0x0007
    execute(&mut cpu, &mut mmu);
    assert_eq!(cpu.r.pc, 0x0007);

    //loop util PC equals 0x000C
    while cpu.r.pc != 0x000C {
        execute(&mut cpu, &mut mmu);
    }
    println!("done with the zeroing of vram");
    execute(&mut cpu, &mut mmu); //0x000C  LD HL $0xFF26  # load 0xFF26 into HL
    execute(&mut cpu, &mut mmu); //0x000F  LD C, $0x11    # load 0x11 into C
    execute(&mut cpu, &mut mmu); //0x0011  LD A, $0x80    # load 0x80 into A
    execute(&mut cpu, &mut mmu); //0x0013  LD (HL-), A # load A to address pointed to by HL and Dec HL
    execute(&mut cpu, &mut mmu); //0x0014  LD ($0xFF00+C), A # load A to address 0xFF00+C (0xFF11)
    execute(&mut cpu, &mut mmu); //0x0015 – INC C # increment C register
    execute(&mut cpu, &mut mmu); //0x0016 – LD A, $0xF3 # load 0xF3 to A
    execute(&mut cpu, &mut mmu); //0x0018 – LD ($0xFF00+C), A # load A to address 0xFF00+C (0xFF12)
    execute(&mut cpu, &mut mmu); //0x0019 – LD (HL-), A # load A to address pointed to by HL and Dec HL
    execute(&mut cpu, &mut mmu); //0x001A – LD A, $0x77 # load 0x77 to A
    execute(&mut cpu, &mut mmu); //0x001C – LD (HL), A # load A to address pointed to by HL


}
// INC C
fn op_000c_INC_C(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    // println!("INC C");
    cpu.r.c += 1;
    (1,8)
}

// LD C
fn op_000e_LD_C(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    // println!("LD C");
    cpu.r.c = mmu.read8(cpu.r.pc+1);
    (2,8)
}
// LD A
fn op_003e_LD_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.a = mmu.read8(cpu.r.pc+1);
    // println!("LD A. value is {:x}",cpu.r.a);
    (2,8)
}
// LD (C),A
fn op_00e2_LD_CA(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    let addr:u16 = ((0xFF00 as u16) + (cpu.r.c as u16)) as u16;
    mmu.write8(addr, cpu.r.a);
    (1,8)
}

// JUMP if not zero to the address
fn op_0020_JR_NZ_r8(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    if !cpu.r.zero_flag {
        let off = mmu.read8(cpu.r.pc+1);
        //convert off to i8 then i32 so it will be interpreted as signed
        let addr = ((cpu.r.pc+2) as i32) + (off as i8 as i32);
        //subtract off an extra two because the CPU will automatically move us forward two
        cpu.r.pc =  (addr - 2) as u16;
    }
    (2,12)
}


// LD SP 16 -> load next two bytes into the stack pointer
fn op_0031_LD_SP(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.sp = mmu.read16(cpu.r.pc+1); // load word at PC
    (3,//3 spots, instruction + next two bytes
     12 // timing
    )
}
// XOR A
fn op_00AF_XOR_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.a.bitxor_assign(cpu.r.a);
    (1,4)
}

// load next word into HL
fn op_0021_LD_HL(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.set_hl(mmu.read16(cpu.r.pc+1));
    (3,12)
}
// load register A into memory pointed at by HL, then decrement HL
fn op_0032_LD_HLm_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    mmu.write8(cpu.r.get_hl(),cpu.r.a);
    cpu.r.set_hl(cpu.r.get_hl()-1);
    (1,8)
}
// load register A into memory pointed at by HL, then decrement HL
fn op_0077_LD_HL_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    mmu.write8(cpu.r.get_hl(),cpu.r.a);
    (1,8)
}
// BIT 7,H
fn op_CB76_BIT_7_H(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.zero_flag = !((cpu.r.h & 0b1000_0000) > 0);
    (2,8)
}

fn execute(cpu: &mut Z80, mmu: &mut MMU) {
    // println!("PC at {:04x}",cpu.r.pc);
    let (opcode, off) = fetch_opcode_from_memory(cpu, mmu);
    // println!("op {:0x} arg {:0x}", opcode, off);
    let (off,size_of_inst) = decode(opcode, off, cpu, mmu);
    // println!("off is {}",off);
    cpu.r.pc = cpu.r.pc.wrapping_add(off as u16);
    // println!("inst size was {}",size_of_inst);
    // cpu.r.pc = cpu.r.pc.wrapping_add(size_of_inst as u16);
}

