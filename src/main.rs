use std::collections::HashMap;
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
    pub(crate) fn set_bc(&mut self, val:u16) {
        self.b = (val >> 8) as u8;
        self.c = (0x00FF & val) as u8;
    }
    pub(crate) fn set_hl(&mut self, val:u16) {
        self.h = (val >> 8) as u8;
        self.l = (0x00FF & val) as u8;
    }
    pub(crate) fn set_de(&mut self, val:u16) {
        self.d = (val >> 8) as u8;
        self.e = (0x00FF & val) as u8;
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
        if addr == 0xFF47 {
            println!("writing to special LCD register")
        }
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
    0x3E, //0x0019 LD (HL-) A
    0x77, 0x77, // 0x001A  LD A

    0x3E, 0xFC, // 0x001D LD A with 0xFC
    0xE0, 0x47, // 0x001F put 0xfc at 0xFF47
    0x11, 0x04, // 0x0021
    0x01, 0x21, 0x10,
    0x80,
    0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
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
    if cpu.r.pc >= 0x0007 && cpu.r.pc <= 0x00a {
        // println!("in the loop")
    } else {
        println!("PC{:04x}: OP {:04x}: {}", cpu.r.pc, code, op_to_name(code));
    }
    match code {
        // 0x0000 => op_0000(arg,cpu,mmu),
        0x00_01 => op_0001_LD_BC_d16(arg, cpu, mmu),
        0x00_11 => op_0011_LD_DE_d16(arg,cpu,mmu),
        0x00_21 => op_0021_LD_HL_d16(arg, cpu, mmu),
        0x00_31 => op_0031_LD_SP_d16(arg, cpu, mmu),

        0x00_AF => op_00AF_XOR_A(arg,cpu,mmu),

        0x00_32 => op_0032_LD_HLm_A(arg,cpu,mmu),
        0xCB_7C => op_CB76_BIT_7_H(arg,cpu,mmu),
        0x00_20 => op_0020_JR_NZ_r8(arg,cpu,mmu),
        0x00_0e => op_000e_LD_C(arg,cpu,mmu),
        0x00_3e => op_003e_LD_A(arg,cpu,mmu),
        0x00_e2 => op_00e2_LD_CA(arg,cpu,mmu),
        0x00_0c => op_000c_INC_C(arg,cpu,mmu),
        0x00_77 => op_0077_LD_HL_A(arg,cpu,mmu),
        0x00_e0 => op_00e0_LDH_a8_A(arg,cpu,mmu),
        0x00_1a => op_001a_LD_A_DE(arg,cpu,mmu),
        0x00_cd => op_00cd_call_a16(arg,cpu,mmu),
        0x00_13 => op_0013_inc_de(arg,cpu,mmu),
        0x00_7b => op_007b_LD_A_E(arg,cpu,mmu),
        0x00_fe => op_00fe_CP_d8(arg,cpu,mmu),
        0x00_06 => op_0006_LD_B_d8(arg,cpu,mmu),
        0x00_05 => op_0005(arg,cpu,mmu),
        0x00_22 => op_0022(arg,cpu,mmu),
        0x00_23 => op_0023(arg,cpu,mmu),
        0x00_ea => op_00ea(arg,cpu,mmu),
        0x00_3d => op_003d(arg,cpu,mmu),
        0x00_28 => op_0028(arg,cpu,mmu),
        0x00_0d => op_000d(arg,cpu,mmu),
        0x00_2e => op_002e(arg,cpu,mmu),
        0x00_18 => op_0018(arg,cpu,mmu),
        0x00_67 => op_0067(arg,cpu,mmu),
        0x00_57 => op_0057(arg,cpu,mmu),
        0x00_04 => op_0004(arg,cpu,mmu),
        0x00_1e => op_001e(arg,cpu,mmu),
        0x00_1d => op_001d(arg,cpu,mmu),
        0x00_f0 => op_00f0(arg,cpu,mmu),
        0x00_24 => op_0024(arg,cpu,mmu),
        0x00_7c => op_007c(arg,cpu,mmu),
        0x00_f2 => op_00f2(arg,cpu,mmu),

        0x00_41 => op_0041(arg,cpu,mmu),
        0x00_42 => op_0042(arg,cpu,mmu),

        0x00_90 => op_0090(arg,cpu,mmu),
        0x00_15 => op_0015(arg,cpu,mmu),
        0x00_16 => op_0016(arg,cpu,mmu),
        0x00_17 => op_0017(arg,cpu,mmu),
        0x00_4f => op_004f(arg,cpu,mmu),
        0x00_c5 => op_00c5(arg,cpu,mmu),
        0xcb_11 => op_cb11(arg,cpu,mmu),
        0x00_c1 => op_00c1(arg,cpu,mmu),
        0x00_c9 => op_00c9(arg,cpu,mmu),

        _ => {
            panic!("unknown op code {:04x}:  {:?}",code, cpu);
        }
    }
}

fn op_0028(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}

fn op_000d(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}

fn op_0023(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,8)
}

fn op_0022(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,8)
}

fn op_0013_inc_de(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,8)
}

fn op_00cd_call_a16(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (3,24)
}

fn op_001a_LD_A_DE(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,8)
}
fn op_007b_LD_A_E(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_00fe_CP_d8(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_0006_LD_B_d8(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_0005(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_00ea(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (3,16)
}
fn op_003d(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_002e(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_0018(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,12)
}
fn op_0067(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_0057(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_0004(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_001e(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_00f0(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,12)
}
fn op_001d(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_0024(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_007c(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_00f2(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}

// register to register loads
fn op_0041(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.b = cpu.r.c;
    (1,4)
}
fn op_0042(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.b = cpu.r.d;
    (1,4)
}
fn op_0043(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.b = cpu.r.e;
    (1,4)
}


fn op_0090(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_0015(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_0016(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_0017(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_004f(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,4)
}
fn op_00c5(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.sp = cpu.r.sp -1;
    mmu.write8(cpu.r.sp,cpu.r.b);
    cpu.r.sp = cpu.r.sp -1;
    mmu.write8(cpu.r.sp,cpu.r.c);
    (1,16)
}
fn op_cb11(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (2,8)
}
fn op_00c1(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,12)
}
fn op_00c9(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    (1,16)
}


fn op_to_name(op: u16) -> &'static str {
    match op {
        0x00_31 => "LD SP",
        0x00_AF => "XOR A",
        0x00_21 => "LD HL",
        0x00_32 => "LD (HL-) A",
        0xCB_7C => "BIT 7 H",
        0xCB_11 => "RL C",
        0x00_20 => "JR NZ.+",
        0x00_0e => "LD C",
        0x00_3e => "LD A",
        0x00_e2 => "LD (C),A",
        0x00_0c => "INC C",
        0x00_77 => "LD (HL) A",
        0x00_e0 => "LDH (a8),A",
        0x00_11 => "LD DE,d16",
        0x00_1a => "LD A,(DE)",
        0x00_cd => "CALL a16",
        0x00_13 => "INC DE",
        0x00_7b => "LD A,E",
        0x00_fe => "CP d8",
        0x00_06 => "LD B,d8",
        0x00_05 => "DEC B",
        0x00_22 => "LD (HL+),A",
        0x00_23 => "INC HL",
        0x00_ea => "LD (a16), A",
        0x00_3d => "DEC A",
        0x00_28 => "JR Z, r8",
        0x00_0d => "DEC C",
        0x00_2e => "LD L,d8",
        0x00_18 => "JR r8",
        0x00_67 => "LD H, A",
        0x00_57 => "LD D, A",
        0x00_04 => "INC B",
        0x00_1e => "LD E, d8",
        0x00_f0 => "LDH A,(a8)",
        0x00_1d => "DEC E",
        0x00_24 => "INC H",
        0x00_7c => "LD A,H",
        0x00_f2 => "LD A,(C)",

        0x00_40 => "LD B,B",
        0x00_41 => "LD B,C",
        0x00_42 => "LD B,D",
        0x00_43 => "LD B,E",

        0x00_90 => "SUB B",
        0x00_15 => "DEC D",
        0x00_16 => "LD D,d8",
        0x00_4f => "LD C,A",
        0x00_c5 => "PUSH BC",
        0x00_17 => "RLA",
        0x00_c1 => "POP BC",
        0x00_c9 => "RET",
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

    if true {
        return ()
    }
    println!("doing demanding part");
    execute(&mut cpu, &mut mmu); // 0x001D  LD A, $0xFC  # A represents the color number's mapping
    execute(&mut cpu, &mut mmu); // 0x001F  LD (0xFF00 + 0x47), A #initialize the palette
    execute(&mut cpu, &mut mmu); // 0x0021  LD DE 0x0104 # pointer to Nintendo logo
    execute(&mut cpu, &mut mmu); // 0x0024  LD HL 0x8010 # pointer to vram
    execute(&mut cpu, &mut mmu); // 0x0027  LD A, (DE) # load next byte from Nintendo logo
    execute(&mut cpu, &mut mmu); // 0x0028  CALL $0x0095 # decompress, scale and write pixels to VRAM (1)
    execute(&mut cpu, &mut mmu); // 0x002B  CALL $0x0096 # decompress, scale and write pixels to VRAM (2)
    execute(&mut cpu, &mut mmu); // 0x002E  INC DE # advance pointer
    execute(&mut cpu, &mut mmu); // 0x002F – LD A, E # …
    execute(&mut cpu, &mut mmu); // 0x0030 – CP $0x34 # compare accumulator to 0x34
    execute(&mut cpu, &mut mmu); // 0x0032 – JRNZ .+0xf3 # loop if not finished comparing
    execute(&mut cpu, &mut mmu); // 0x0034 – LD DE, $0x00D8 # …
    execute(&mut cpu, &mut mmu); // 0x0037 – LD B, $0x8 # …
    execute(&mut cpu, &mut mmu); // 0x0039 – LD A, (DE) # …
    execute(&mut cpu, &mut mmu); // 0x003A – INC DE # …
    execute(&mut cpu, &mut mmu); // 0x003B – LD (HL+), A # …
    execute(&mut cpu, &mut mmu); // 0x003C – INC HL # …
    execute(&mut cpu, &mut mmu); // 0x003D – DEC B # …
    execute(&mut cpu, &mut mmu); // 0x003E – JRNZ .+0xf9 # jump if not zero to 0x0039

    for n in 0..100 {
        execute(&mut cpu, &mut mmu);
    }


}

fn op_00e0_LDH_a8_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.a = mmu.read8(cpu.r.pc+1);
    (2,8)
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
    println!("LD A. value is {:x}",cpu.r.a);
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


// XOR A
fn op_00AF_XOR_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.a.bitxor_assign(cpu.r.a);
    (1,4)
}




//16bit register loads

// load next word into
fn op_0001_LD_BC_d16(arg:u16, cpu:&mut Z80, mmu:&mut MMU) -> (usize, usize) {
    cpu.r.set_bc(mmu.read16(cpu.r.pc+1));
    (3,12)
}
fn op_0011_LD_DE_d16(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.set_de(mmu.read16(cpu.r.pc+1));
    (3,12)
}
// load next word into HL
fn op_0021_LD_HL_d16(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.set_hl(mmu.read16(cpu.r.pc+1));
    (3,12)
}
// LD SP 16 -> load next two bytes into the stack pointer
fn op_0031_LD_SP_d16(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    cpu.r.sp = mmu.read16(cpu.r.pc+1); // load word at PC
    (3,
     12 // timing
    )
}








// load register A into memory pointed at by HL, then decrement HL
fn op_0032_LD_HLm_A(arg: u16, cpu: &mut Z80, mmu: &mut MMU) -> (usize, usize) {
    mmu.write8(cpu.r.get_hl(),cpu.r.a);
    cpu.r.set_hl(cpu.r.get_hl()-1);
    (1,8)
}
// load register A into memory pointed at by HL
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

