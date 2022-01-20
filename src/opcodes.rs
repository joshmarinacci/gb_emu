use std::fmt::{Display, Formatter};
use serde_json::Value;
use crate::cpu::Op;
use crate::{MMU, OpList, Z80};
use crate::opcodes::DoubleRegister::{BC, DE, HL, SP};
use crate::RegisterName::{A, B, C, D, E, H, L};

pub fn setup_op_codes() -> OpList {
    let mut ol = OpList::init();

    //NO-OP
    ol.add(0x00_00,"NOOP",1,1, |cpu,mmu|());

    //
    ol.add(0x000e,"LD C, d8",2,8,|cpu,mmu|cpu.r.c = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_1e,"LD E, d8",2,8,|cpu,mmu|cpu.r.e = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_2e,"LD L, d8",2,8,|cpu,mmu|cpu.r.l = mmu.read8(cpu.r.pc+1));
    ol.add(0x003e,"LD A, d8",2,8,|cpu,mmu|cpu.r.a = mmu.read8(cpu.r.pc+1));

    //16bit immediate loads
    // ol.add(0x00_01,"LD BC d16", 3, 12, |cpu,mmu| {cpu.r.set_bc(mmu.read16(cpu.r.pc+1));});
    ol.add(0x0011,"LD DE d16", 3, 12, |cpu,mmu| {cpu.r.set_de(mmu.read16(cpu.r.pc+1));});
    // ol.add(0x21,"LD HL d16", 3, 12, |cpu,mmu| {cpu.r.set_hl(mmu.read16(cpu.r.pc+1));});
    ol.add(0x0031,"LD SP d16", 3, 12, |cpu,mmu| {cpu.r.set_sp(mmu.read16(cpu.r.pc+1));});
    //
    //
    // // load 8bit register to 8bit register
    ol.add(0x0047, "LD B, A", 1,4,|cpu,mmu| cpu.r.b = cpu.r.a);
    // ol.add(0x00_5f, "LD E, A", 1,4,|cpu,mmu| cpu.r.e = cpu.r.a);
    // ol.add(0x00_67, "LD H, A", 1,4,|cpu,mmu| cpu.r.h = cpu.r.a);
    // ol.add(0x00_6f, "LD L, A", 1,4,|cpu,mmu| cpu.r.l = cpu.r.a);
    //
    // e0 => LDH (n), A => load contents of A into address of (0xFF00 + immediate value)
    // ol.add(0xe0, "LDH(n), A",2,12, |cpu,mmu| {
    //     let v = mmu.read8(cpu.r.pc+1);
    //     println!("n is {:x}",v);
    //     let addr = (0xFF00 as u16) + (v as u16);
    //     println!("calculated address {:04x}",addr);
    //     mmu.write8(addr,cpu.r.a);
    // });

    // put the memory addres 0xFF00 + n into A
    // ol.add(0xF0,"LDH A,(n)",2,3,|cpu,mmu|{
    //     println!("running LDH");
    //     let n = mmu.read8(cpu.r.pc+1);
    //     let addr = (0xFF00 as u16) + (n as u16);
    //     cpu.r.a = mmu.read8(addr);
    //     println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    // });

    // put the immediate value into BC
    ol.add(0x01,"LD BC, u16",3,3,|cpu,mmu|{
        let nn = mmu.read16(cpu.r.pc+1);
        cpu.r.set_bc(nn);
        // let n = mmu.read8(cpu.r.pc+1);
        // let addr = (0xFF00 as u16) + (n as u16);
        // cpu.r.a = mmu.read8(addr);
        // println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    });

    // put value pointed to by DE into A
    // ol.add(0x1a,"LD A,(DE)",1,2,|cpu,mmu|{
    //     cpu.r.a = mmu.read8(cpu.r.get_de())
    //     // let n = mmu.read8(cpu.r.pc+1);
    //     // let addr = (0xFF00 as u16) + (n as u16);
    //     // cpu.r.a = mmu.read8(addr);
    //     // println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    // });

    //MATH
    // ol.add(0xAF,"XOR A,A",1,1,|cpu,mmu|{
    //     cpu.r.a = cpu.r.a ^ cpu.r.a;
    //     if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    // });
    ol.add(0xA8,"XOR A,B",1,1,|cpu,mmu|{
        cpu.r.a = cpu.r.b ^ cpu.r.a;
        if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    });
    ol.add(0xA9,"XOR A,C",1,1,|cpu,mmu|{
        cpu.r.a = cpu.r.b ^ cpu.r.a;
        if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    });


    // // JUMPs
    ol.add(0xC3, "JP u16", 0,16,|cpu,mmu|{
        let addr = mmu.read16(cpu.r.pc+1);
        println!("jumping to address {:04x}",addr);
        // subtract off an extra two because the CPU will automatically move us forward three
        cpu.r.pc = addr;
        // cpu.r.pc = (addr -3) as u16;
    });
    // JUMP if not zero to the address
    ol.add(0x20,"JR NZ r8",2,12,|cpu,mmu| {
        //convert e to i8 then i32 so it will be interpreted as signed
        let e = mmu.read8(cpu.r.pc+1);
        let addr = ((cpu.r.pc+2) as i32) + (e as i8 as i32);
        if !cpu.r.zero_flag {
            //subtract off an extra two because the CPU will automatically move us forward two
            cpu.r.pc =  (addr - 2) as u16;
            println!("jumping to {:04x}", cpu.r.pc);
        }
    });
    //jump to relative immediate address if condition, signed immediate value.
    //check the carry flag
    // ol.add(0x38, "JR cc,e Carry Flag",0,4,|cpu,mmu|{
    //     let e = mmu.read8(cpu.r.pc+1);
    //     cpu.r.pc += 2;
    //     // println!("immediate value is 0x{:x} {} {}",e, (e as i8), u8_as_i8(e));
    //     println!("carry flag is set to {}",cpu.r.carry_flag);
    //     if cpu.r.carry_flag {
    //         // let v2 = (e as i8);
    //         // println!("signed {}",v2);
    //         // let v3 = cpu.r.pc as i32;
    //         // println!("v3 is {}",v3);
    //         // let v4 = v3 + (v2 as i32);
    //         let addr = (((cpu.r.pc) as i32) + (u8_as_i8(e) as i32));
    //         cpu.r.pc =  addr as u16;
    //         println!("jumping to {:04x}", cpu.r.pc);
    //     } else {
    //         println!("not jumping, just continuing");
    //     }
    // });
    //unconditional Jump to relative address specified by signed 8bit immediate value
    ol.add(0x18,"JR e",0,3,|cpu,mmu|{
        let e = mmu.read8(cpu.r.pc+1);
        let addr = ((cpu.r.pc as i32) + (u8_as_i8(e)) as i32);
        println!("jumping by {}",u8_as_i8(e));
        cpu.r.pc = addr as u16;
    });


    // //Returns
    ol.add(0x00C9, "RET",1,4,|cpu,mmu|{
        let dst = mmu.read16(cpu.r.sp);
        cpu.r.sp = cpu.r.sp + 2;
        println!("returning from a function at {:0x} to dst {:0x}", cpu.r.sp,dst);
        cpu.r.pc = dst;
    });
    // ol.add(0x00_c0, "RET NZ",1,2,|cpu,mmu|{
    //     if cpu.r.zero_flag {
    //         println!("returning");
    //     } else {
    //         println!("not returning");
    //     }
    // });
    //

    // //Load A, (HL+),  copy contents of memory at HL to A, then INC HL
    ol.add(0x2a,"LD A, (HL+)",1,2,|cpu,mmu|{
        cpu.r.a = mmu.read8(cpu.r.get_hl());
        cpu.r.set_hl(cpu.r.get_hl()+1);
    });
    // Load (HL+), A, copy contents of A into memory at HL, then INC HL
    // ol.add(0x22,"LD (HL+), A",1,2,|cpu,mmu|{
    //     mmu.write8(cpu.r.get_hl(),cpu.r.a);
    //     cpu.r.set_hl(cpu.r.get_hl()+1);
    // });

    // ol.add(0x13,"INC DE",1,2,|cpu,mmu|{
    //     println!("incrementing DE");
    //     cpu.r.set_de(cpu.r.get_de()+1);
    // });

    ol.add(0x0B,"DEC BC",1,1,|cpu,mmu|{
        println!("decrementing BC");
        let (v2, changed) = cpu.r.get_bc().overflowing_sub(1);
        cpu.r.set_bc(v2);
        if v2 == 0 { cpu.r.zero_flag = true; }
        cpu.r.subtract_n_flag = true;
    });


    ol.add(0xB1,"OR A, C",1,1,|cpu,mmu|{
        println!("ORING C with A");
        cpu.r.a = cpu.r.c | cpu.r.a;
        if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    });
    ol.add(0xB2,"OR A, D",1,1,|cpu,mmu|{
        println!("ORING C with D");
        cpu.r.a = cpu.r.d | cpu.r.a;
        if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    });

    ol.add(0xA7,"AND A",1,1,|cpu,mmu|{
        println!("ANDING A with itself");
        cpu.r.a = cpu.r.a | cpu.r.a;
    });
    ol.add(0x0012,"LD (DE),A",1,2,|cpu,mmu|{
        mmu.write8(cpu.r.get_de(),cpu.r.a);
    });

    ol.add(0x00f3,"DI", 1,1,|cpu,mmu|{
        println!("Disabling interrupts");
    });

    // Compare A with u8 n. sets flags based on teh comparison
    // ol.add(0xFE, "CP A,n",2,4,|cpu,mmu|{
    //     let n = mmu.read8(cpu.r.pc+1);
    //     println!("comparing A:{:x} to n:{:x}",cpu.r.a,n);
    //     // let v  = cpu.r.a - n;
    //     cpu.r.zero_flag = cpu.r.a == n;
    //     cpu.r.carry_flag = cpu.r.a < n;
    //     cpu.r.subtract_n_flag = true;
    // });

    return ol;
}

pub fn LD(r1: RegisterName, r2: RegisterName, cpu:&mut Z80) -> Option<(usize, usize)> {
    let value = get_cpu_register_u8(cpu, &r2);
    set_cpu_register_u8(cpu, &r1, value);
    println!("LDing {:?} {:?}",r1.to_string(),r2.to_string());
    Some((1,1))
}
pub fn LD_r_u8(reg: RegisterName, cpu:&mut Z80, mmu:&mut MMU) -> Option<(usize, usize)> {
    set_cpu_register_u8(cpu, &reg, mmu.read8(cpu.r.pc+1));
    Some((2,8))
}
pub fn INC(reg:RegisterName, cpu:&mut Z80) -> Option<(usize, usize)> {
    let old_value = get_cpu_register_u8(cpu,&reg);
    // println!("register a contains {:x}",old_value);
    let (new_value, changed) = old_value.overflowing_add(1);
    //         println!("now it is {:x} flipped={}", new_value, changed);
    set_cpu_register_u8(cpu,&reg,new_value);
    //         // cpu.r.a = new_value;
    if new_value == 0 { cpu.r.zero_flag = true; }
    cpu.r.subtract_n_flag = false;
    //         println!("zero flag is {}",cpu.r.zero_flag);
    Some((1,1))
}

pub fn DEC(reg:RegisterName, cpu:&mut Z80) -> Option<(usize, usize)> {
    // println!("register c contains {:x}",get_cpu_register_u8(cpu,&reg));
    let (v2, changed) = get_cpu_register_u8(cpu,&reg).overflowing_sub(1);
    // println!("now it is {:x} flipped={}",v2, changed);
    set_cpu_register_u8(cpu,&reg,v2);
    // cpu.r.c = v2;
    if v2 == 0 { cpu.r.zero_flag = true; }
    cpu.r.subtract_n_flag = true;
    // println!("zero flag is {}",cpu.r.zero_flag);
    Some((1,1))
}


pub fn u8_as_i8(v: u8) -> i8 {
    return v as i8
}

pub enum RegisterName {
    A,B,C,D,E,H,L
}
pub enum DoubleRegister {
    BC,DE,HL, SP,
}
pub enum Special {
    DisableInterrupts()
}
pub enum Load {
    Load_r_u8(RegisterName),
    Load_r_r(RegisterName,RegisterName),
    Load_high_r_u8(RegisterName),
    Load_high_u8_r(RegisterName),
    Load_R2_u16(DoubleRegister),
    Load_r_addr_R2(DoubleRegister),
    Load_addr_R2_A_inc(DoubleRegister),  // Load (HL+), A, copy contents of A into memory at HL, then INC HL
}
pub enum Jump {
    JumpAbsolute_u16(),
    JumpRelative_cond_carry_u8(),
}
pub enum Compare {
    CP_A_r(RegisterName),
    CP_A_n()
}
pub enum Math {
    Xor_A_r(RegisterName),
    OR_A_r(RegisterName),
    AND_A_r(RegisterName),
    Inc_r(RegisterName),
    Inc_rr(DoubleRegister),
    Dec_r(RegisterName),
    Dec_rr(DoubleRegister),
}
pub enum Instr {
    Load(Load),
    Special(Special),
    Jump(Jump),
    Compare(Compare),
    Math(Math),
}

impl Display for RegisterName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            A => "A",
            B => "B",
            C => "C",
            D => "D",
            E => "E",
            H => "H",
            L => "L",
        })
    }
}

impl Display for DoubleRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            DoubleRegister::BC => "BC",
            DoubleRegister::DE => "DE",
            DoubleRegister::HL => "HL",
            SP => "SP",
        })
    }
}

fn set_cpu_register_u8(cpu: &mut Z80, reg: &RegisterName, nv: u8) {
    match reg {
        RegisterName::A => cpu.r.a = nv,
        RegisterName::B => cpu.r.b = nv,
        RegisterName::C => cpu.r.c = nv,
        RegisterName::D => cpu.r.d = nv,
        RegisterName::E => cpu.r.e = nv,
        RegisterName::H => cpu.r.h = nv,
        RegisterName::L => cpu.r.l = nv,
    }
}

fn get_cpu_register_u8(cpu: &mut Z80, reg: &RegisterName) -> u8 {
    match reg {
        RegisterName::A => cpu.r.a,
        RegisterName::B => cpu.r.b,
        RegisterName::C => cpu.r.c,
        RegisterName::D => cpu.r.d,
        RegisterName::E => cpu.r.e,
        RegisterName::H => cpu.r.h,
        RegisterName::L => cpu.r.l,
    }
}

pub fn lookup_opcode(code:u16) -> Option<Instr> {
    return match code {

        // loads
        0x40 => Some(Instr::Load(Load::Load_r_r(B,B))),
        0x50 => Some(Instr::Load(Load::Load_r_r(D,B))),
        0x60 => Some(Instr::Load(Load::Load_r_r(H,B))),
        0x41 => Some(Instr::Load(Load::Load_r_r(B,C))),
        0x51 => Some(Instr::Load(Load::Load_r_r(D,C))),
        0x61 => Some(Instr::Load(Load::Load_r_r(H,C))),
        0x42 => Some(Instr::Load(Load::Load_r_r(B,D))),
        0x52 => Some(Instr::Load(Load::Load_r_r(D,D))),
        0x62 => Some(Instr::Load(Load::Load_r_r(H,D))),
        0x43 => Some(Instr::Load(Load::Load_r_r(B,E))),
        0x53 => Some(Instr::Load(Load::Load_r_r(D,E))),
        0x63 => Some(Instr::Load(Load::Load_r_r(H,E))),

        0x44 => Some(Instr::Load(Load::Load_r_r(B,H))),
        0x54 => Some(Instr::Load(Load::Load_r_r(D,H))),
        0x64 => Some(Instr::Load(Load::Load_r_r(H,H))),
        0x45 => Some(Instr::Load(Load::Load_r_r(B,L))),
        0x55 => Some(Instr::Load(Load::Load_r_r(D,L))),
        0x65 => Some(Instr::Load(Load::Load_r_r(H,L))),

        0x48 => Some(Instr::Load(Load::Load_r_r(C,B))),
        0x58 => Some(Instr::Load(Load::Load_r_r(E,B))),
        0x68 => Some(Instr::Load(Load::Load_r_r(L,B))),
        0x78 => Some(Instr::Load(Load::Load_r_r(A,B))),

        0x06 => Some(Instr::Load(Load::Load_r_u8(B))),
        0xE0 => Some(Instr::Load(Load::Load_high_u8_r(A))),
        0x01 => Some(Instr::Load(Load::Load_R2_u16(BC))),
        0x11 => Some(Instr::Load(Load::Load_R2_u16(DE))),
        0x21 => Some(Instr::Load(Load::Load_R2_u16(HL))),

        0x1a => Some(Instr::Load(Load::Load_r_addr_R2(DE))),
        // put value pointed to by DE into A

        0x22 => Some(Instr::Load(Load::Load_addr_R2_A_inc(HL))),
        // Load (HL+), A, copy contents of A into memory at HL, then INC HL



        0xF3 => Some(Instr::Special(Special::DisableInterrupts())),
        0xF0 => Some(Instr::Load(Load::Load_high_r_u8(A))),
        0xC3 => Some(Instr::Jump(Jump::JumpAbsolute_u16())),
        0xFE => Some(Instr::Compare(Compare::CP_A_n())),
        0x38 => Some(Instr::Jump(Jump::JumpRelative_cond_carry_u8())),

        0xAF => Some(Instr::Math(Math::Xor_A_r(A))),
        0xB0 => Some(Instr::Math(Math::OR_A_r(B))),
        0xB1 => Some(Instr::Math(Math::OR_A_r(C))),
        0xB2 => Some(Instr::Math(Math::OR_A_r(D))),
        0xB3 => Some(Instr::Math(Math::OR_A_r(E))),
        0xB4 => Some(Instr::Math(Math::OR_A_r(H))),
        0xB5 => Some(Instr::Math(Math::OR_A_r(L))),

        0xB7 => Some(Instr::Math(Math::OR_A_r(A))),


        //increments and decrements
        0x04 => Some(Instr::Math(Math::Inc_r(B))),
        0x14 => Some(Instr::Math(Math::Inc_r(D))),
        0x05 => Some(Instr::Math(Math::Dec_r(B))),
        0x15 => Some(Instr::Math(Math::Dec_r(D))),

        0x03 => Some(Instr::Math(Math::Inc_rr(BC))),
        0x13 => Some(Instr::Math(Math::Inc_rr(DE))),
        0x23 => Some(Instr::Math(Math::Inc_rr(HL))),
        0x33 => Some(Instr::Math(Math::Inc_rr(SP))),

        0x0B => Some(Instr::Math(Math::Dec_rr(BC))),
        0x1B => Some(Instr::Math(Math::Dec_rr(DE))),
        0x2B => Some(Instr::Math(Math::Dec_rr(HL))),
        0x3B => Some(Instr::Math(Math::Dec_rr(SP))),
        _ => {
            println!("WARNING. can't lookup opcode {:04x}",code);
            None
        }
    }
}
pub fn decode(code:u16, arg:u16, cpu:&mut Z80, mmu:&mut MMU, opcodes: &Value) -> (usize, usize) {
    println!("executing op {:02x}", code);
    let res:Option<(usize,usize)> = match code {
        // 8bit immediate value to register copy: LD r,n
        0x06 => LD_r_u8(B,cpu,mmu),
        0x0E => LD_r_u8(C,cpu,mmu),
        0x16 => LD_r_u8(D,cpu,mmu),
        0x1E => LD_r_u8(E,cpu,mmu),
        0x26 => LD_r_u8(H,cpu,mmu),
        0x2E => LD_r_u8(L,cpu,mmu),

        // 8bit register to register copies:  LD r,r
        0x78 => LD(A, B, cpu),
        0x79 => LD(A, C, cpu),
        0x7A => LD(A, D, cpu),
        0x7B => LD(A, E, cpu),
        0x7C => LD(A, H, cpu),
        0x7D => LD(A, L, cpu),
        0x7F => LD(A, A, cpu),

        0x40 => LD(B, B, cpu),
        0x41 => LD(B, C, cpu),
        0x42 => LD(B, D, cpu),
        0x43 => LD(B, E, cpu),
        0x44 => LD(B, H, cpu),
        0x45 => LD(B, L, cpu),

        0x48 => LD(C, B, cpu),
        0x49 => LD(C, C, cpu),
        0x4A => LD(C, D, cpu),
        0x4B => LD(C, E, cpu),
        0x4C => LD(C, H, cpu),
        0x4D => LD(C, L, cpu),
        0x4F => LD(C, A, cpu),

        0x50 => LD(D, B, cpu),
        0x51 => LD(D, C, cpu),
        0x52 => LD(D, D, cpu),
        0x53 => LD(D, E, cpu),
        0x54 => LD(D, H, cpu),
        0x55 => LD(D, L, cpu),

        0x58 => LD(E, B, cpu),
        0x59 => LD(E, C, cpu),
        0x5A => LD(E, D, cpu),
        0x5B => LD(E, E, cpu),
        0x5C => LD(E, H, cpu),
        0x5D => LD(E, L, cpu),

        0x60 => LD(H, B, cpu),
        0x61 => LD(H, C, cpu),
        0x62 => LD(H, D, cpu),
        0x63 => LD(H, E, cpu),
        0x64 => LD(H, H, cpu),
        0x65 => LD(H, L, cpu),

        0x68 => LD(L, B, cpu),
        0x69 => LD(L, C, cpu),
        0x6A => LD(L, D, cpu),
        0x6B => LD(L, E, cpu),
        0x6C => LD(L, H, cpu),
        0x6D => LD(L, L, cpu),

        // Register Increments
        0x3C => INC(A, cpu),
        0x04 => INC(B, cpu),
        0x0C => INC(C, cpu),
        0x14 => INC(D, cpu),
        0x1C => INC(E, cpu),
        0x24 => INC(H, cpu),
        0x2C => INC(L, cpu),

        //register decrements
        0x3d => DEC(A,cpu),
        0x05 => DEC(B,cpu),
        0x0d => DEC(C,cpu),
        0x15 => DEC(D,cpu),
        0x1d => DEC(E,cpu),
        0x25 => DEC(H,cpu),
        0x2d => DEC(L,cpu),

        _ => {None}
    };

    if let Some((a,b)) = res {
        return (a,b)
    }


    if let Some(op) = cpu.ops.ops.get(&code) {
        println!("PC {:04x}: OP {:04x}: {}", cpu.r.pc, code, op.name);
        let il = op.inst_len;
        let tl = op.tim_len;
        (op.fun)(cpu, mmu);
        return (il, tl);
    }
    let opcode_str =format!("0x{:2x}",code);
    println!("unknown opcode {}",opcode_str);
    let def = opcodes.as_object().unwrap()
        .get("unprefixed").unwrap().as_object().unwrap()
        .get(&opcode_str).unwrap();
    let j = serde_json::to_string_pretty(&def).unwrap();
    println!("def is {}",j);
    panic!("unknown op code {:04x}", code);
}
