use std::fmt::{Display, Formatter};
use crate::cpu::Op;
use crate::{OpList, Z80};

pub fn setup_op_codes() -> OpList {
    let mut ol = OpList::init();

    //NO-OP
    ol.add(0x00_00,"NOOP",1,1, |cpu,mmu|());

    // Load immediate into 8 bit register
    // ol.add(0x00_06,"LD B, d8",2,8,|cpu,mmu|cpu.r.b = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_16,"LD D, d8",2,8,|cpu,mmu|cpu.r.d = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_26,"LD H, d8",2,8,|cpu,mmu|cpu.r.h = mmu.read8(cpu.r.pc+1));
    //
    ol.add(0x000e,"LD C, d8",2,8,|cpu,mmu|cpu.r.c = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_1e,"LD E, d8",2,8,|cpu,mmu|cpu.r.e = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_2e,"LD L, d8",2,8,|cpu,mmu|cpu.r.l = mmu.read8(cpu.r.pc+1));
    // ol.add(0x00_3e,"LD A, d8",2,8,|cpu,mmu|cpu.r.a = mmu.read8(cpu.r.pc+1));

    //16bit immediate loads
    // ol.add(0x00_01,"LD BC d16", 3, 12, |cpu,mmu| {cpu.r.set_bc(mmu.read16(cpu.r.pc+1));});
    ol.add(0x0011,"LD DE d16", 3, 12, |cpu,mmu| {cpu.r.set_de(mmu.read16(cpu.r.pc+1));});
    ol.add(0x0021,"LD HL d16", 3, 12, |cpu,mmu| {cpu.r.set_hl(mmu.read16(cpu.r.pc+1));});
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
    ol.add(0x00e0, "LDH(n), A",2,12, |cpu,mmu| {
        let v = mmu.read8(cpu.r.pc+1);
        println!("n is {:x}",v);
        let addr = (0xFF00 as u16) + (v as u16);
        println!("calculated address {:04x}",addr);
        mmu.write8(addr,cpu.r.a);
    });

    // put the memory addres 0xFF00 + n into A
    ol.add(0x00F0,"LDH A,(n)",2,3,|cpu,mmu|{
        let n = mmu.read8(cpu.r.pc+1);
        let addr = (0xFF00 as u16) + (n as u16);
        cpu.r.a = mmu.read8(addr);
        println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    });

    // put the immediate value into BC
    ol.add(0x0001,"LD BC, u16",3,3,|cpu,mmu|{
        let nn = mmu.read16(cpu.r.pc+1);
        cpu.r.set_bc(nn);
        // let n = mmu.read8(cpu.r.pc+1);
        // let addr = (0xFF00 as u16) + (n as u16);
        // cpu.r.a = mmu.read8(addr);
        // println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    });

    //
    ol.add(0x001a,"LD A,(DE)",1,2,|cpu,mmu|{
        cpu.r.a = mmu.read8(cpu.r.get_de())
        // let n = mmu.read8(cpu.r.pc+1);
        // let addr = (0xFF00 as u16) + (n as u16);
        // cpu.r.a = mmu.read8(addr);
        // println!("assigned content of mem:{:x} value {:x}, to A",addr,cpu.r.a);
    });

    //MATH
    ol.add(0x00AF,"XOR A,A",1,1,|cpu,mmu|{
        cpu.r.a = cpu.r.a ^ cpu.r.a
    });


    // // JUMPs
    ol.add(0x00C3, "JP u16", 0,16,|cpu,mmu|{
        let addr = mmu.read16(cpu.r.pc+1);
        println!("jumping to address {:04x}",addr);
        // subtract off an extra two because the CPU will automatically move us forward three
        cpu.r.pc = addr;
        // cpu.r.pc = (addr -3) as u16;
    });
    // JUMP if not zero to the address
    ol.add(0x0020,"JR NZ r8",2,12,|cpu,mmu| {
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
    ol.add(0x0038, "JR cc,e Carry Flag",0,4,|cpu,mmu|{
        let e = mmu.read8(cpu.r.pc+1);
        cpu.r.pc += 2;
        println!("immediate value is 0x{:x} {} {}",e, (e as i8), u8_as_i8(e));
        println!("carry flag is set to {}",cpu.r.carry_flag);
        if cpu.r.carry_flag {
            // let v2 = (e as i8);
            // println!("signed {}",v2);
            // let v3 = cpu.r.pc as i32;
            // println!("v3 is {}",v3);
            // let v4 = v3 + (v2 as i32);
            let addr = (((cpu.r.pc) as i32) + (u8_as_i8(e) as i32));
            cpu.r.pc =  addr as u16;
            println!("jumping to {:04x}", cpu.r.pc);
        }
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

    // // ====== INCREMENT Registers ==========
    // ol.add(0x003c,"INC A",1,1,|cpu,mmu|{
    //     println!("register a contains {:x}",cpu.r.a);
    //     let (v2, changed) = cpu.r.a.overflowing_add(1);
    //     println!("now it is {:x} flipped={}",v2, changed);
    //     cpu.r.a = v2;
    //     if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    //     cpu.r.subtract_n_flag = false;
    //     println!("zero flag is {}",cpu.r.zero_flag);
    //     // cpu.r.h_flag
    // });
    ol.ops.insert(0x003c, Op{
        //INC
        name: format!("INC {}",RegisterName::A),
        inst_len: 1,
        tim_len: 1,
        fun: (|cpu,mmu|{
            let r = RegisterName::A;
            let old_value = get_cpu_register_u8(cpu,&r);
            println!("register a contains {:x}",old_value);
            let (new_value, changed) = old_value.overflowing_add(1);
            println!("now it is {:x} flipped={}", new_value, changed);
            set_cpu_register_u8(cpu,&r,new_value);
            // cpu.r.a = new_value;
            if new_value == 0 { cpu.r.zero_flag = true; }
            cpu.r.subtract_n_flag = false;
            println!("zero flag is {}",cpu.r.zero_flag);
        })
    });
    ol.add(0x001c,"INC E",1,1,|cpu,mmu|{
        println!("register e contains {:x}",cpu.r.e);
        let (v2, changed) = cpu.r.e.overflowing_add(1);
        println!("now it is {:x} flipped={}",v2, changed);
        cpu.r.e = v2;
        if cpu.r.e == 0 { cpu.r.zero_flag = true; }
        cpu.r.subtract_n_flag = false;
        println!("zero flag is {}",cpu.r.zero_flag);
        // cpu.r.h_flag
    });
    ol.add(0x0014,"INC D",1,1,|cpu,mmu|{
        println!("register D contains {:x}",cpu.r.d);
        let (v2, changed) = cpu.r.d.overflowing_add(1);
        println!("now it is {:x} flipped={}",v2, changed);
        cpu.r.d = v2;
        if cpu.r.d == 0 { cpu.r.zero_flag = true; }
        cpu.r.subtract_n_flag = false;
        println!("zero flag is {}",cpu.r.zero_flag);
        // cpu.r.h_flag
    });

    ol.add(0x000d,"DEC C",1,1,|cpu,mmu|{
        println!("register c contains {:x}",cpu.r.c);
        let (v2, changed) = cpu.r.c.overflowing_sub(1);
        println!("now it is {:x} flipped={}",v2, changed);
        cpu.r.c = v2;
        if cpu.r.c == 0 { cpu.r.zero_flag = true; }
        cpu.r.subtract_n_flag = true;
        println!("zero flag is {}",cpu.r.zero_flag);
        // cpu.r.h_flag
    });

    // ol.add(0x001c,"DEC C",1,1,|cpu,mmu|{
    //     println!("register e contains {:x}",cpu.r.e);
    //     let (v2, changed) = cpu.r.e.overflowing_add(1);
    //     println!("now it is {:x} flipped={}",v2, changed);
    //     cpu.r.e = v2;
    //     if cpu.r.e == 0 { cpu.r.zero_flag = true; }
    //     cpu.r.subtract_n_flag = false;
    //     println!("zero flag is {}",cpu.r.zero_flag);
    //     // cpu.r.h_flag
    // });
    // ol.add(0x00_3c,"INC A",1,1,|cpu,mmu|{
    //     cpu.r.a = cpu.r.a + 1;
    //     if cpu.r.a == 0 { cpu.r.zero_flag = true; }
    //     cpu.r.subtract_n_flag = false;
    // });
    //


    // //Load A, (HL+)
    ol.add(0x002a,"LD A, (HL+)",1,2,|cpu,mmu|{
        cpu.r.a = mmu.read8(cpu.r.get_hl());
        cpu.r.set_hl(cpu.r.get_hl()+1);
    });


    ol.add(0x0012,"LD (DE),A",1,2,|cpu,mmu|{
        mmu.write8(cpu.r.get_de(),cpu.r.a);
    });

    ol.add(0x00f3,"DI", 1,1,|cpu,mmu|{
        println!("Disabling interrupts");
    });

    ol.add(0x00FE, "CP A,n",2,4,|cpu,mmu|{
        let n = mmu.read8(cpu.r.pc+1);
        println!("comparing A:{:x} to n:{:x}",cpu.r.a,n);
        // let v  = cpu.r.a - n;
        cpu.r.zero_flag = cpu.r.a == n;
        cpu.r.carry_flag = cpu.r.a < n;
        cpu.r.subtract_n_flag = true;
    });

    return ol;
}

pub fn LD(r1: RegisterName, r2: RegisterName, cpu:&mut Z80) -> Option<(usize, usize)> {
    let value = get_cpu_register_u8(cpu, &r2);
    set_cpu_register_u8(cpu, &r1, value);
    Some((1,1))
}

fn u8_as_i8(v: u8) -> i8 {
    return v as i8
}

pub enum RegisterName {
    A,B,C,D,E,H,L
}
impl Display for RegisterName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("blah")
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
