use crate::OpList;

pub fn setup_op_codes() -> OpList {
    let mut ol = OpList::init();

    // Load immediate into 8 bit register
    ol.add(0x00_06,"LD B, d8",2,8,|cpu,mmu|cpu.r.b = mmu.read8(cpu.r.pc+1));
    ol.add(0x00_16,"LD D, d8",2,8,|cpu,mmu|cpu.r.d = mmu.read8(cpu.r.pc+1));
    ol.add(0x00_26,"LD H, d8",2,8,|cpu,mmu|cpu.r.h = mmu.read8(cpu.r.pc+1));

    ol.add(0x00_0e,"LD C, d8",2,8,|cpu,mmu|cpu.r.c = mmu.read8(cpu.r.pc+1));
    ol.add(0x00_1e,"LD E, d8",2,8,|cpu,mmu|cpu.r.e = mmu.read8(cpu.r.pc+1));
    ol.add(0x00_2e,"LD L, d8",2,8,|cpu,mmu|cpu.r.l = mmu.read8(cpu.r.pc+1));
    ol.add(0x00_3e,"LD A, d8",2,8,|cpu,mmu|cpu.r.a = mmu.read8(cpu.r.pc+1));

    //16bit immediate loads
    ol.add(0x00_01,"LD BC d16", 3, 12, |cpu,mmu| {cpu.r.set_bc(mmu.read16(cpu.r.pc+1));});
    ol.add(0x00_11,"LD DE d16", 3, 12, |cpu,mmu| {cpu.r.set_de(mmu.read16(cpu.r.pc+1));});
    ol.add(0x00_21,"LD HL d16", 3, 12, |cpu,mmu| {cpu.r.set_hl(mmu.read16(cpu.r.pc+1));});
    ol.add(0x00_31,"LD SP d16", 3, 12, |cpu,mmu| {cpu.r.set_sp(mmu.read16(cpu.r.pc+1));});


    // load 8bit register to 8bit register
    ol.add(0x00_40, "LD B, B", 1,4,|cpu,mmu| cpu.r.b = cpu.r.b);
    ol.add(0x00_41, "LD B, C", 1,4,|cpu,mmu| cpu.r.b = cpu.r.c);
    ol.add(0x00_42, "LD B, D", 1,4,|cpu,mmu| cpu.r.b = cpu.r.d);
    ol.add(0x00_43, "LD B, E", 1,4,|cpu,mmu| cpu.r.b = cpu.r.e);
    ol.add(0x00_44, "LD B, H", 1,4,|cpu,mmu| cpu.r.b = cpu.r.h);
    ol.add(0x00_45, "LD B, L", 1,4,|cpu,mmu| cpu.r.b = cpu.r.l);
    ol.add(0x00_47, "LD B, A", 1,4,|cpu,mmu| cpu.r.b = cpu.r.a);
    ol.add(0x00_48, "LD C, B", 1,4,|cpu,mmu| cpu.r.c = cpu.r.b);
    ol.add(0x00_49, "LD C, C", 1,4,|cpu,mmu| cpu.r.c = cpu.r.c);
    ol.add(0x00_4a, "LD C, D", 1,4,|cpu,mmu| cpu.r.c = cpu.r.d);
    ol.add(0x00_4b, "LD C, E", 1,4,|cpu,mmu| cpu.r.c = cpu.r.e);
    ol.add(0x00_4c, "LD C, H", 1,4,|cpu,mmu| cpu.r.c = cpu.r.h);
    ol.add(0x00_4d, "LD C, L", 1,4,|cpu,mmu| cpu.r.c = cpu.r.l);
    ol.add(0x00_4f, "LD C, A", 1,4,|cpu,mmu| cpu.r.c = cpu.r.a);

    ol.add(0x00_50, "LD D, B", 1,4,|cpu,mmu| cpu.r.d = cpu.r.b);
    ol.add(0x00_51, "LD D, C", 1,4,|cpu,mmu| cpu.r.d = cpu.r.c);
    ol.add(0x00_52, "LD D, D", 1,4,|cpu,mmu| cpu.r.d = cpu.r.d);
    ol.add(0x00_53, "LD D, E", 1,4,|cpu,mmu| cpu.r.d = cpu.r.e);
    ol.add(0x00_54, "LD D, H", 1,4,|cpu,mmu| cpu.r.d = cpu.r.h);
    ol.add(0x00_55, "LD D, L", 1,4,|cpu,mmu| cpu.r.d = cpu.r.l);
    ol.add(0x00_57, "LD D, A", 1,4,|cpu,mmu| cpu.r.d = cpu.r.a);
    ol.add(0x00_58, "LD E, B", 1,4,|cpu,mmu| cpu.r.e = cpu.r.b);
    ol.add(0x00_59, "LD E, C", 1,4,|cpu,mmu| cpu.r.e = cpu.r.c);
    ol.add(0x00_5a, "LD E, D", 1,4,|cpu,mmu| cpu.r.e = cpu.r.d);
    ol.add(0x00_5b, "LD E, E", 1,4,|cpu,mmu| cpu.r.e = cpu.r.e);
    ol.add(0x00_5c, "LD E, H", 1,4,|cpu,mmu| cpu.r.e = cpu.r.h);
    ol.add(0x00_5d, "LD E, L", 1,4,|cpu,mmu| cpu.r.e = cpu.r.l);
    ol.add(0x00_5f, "LD E, A", 1,4,|cpu,mmu| cpu.r.e = cpu.r.a);

    ol.add(0x00_60, "LD H, B", 1,4,|cpu,mmu| cpu.r.h = cpu.r.b);
    ol.add(0x00_61, "LD H, C", 1,4,|cpu,mmu| cpu.r.h = cpu.r.c);
    ol.add(0x00_62, "LD H, D", 1,4,|cpu,mmu| cpu.r.h = cpu.r.d);
    ol.add(0x00_63, "LD H, E", 1,4,|cpu,mmu| cpu.r.h = cpu.r.e);
    ol.add(0x00_64, "LD H, H", 1,4,|cpu,mmu| cpu.r.h = cpu.r.h);
    ol.add(0x00_65, "LD H, L", 1,4,|cpu,mmu| cpu.r.h = cpu.r.l);
    ol.add(0x00_67, "LD H, A", 1,4,|cpu,mmu| cpu.r.h = cpu.r.a);
    ol.add(0x00_68, "LD L, B", 1,4,|cpu,mmu| cpu.r.l = cpu.r.b);
    ol.add(0x00_69, "LD L, C", 1,4,|cpu,mmu| cpu.r.l = cpu.r.c);
    ol.add(0x00_6a, "LD L, D", 1,4,|cpu,mmu| cpu.r.l = cpu.r.d);
    ol.add(0x00_6b, "LD L, E", 1,4,|cpu,mmu| cpu.r.l = cpu.r.e);
    ol.add(0x00_6c, "LD L, H", 1,4,|cpu,mmu| cpu.r.l = cpu.r.h);
    ol.add(0x00_6d, "LD L, L", 1,4,|cpu,mmu| cpu.r.l = cpu.r.l);
    ol.add(0x00_6f, "LD L, A", 1,4,|cpu,mmu| cpu.r.l = cpu.r.a);

    // e0 => LDH (n), A => load contents of A into address of (0xFF00 + immediate value)
    ol.add(0x00_e0, "LDH(n), A",2,12, |cpu,mmu| {
        let v = mmu.read8(cpu.r.pc+1);
        println!("n is {:x}",v);
        let addr = (0xFF00 as u16) + (v as u16);
        println!("calculated address {:04x}",addr);
        mmu.write8(addr,cpu.r.a);
    });

    return ol;
}
