use gb_emu::optest::setup_test_rom;

fn main() {
    println!("doing a cool debug here");

    let mut gb = setup_test_rom("./resources/testroms/hello-world.gb").unwrap();

    let goal = 20_000;
    gb.cpu.set_pc(0);
    loop {
        let prev_pc = gb.cpu.get_pc();
        gb.execute();
    }


}
