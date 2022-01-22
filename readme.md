
# a mostly broken gameboy emulator written in Rust

Currently only the CPU emulation works, and not even that much.

To try executing the boot rom

```shell
cargo run
```

To try executing one of the test roms

```shell
cargo run "./resources/testroms/cpu_instrs/individual/04-op r,imm.gb"
```

You can run it in the debugger with --interactive. You can
fast forward a certain number of clock cycles with --fastforward. For example, to run the hello world demo to the point where it finishes drawing do this:
```shell
cargo run -- --interactive --fastforward=14575  ./resources/testroms/hello-world.gb
```



`hello-world.gb` is from [here](https://github.com/dusterherz/gb-hello-world) by 'dusterherz'.

`GB-ticTacToe.gb` is from [here](https://gbhh.avivace.com/game/GB-Tic-Tac-Toe) by 'Norman Nithman'

`vectroid.gbc` is from [here](https://gitlab.com/BonsaiDen/vectroid.gb) by 'Ivo Wetzel'

