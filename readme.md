
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


`Snake.gb` is from [here](https://donaldhays.com/projects/snake/) by 'Donald Hays'

special thanks to [here](https://github.com/mvdnes/rboy/blob/master/src/cpu.rs) for the bit map in rust examples

[bootrom explanation](https://realboyemulator.wordpress.com/2013/01/03/a-look-at-the-game-boy-bootstrap-let-the-fun-begin/)


# boot rom
the bootrom is internal to the emulator. You can include it with --boot. When stepping through it you'll probably
want to jump to certain interesting parts.  On startup it does a bunch of loops to clear memory and
perform the CRC check.

* 0  - start the whole process
* 24574  the very end of the VRAM clearing process, start audio init
* 24594  start Nintendo Logo check
* 28674  end of the logo check, starting to fill VRAM with tilemap
* 28806 start scrolling  

