# Siena - Super Nintendo (SNES) emulator

[![Build and test](https://github.com/twvd/siena/actions/workflows/build_test.yml/badge.svg)](https://github.com/twvd/siena/actions/workflows/build_test.yml) [![codecov](https://codecov.io/github/twvd/siena/graph/badge.svg?token=12207MF9CW)](https://codecov.io/github/twvd/siena)

Siena is a Super Nintendo / Super Famicom emulator, written in Rust. This emulator is a hobby project born out of nostalgia. It is currently a work in progress and not really fit for playing games.

Current features and state:
 * SDL2 graphical frontend
 * Supports LoROM / HiROM cartridges, with auto-detect
 * Fully functional (native-mode only), cycle accurate 65816 main CPU core
 * Fully functional, cycle accurate SPC700 audio CPU core
 * Functional DMA and HDMA
 * PPU modes 0, 1, 3 functional
 * Color math

## Building and running

After checking out the source, run the following command to build and run the emulator, specifying a ROM:

```sh
cargo run --release -- path/to/rom.smc
```

## Tests

This project is automatically tested against:
 * Self-written unit tests
 * [Synthetic 65816 CPU tests](https://github.com/TomHarte/ProcessorTests/tree/main/65816)
 * [Synthetic SPC700 CPU tests](https://github.com/raddad772/jsmoo/tree/main/misc/tests/GeneratedTests/spc700/v1)
 * A collection of test ROMs by various people

Test input lives in a [separate repository](https://github.com/twvd/siena_tests) as they are rather large.

To run tests:

1. Checkout [siena_tests](https://github.com/twvd/siena_tests]) at the same level as this repository (so you have 'siena' and 'siena_tests' at the same directory level).
2. In the directory of this repository, run `cargo test`.

Coverage data is available on [Codecov](https://app.codecov.io/gh/twvd/siena).

![codecov sunburst](https://codecov.io/github/twvd/siena/graphs/sunburst.svg?token=12207MF9CW)
