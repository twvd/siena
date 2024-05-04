use super::test_display;
use crate::snes::cartridge::Mapper;

use hex_literal::hex;

macro_rules! ppu_test {
    ($testname:ident, $testfn:expr, $hash:expr) => {
        #[allow(non_snake_case)]
        #[test]
        fn $testname() {
            test_display(
                include_bytes!(concat!("../../../siena_tests/SNES/PPU/", $testfn)),
                &hex!($hash),
                60000,
                true,
                Mapper::LoROM,
            );
        }
    };
}
#[allow(unused_macros)]
macro_rules! ppu_test_ns {
    ($testname:ident, $testfn:expr, $hash:expr) => {
        #[allow(non_snake_case)]
        #[test]
        fn $testname() {
            test_display(
                include_bytes!(concat!("../../../siena_tests/SNES/PPU/", $testfn)),
                &hex!($hash),
                60000,
                false,
                false,
            );
        }
    };
}

ppu_test!(
    BGMAP_8x8BG1Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG1Map2BPP32x328PAL/8x8BG1Map2BPP32x328PAL.sfc",
    "41d8648bd8f446360a380356bbb2d89ec0fe49271c507c894edd3c0536adcff2"
);
ppu_test!(
    BGMAP_8x8BG2Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG2Map2BPP32x328PAL/8x8BG2Map2BPP32x328PAL.sfc",
    "31178df307556b79d0f8f3d3aa1aa7303d4d7e773e620ff26fc232509358befd"
);
ppu_test!(
    BGMAP_8x8BG3Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG3Map2BPP32x328PAL/8x8BG3Map2BPP32x328PAL.sfc",
    "31178df307556b79d0f8f3d3aa1aa7303d4d7e773e620ff26fc232509358befd"
);
ppu_test!(
    BGMAP_8x8BG4Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG4Map2BPP32x328PAL/8x8BG4Map2BPP32x328PAL.sfc",
    "31178df307556b79d0f8f3d3aa1aa7303d4d7e773e620ff26fc232509358befd"
);
ppu_test!(
    BGMAP_8x8BG4Map4BPP32x328PAL,
    "BGMAP/8x8/4BPP/8x8BGMap4BPP32x328PAL/8x8BGMap4BPP32x328PAL.sfc",
    "e15d3050d9d9b0d81a64755b76360b2b5e241ba861e69b807b8f8e7178e5e73b"
);
//ppu_test_ns!(
//    // This test moves automatically
//    // TODO fix
//    BGMAP_8x8BGMap8BPP32x32,
//    "BGMAP/8x8/8BPP/32x32/8x8BGMap8BPP32x32.sfc",
//    "a2c6faf484b61e227482cc702ac6e9301cc48ade675527f73e84bdb206b2ecf8"
//);
ppu_test!(
    Rings,
    "Rings/Rings.sfc",
    "78e55f33bd71d44aa0c3d2a65435298fe8317e346b1c0ad2e1d35793c8458f67"
);

// Color math tests
ppu_test!(
    HiColor1241DLair,
    "Blend/HiColor/HiColor1241DLair/HiColor1241DLair.sfc",
    "fe4f5d8fce894905e66fa0b26fd21c009b80a63b190b5ad448937e1404b622ba"
);
ppu_test!(
    HiColor3840,
    "Blend/HiColor/HiColor3840/HiColor3840.sfc",
    "8e4b2b3bab0167b25126d1bff6391d0138cb8c00c5f0a443c720107624dd31f9"
);
ppu_test!(
    HiColor575Myst,
    "Blend/HiColor/HiColor575Myst/HiColor575Myst.sfc",
    "887de33880a6d7719544097009238e7ebc09d1e80cbe93e1023e927fb6eeedba"
);

// Window/HDMA tests
ppu_test!(
    WindowHDMA,
    "Window/WindowHDMA/WindowHDMA.sfc",
    "643e601d8c5ebbd0314566fe355ec377990c6604b5fddf56644c4bbda3eff54d"
);
ppu_test!(
    WindowMultiHDMA,
    "Window/WindowMultiHDMA/WindowMultiHDMA.sfc",
    "da4ad21e28680c799409a35eebbbbf09a85284a08d15415977a2c29bb5aa867c"
);

// Mode 7 tests
ppu_test!(
    Mode7RotZoom,
    "Mode7/RotZoom/RotZoom.sfc",
    "9cc7010be5454eb794178fc676145b81912349a115b18d6e46b63cee63f2d7f6"
);
ppu_test!(
    // Tests screenover (transoarent)
    Mode7Perspective,
    "Mode7/Perspective/Perspective.sfc",
    "0b607da41c14acb1145e5656f1c40af542a2198b4433da414d1b97e1224fc711"
);
