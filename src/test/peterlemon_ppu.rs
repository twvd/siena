use super::test_display;

use hex_literal::hex;

macro_rules! ppu_test {
    ($testname:ident, $testfn:expr, $hash:expr) => {
        #[allow(non_snake_case)]
        #[test]
        fn $testname() {
            test_display(
                include_bytes!(concat!("../../../souper_tests/SNES/PPU/", $testfn)),
                &hex!($hash),
                20000,
            );
        }
    };
}

ppu_test!(
    BGMAP_8x8BG1Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG1Map2BPP32x328PAL/8x8BG1Map2BPP32x328PAL.sfc",
    "346b1f7939b6438393ba631639c3236bf7ed6d737296cbb1e4f9b5eddec1f76f"
);
ppu_test!(
    BGMAP_8x8BG2Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG2Map2BPP32x328PAL/8x8BG2Map2BPP32x328PAL.sfc",
    "8c392a2df6f5d59f38c94577c79dadda0d8737b675e6dedb71ed97566d54b46d"
);
ppu_test!(
    BGMAP_8x8BG3Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG3Map2BPP32x328PAL/8x8BG3Map2BPP32x328PAL.sfc",
    "8c392a2df6f5d59f38c94577c79dadda0d8737b675e6dedb71ed97566d54b46d"
);
ppu_test!(
    BGMAP_8x8BG4Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG4Map2BPP32x328PAL/8x8BG4Map2BPP32x328PAL.sfc",
    "8c392a2df6f5d59f38c94577c79dadda0d8737b675e6dedb71ed97566d54b46d"
);
ppu_test!(
    BGMAP_8x8BG4Map4BPP32x328PAL,
    "BGMAP/8x8/4BPP/8x8BGMap4BPP32x328PAL/8x8BGMap4BPP32x328PAL.sfc",
    "6b85de62a208a94c5c3c393aa305e4bdd3207ca238923df5f57a167dcca1abdf"
);
ppu_test!(
    BGMAP_8x8BGMap8BPP32x32,
    "BGMAP/8x8/8BPP/32x32/8x8BGMap8BPP32x32.sfc",
    "a2c6faf484b61e227482cc702ac6e9301cc48ade675527f73e84bdb206b2ecf8"
);
