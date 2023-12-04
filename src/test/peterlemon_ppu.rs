use super::test_display;

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
            );
        }
    };
}

ppu_test!(
    BGMAP_8x8BG1Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG1Map2BPP32x328PAL/8x8BG1Map2BPP32x328PAL.sfc",
    "7168d73281a2ac2fa148be6299a27b6658bea7336dbbd9e1d64a4777354d0298"
);
ppu_test!(
    BGMAP_8x8BG2Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG2Map2BPP32x328PAL/8x8BG2Map2BPP32x328PAL.sfc",
    "ed04e4f004d43093b4e05909219f15f473256b837f8b8e8eb55f2e4d517af203"
);
ppu_test!(
    BGMAP_8x8BG3Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG3Map2BPP32x328PAL/8x8BG3Map2BPP32x328PAL.sfc",
    "ed04e4f004d43093b4e05909219f15f473256b837f8b8e8eb55f2e4d517af203"
);
ppu_test!(
    BGMAP_8x8BG4Map2BPP32x328PAL,
    "BGMAP/8x8/2BPP/8x8BG4Map2BPP32x328PAL/8x8BG4Map2BPP32x328PAL.sfc",
    "ed04e4f004d43093b4e05909219f15f473256b837f8b8e8eb55f2e4d517af203"
);
ppu_test!(
    BGMAP_8x8BG4Map4BPP32x328PAL,
    "BGMAP/8x8/4BPP/8x8BGMap4BPP32x328PAL/8x8BGMap4BPP32x328PAL.sfc",
    "376c8e27eb22a1885e434eefd5ca24144ee48b93d2938eaba2e959667aae8bb7"
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
    "24ad228f589fccfa8882fcd9a594366a0a517972a7114f2d964b34025f0b355a"
);

// Color math tests
ppu_test!(
    HiColor1241DLair,
    "Blend/HiColor/HiColor1241DLair/HiColor1241DLair.sfc",
    "7342e56437c435e7236fe69c283b4e8e4ce17b2c6d5a0f6e85ac915dce16be02"
);
ppu_test!(
    HiColor3840,
    "Blend/HiColor/HiColor3840/HiColor3840.sfc",
    "0759ec270966298ed871806502c6ffda3131213a87e67edbe4396ff2e49bc03d"
);
ppu_test!(
    HiColor575Myst,
    "Blend/HiColor/HiColor575Myst/HiColor575Myst.sfc",
    "8f9117eaf0bc0e3efd2f2ba81f3cef7f02646f9586eaedd8dea3d01c3742ad8b"
);
