use super::test_display;
use crate::snes::cartridge::Mapper;

use hex_literal::hex;

macro_rules! test {
    ($testname:ident, $hash:expr) => {
        #[allow(non_snake_case)]
        #[test]
        fn $testname() {
            test_display(
                include_bytes!(concat!(
                    "../../../siena_tests/SNES/CHIP/GSU/GSUTest/",
                    stringify!($testname),
                    "/GSU",
                    stringify!($testname),
                    ".sfc"
                )),
                &hex!($hash),
                20000,
                true,
                Mapper::SuperFX,
            );
        }
    };
}

test!(
    IWT,
    "8f9723fd9dc60671f2fe8533c2bbe440f00159d629022dbafb2480934e2d1603"
);
test!(
    NOT,
    "0df76d9fc716575400c4c6baaf53f284f59cf8b6ba066c013c150167e1c1652e"
);
