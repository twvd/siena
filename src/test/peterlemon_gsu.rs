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
    HIB,
    "007cc9321ac216460fa3827b590cd9e320206da8e87049ad82c17ff62c049c0a"
);
test!(
    IWT,
    "8f9723fd9dc60671f2fe8533c2bbe440f00159d629022dbafb2480934e2d1603"
);
test!(
    NOT,
    "0df76d9fc716575400c4c6baaf53f284f59cf8b6ba066c013c150167e1c1652e"
);
test!(
    SEX,
    "0111d224c50333ca7a15649593a1978b27ab6a157d7b9d2296e35cf917dfa9b9"
);
test!(
    XOR,
    "cd0d145aa76c202c872d59e03866aee5ab82c183283a991c4b308ed521325918"
);
