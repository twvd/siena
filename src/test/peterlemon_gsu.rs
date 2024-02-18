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
    ADC,
    "37f2762099429d75055f307c9b1d5e82e204b0e431279c8036a9d108b5fb07eb"
);
test!(
    ADD,
    "de9f72e11f37844212389e9a90a5275ad54ac437b1d0c73b684f26a180228b2a"
);
test!(
    AND,
    "cd8cfe5d386d79c67599942f6f35403a8cca101b5337ac4e83e3ceacb6798bc8"
);
test!(
    BIC,
    "06aa897653bc52e02cf37f65b0bc71951a440b465aeed2d839e28dacaea1261e"
);
test!(
    HIB,
    "007cc9321ac216460fa3827b590cd9e320206da8e87049ad82c17ff62c049c0a"
);
test!(
    IWT,
    "8f9723fd9dc60671f2fe8533c2bbe440f00159d629022dbafb2480934e2d1603"
);
test!(
    MERGE,
    "e9fe1f2f7a24612b0d43ea5241b64b3c2453d0f824b3152c7d4cb85d1525e25c"
);
test!(
    NOT,
    "0df76d9fc716575400c4c6baaf53f284f59cf8b6ba066c013c150167e1c1652e"
);
test!(
    OR,
    "3247f4d2ff8df0f040a1c87295cda3655d5900a96bea94723a0fdf1029369cf1"
);
test!(
    SBC,
    "5ca8e8b3fd757c6624c12ab24933fa1877fdfdf5f1e93e456af06cf1eaf2cada"
);
test!(
    SEX,
    "0111d224c50333ca7a15649593a1978b27ab6a157d7b9d2296e35cf917dfa9b9"
);
test!(
    SUB,
    "1e79664b574ef3a418f8896763e3a7a78d6cf9d82e883f662574e389c9ea5bd2"
);
test!(
    XOR,
    "cd0d145aa76c202c872d59e03866aee5ab82c183283a991c4b308ed521325918"
);
