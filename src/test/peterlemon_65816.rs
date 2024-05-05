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
                    "../../../siena_tests/SNES/CPUTest/CPU/",
                    stringify!($testname),
                    "/CPU",
                    stringify!($testname),
                    ".sfc"
                )),
                &hex!($hash),
                60000,
                true,
                Mapper::LoROM,
            );
        }
    };
}

test!(
    ADC,
    "09e1e039d4ce04db11444c391dce4278fd9454a156ac00550c0e67a809afb0f4"
);
test!(
    AND,
    "5883aff6b941092a4537000267f39c85d2e98a276d65346b992f92b185d836ab"
);
test!(
    ASL,
    "4c8fb96e050fbefbc8b34891ba2d7a62ba60390291542674b3aadb20d2e28bd9"
);
test!(
    BIT,
    "6b979e59f0b84c114a78eecd5dc1be918ded4273d955a0c1cfd4782091a57fba"
);
test!(
    BRA,
    "855ad6b651be7bc54a79ca05ba5b3026527aab37c4f11112a17a2f3e3e535917"
);
test!(
    CMP,
    "f1d0681e1a5784a04be5e2c7dec14ef7eb2f440de68f834f8fb3d0e155e5a522"
);
test!(
    DEC,
    "bded97b61ade7ddc4c04f056d32fa161ee202388adbd09fdc7ffcaf17edb248d"
);
test!(
    EOR,
    "f93f0d8d4c4a82f015f54437bcb70f76c1c7404ac4c691291e05fdf3f4522690"
);
test!(
    INC,
    "66a3ad9b024c502fb57a6dbe84a48e771b38c790289935d9f0aca403de787edd"
);
test!(
    JMP,
    "99a7d5e4b8d6d3a2a3e9a913ff6ade1e36f8dc1ba9aa7b966c921947becfa64c"
);
test!(
    LDR,
    "b17468cc1c7cd6585fd022247af8ad2d45f50b048f47930893464dce46e93ac1"
);
test!(
    LSR,
    "231b26a5f0f68adf5c43e99cf556482e38aa51a89b400729d6036de372903206"
);
test!(
    MOV,
    "a935507f18286e8527a0a056ec0ac5f482daa1b13fefa9e5d5580a1ebe216f4a"
);
//test!(
//    MSC,
//    ""
//);
test!(
    ORA,
    "7c4c14dab0163ab0c05adf114f741bae6344973cb8636571cdbd8850f9c9caad"
);
test!(
    PHL,
    "ccd2830e7da40fe7ee4ff45fa96d579508868bf93956fbc0039c1c2118c910b5"
);
test!(
    PSR,
    "2cc82ca6150945d6e6e54b2726e17f27f2b7c3e8aeede349dcca5032d5e1208f"
);
test!(
    RET,
    "60ff23a891458de47c736cab93b1da275045809f314ad8bb034475dfdb82ad88"
);
test!(
    ROL,
    "c340d41e39c8818c10d329556d96a790a6661c3d834984f81396215f0ce706f5"
);
test!(
    ROR,
    "cbec3c3ad79e4ce60068b99d1733ac8aecebd3f24a491179644c5462a1a986e8"
);
test!(
    SBC,
    "a379af0bad0687a51f7aefd2887dda837508b2fa50ab8b61db3f8dcff15bb184"
);
test!(
    STR,
    "aa0a524afd646b589f83dabd2fc4d94ca941ea4ecdb69a29ff88705b75582df5"
);
test!(
    TRN,
    "d29eb5016ffff391324c79d7a4cd1f52cda2e811ce8cbbd35775b313370b5671"
);
