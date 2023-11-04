use super::test_display;

use hex_literal::hex;

macro_rules! test {
    ($testname:ident, $hash:expr) => {
        #[allow(non_snake_case)]
        #[test]
        fn $testname() {
            test_display(
                include_bytes!(concat!(
                    "../../../souper_tests/SNES/CPUTest/CPU/",
                    stringify!($testname),
                    "/CPU",
                    stringify!($testname),
                    ".sfc"
                )),
                &hex!($hash),
                20000,
                true,
            );
        }
    };
}

test!(
    ADC,
    "cab2c6662a14a8b177e0ca6bb8b865c12656b1c166779d0ae704e9e5baaaf5dc"
);
test!(
    AND,
    "158ba5d6723be51641ffe49447b088bafe170d7f21ec3c6bd04f88a9e1ef0618"
);
test!(
    ASL,
    "041c896409458ddffe8225d710bc5823776af97f737ed0f40ad124d69c96eb1d"
);
test!(
    BIT,
    "3fbf760e151a726a675003931fdca8e7aed75a4c515dae301db5ae6326338ffc"
);
test!(
    BRA,
    "d20d33774f4f96b8bef79141e90367c665024e1740cd4eab379156fe9615a00e"
);
test!(
    CMP,
    "ea1b154d1670dd6ba2bead1073ad25eb41e1a8751bd77960eb29d74a75d3b00c"
);
test!(
    DEC,
    "587280edeaa4a8664f7d636cda285c0f58d1c875217b7bf228670ffb2c4b4b89"
);
test!(
    EOR,
    "399c8d430fe782d91a6d3c0c7189110af6be26faf894b0d918fdd7f8490d56f0"
);
test!(
    INC,
    "b5f37b2ac1ea873db20a79126fcf4d862984656b32133607c683204e3cbb546c"
);
test!(
    JMP,
    "00d2ae4d37b5fdd4656dd9b2dbcc00387dd6ba84e626a190b860bfc78261099d"
);
test!(
    LDR,
    "c3b2d95ffc9ad2b98eeb3200d078ecb8fac0e2631eaff871c84967c85463f442"
);
test!(
    LSR,
    "af6bb34554a56a2d698f1feba48dca07910ec47c20f6bc64e196ae8684c9bec8"
);
test!(
    MOV,
    "53b5bab84bb6059af1b3b28d33520d6e4d58c157eeef8727f4e2652637c2254f"
);
//test!(
//    MSC,
//    ""
//);
test!(
    ORA,
    "e65738dffc80bd20c05b29caee38d27f95a1efa812501bda98ab8bfbab680ab7"
);
test!(
    PHL,
    "6f0e80f3aec0ca1a1ae1cbb7e3e7ff07bfb867cd2ca03c2dea2688ac46409da0"
);
test!(
    PSR,
    "b5b8abec1fd6d402ea0b56402499e583ab98b72cf3a1f2f313d7138b0c2069fd"
);
test!(
    RET,
    "2eb228e9dbc4d03fe88f595dc7ce93e37643e5cb86377b5f12013a38e4a6ce81"
);
test!(
    ROL,
    "5df1c0ac47243d501738274bd9878639794e99055a45a95618367575daea005d"
);
test!(
    ROR,
    "6d20b4590ce4d8972524f717d1789d4d10b77c17c6295b9fba6adc3e6bd0a6bc"
);
test!(
    SBC,
    "273068764d3661b9f6cb9b4844dfd3a2f3770a93bb8605cc7bcdbeea3062a3bf"
);
test!(
    STR,
    "d78ee8a45f70d93fd869e92bd7e14af218d890cd437bbd1dbdd0a73653763d59"
);
test!(
    TRN,
    "3c13f89ace1ab878e303d08b81f705a1a50e1d8d9e9bc42d1335a733ee355729"
);
