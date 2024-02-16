use super::test_display;
use crate::snes::cartridge::Mapper;

use hex_literal::hex;

macro_rules! lorom_test {
    ($testname:ident, $testfn:expr, $hash:expr) => {
        #[allow(non_snake_case)]
        #[test]
        fn $testname() {
            test_display(
                include_bytes!(concat!("../../../siena_tests/SNES/BANK/", $testfn)),
                &hex!($hash),
                60000,
                true,
                Mapper::LoROM,
            );
        }
    };
}
macro_rules! hirom_test {
    ($testname:ident, $testfn:expr, $hash:expr) => {
        #[allow(non_snake_case)]
        #[test]
        fn $testname() {
            test_display(
                include_bytes!(concat!("../../../siena_tests/SNES/BANK/", $testfn)),
                &hex!($hash),
                60000,
                true,
                Mapper::HiROM,
            );
        }
    };
}
hirom_test!(
    BANKHiROMFastROM,
    "HiROMFastROM/BANKHiROMFastROM.sfc",
    "2ab4871e60935c6fcaf06a40849912f7b8ad23f8e850bb0f03d2576e96664536"
);
hirom_test!(
    BANKHiROMSlowROM,
    "HiROMSlowROM/BANKHiROMSlowROM.sfc",
    "36ab039767172d7a452d6337f7703627e912e8ffc833b10e3eb3467b1cc38e60"
);
lorom_test!(
    BANKLoROMFastROM,
    "LoROMFastROM/BANKLoROMFastROM.sfc",
    "1f2385252686bc02246eed8d670ee46d1bd01e25ad3c0a217dcb84a180e2f045"
);
lorom_test!(
    BANKLoROMSlowROM,
    "LoROMSlowROM/BANKLoROMSlowROM.sfc",
    "89b0ae2ce790bceaa0ea97f780b78e69546e812c0539a39b810d6e87f7d2f379"
);
lorom_test!(
    BANKWRAM,
    "WRAM/BANKWRAM.sfc",
    "c54861d958c14c00b9eadfe2727b0ce9c4b6e25411cf1b833efe337714b61a6c"
);
