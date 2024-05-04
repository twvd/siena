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
    "d777408bf78b94b7cde5331f9b2669de066b4df1f744751c1aff4205f290f35a"
);
hirom_test!(
    BANKHiROMSlowROM,
    "HiROMSlowROM/BANKHiROMSlowROM.sfc",
    "af47aefcfd6f0a0842d7ef117d4539d8d3c1ae1e97ea713df8167fce8b81458c"
);
lorom_test!(
    BANKLoROMFastROM,
    "LoROMFastROM/BANKLoROMFastROM.sfc",
    "ba0b954785794fd831d84de131fb022b252aceab6fe1d2dbf154fcbab9ac0f41"
);
lorom_test!(
    BANKLoROMSlowROM,
    "LoROMSlowROM/BANKLoROMSlowROM.sfc",
    "09e7951a9d0ccee76f6e7c388a452d7e107a46703ae61e1272af661e1540973c"
);
lorom_test!(
    BANKWRAM,
    "WRAM/BANKWRAM.sfc",
    "fa69f0d486b66e4825fcc1a9c94a70ef4a748c748ae9c75e888302543209470e"
);
