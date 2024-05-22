use dbg_hex::dbg_hex;
use serde_json::Value;
use std::collections::HashMap;
use std::fs;

use crate::cpu_sm83::cpu::CpuSm83;
use crate::cpu_sm83::regs::RegisterFile;
use crate::gameboy::bus::bus::BusMember;
use crate::gameboy::bus::testbus::{Access, Testbus};

macro_rules! cpu_test {
    ($testfn:ident, $instr:expr, $cycle_count_correct:expr) => {
        #[test]
        fn $testfn() {
            assert_eq!(stringify!($testfn), format!("instr_{:02x}", $instr));

            let filename = format!("../siena_tests/GameboyCPUTests/v1/{:02x}.json", $instr);
            let testcases: Value =
                serde_json::from_str(fs::read_to_string(filename).unwrap().as_str()).unwrap();

            for testcase in testcases.as_array().unwrap() {
                run_testcase(testcase, $cycle_count_correct);
            }
        }
    };
}

fn parse_regs(v: &Value) -> RegisterFile {
    let p = |r| u8::from_str_radix(&v[r].as_str().unwrap().trim_start_matches("0x"), 16).unwrap();
    let p16 =
        |r| u16::from_str_radix(&v[r].as_str().unwrap().trim_start_matches("0x"), 16).unwrap();
    RegisterFile {
        a: p("a"),
        b: p("b"),
        c: p("c"),
        d: p("d"),
        e: p("e"),
        f: p("f"),
        h: p("h"),
        l: p("l"),
        pc: p16("pc"),
        sp: p16("sp"),
    }
}

fn parse_ram(v: &Value) -> HashMap<u16, u8> {
    HashMap::from_iter(v.as_array().unwrap().into_iter().map(|a| {
        (
            u16::from_str_radix(a[0].as_str().unwrap().trim_start_matches("0x"), 16).unwrap(),
            u8::from_str_radix(a[1].as_str().unwrap().trim_start_matches("0x"), 16).unwrap(),
        )
    }))
}

fn run_testcase(testcase: &Value, cycle_count_correct: bool) {
    let regs_initial = parse_regs(&testcase["initial"]["cpu"]);
    let regs_final = parse_regs(&testcase["final"]["cpu"]);
    let ram_initial = parse_ram(&testcase["initial"]["ram"]);
    let ram_final = parse_ram(&testcase["final"]["ram"]);

    let mut bus = Testbus::new();
    for (addr, val) in ram_initial {
        bus.write(addr, val);
    }
    bus.reset_trace();

    let mut cpu = CpuSm83::new(bus, false);
    cpu.regs = regs_initial;
    cpu.step().unwrap();

    // Extract the bus trace now so we don't record the
    // verification later.
    let bus = std::mem::replace(&mut cpu.bus, Testbus::new());
    let mut bus_trace = bus.get_trace();

    if cpu.regs != regs_final {
        dbg!(testcase);
        dbg_hex!(regs_final);
        dbg_hex!(cpu.regs);
        panic!("Registers incorrect");
    }

    for (addr, exp_val) in ram_final {
        let val = bus.read(addr);
        if exp_val != val {
            dbg!(testcase);
            panic!(
                "Addr {:04X} - expected {:02X}, saw {:02X}",
                addr, exp_val, val
            );
        }
    }

    if cycle_count_correct {
        // The amount of cycles is not correct in some
        // test data files. We ignore the cycle count for those.

        assert!(cpu.get_cycles() / 4 >= testcase["cycles"].as_array().unwrap().len());
    }

    // Compare bus trace to the cycles in the test data
    // Dispose of the first trace, which is the check for
    // the bootrom disable bit in CPU::new().
    bus_trace.remove(0);

    for (trace, expected) in bus_trace.iter().zip(
        testcase["cycles"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|c| !c.is_null()),
    ) {
        let exp_addr =
            u16::from_str_radix(expected[0].as_str().unwrap().trim_start_matches("0x"), 16)
                .unwrap();
        let exp_val =
            u8::from_str_radix(expected[1].as_str().unwrap().trim_start_matches("0x"), 16).unwrap();
        let exp_access = match expected[2].as_str().unwrap() {
            "read" => Access::Read,
            "write" => Access::Write,
            _ => unreachable!(),
        };

        if trace.addr != exp_addr || trace.val != exp_val || trace.access != exp_access {
            dbg!(&testcase);
            dbg_hex!(&bus_trace);
            dbg_hex!(&trace);
            dbg!(&expected);
            panic!("Invalid trace");
        }
    }
}

cpu_test!(instr_00, 0x00, true);
cpu_test!(instr_01, 0x01, true);
cpu_test!(instr_02, 0x02, true);
cpu_test!(instr_03, 0x03, true);
cpu_test!(instr_04, 0x04, true);
cpu_test!(instr_05, 0x05, true);
cpu_test!(instr_06, 0x06, true);
cpu_test!(instr_07, 0x07, true);
cpu_test!(instr_08, 0x08, true);
cpu_test!(instr_09, 0x09, true);
cpu_test!(instr_0a, 0x0a, true);
cpu_test!(instr_0b, 0x0b, true);
cpu_test!(instr_0c, 0x0c, true);
cpu_test!(instr_0d, 0x0d, true);
cpu_test!(instr_0e, 0x0e, true);
cpu_test!(instr_0f, 0x0f, true);
cpu_test!(instr_10, 0x10, false);
cpu_test!(instr_11, 0x11, true);
cpu_test!(instr_12, 0x12, true);
cpu_test!(instr_13, 0x13, true);
cpu_test!(instr_14, 0x14, true);
cpu_test!(instr_15, 0x15, true);
cpu_test!(instr_16, 0x16, true);
cpu_test!(instr_17, 0x17, true);
cpu_test!(instr_18, 0x18, true);
cpu_test!(instr_19, 0x19, true);
cpu_test!(instr_1a, 0x1a, true);
cpu_test!(instr_1b, 0x1b, true);
cpu_test!(instr_1c, 0x1c, true);
cpu_test!(instr_1d, 0x1d, true);
cpu_test!(instr_1e, 0x1e, true);
cpu_test!(instr_1f, 0x1f, true);
cpu_test!(instr_20, 0x20, true);
cpu_test!(instr_21, 0x21, true);
cpu_test!(instr_22, 0x22, true);
cpu_test!(instr_23, 0x23, true);
cpu_test!(instr_24, 0x24, true);
cpu_test!(instr_25, 0x25, true);
cpu_test!(instr_26, 0x26, true);
cpu_test!(instr_27, 0x27, true);
cpu_test!(instr_28, 0x28, true);
cpu_test!(instr_29, 0x29, true);
cpu_test!(instr_2a, 0x2a, true);
cpu_test!(instr_2b, 0x2b, true);
cpu_test!(instr_2c, 0x2c, true);
cpu_test!(instr_2d, 0x2d, true);
cpu_test!(instr_2e, 0x2e, true);
cpu_test!(instr_2f, 0x2f, true);
cpu_test!(instr_30, 0x30, true);
cpu_test!(instr_31, 0x31, true);
cpu_test!(instr_32, 0x32, true);
cpu_test!(instr_33, 0x33, true);
cpu_test!(instr_34, 0x34, true);
cpu_test!(instr_35, 0x35, true);
cpu_test!(instr_36, 0x36, true);
cpu_test!(instr_37, 0x37, true);
cpu_test!(instr_38, 0x38, true);
cpu_test!(instr_39, 0x39, true);
cpu_test!(instr_3a, 0x3a, true);
cpu_test!(instr_3b, 0x3b, true);
cpu_test!(instr_3c, 0x3c, true);
cpu_test!(instr_3d, 0x3d, true);
cpu_test!(instr_3e, 0x3e, true);
cpu_test!(instr_3f, 0x3f, true);
cpu_test!(instr_40, 0x40, true);
cpu_test!(instr_41, 0x41, true);
cpu_test!(instr_42, 0x42, true);
cpu_test!(instr_43, 0x43, true);
cpu_test!(instr_44, 0x44, true);
cpu_test!(instr_45, 0x45, true);
cpu_test!(instr_46, 0x46, true);
cpu_test!(instr_47, 0x47, true);
cpu_test!(instr_48, 0x48, true);
cpu_test!(instr_49, 0x49, true);
cpu_test!(instr_4a, 0x4a, true);
cpu_test!(instr_4b, 0x4b, true);
cpu_test!(instr_4c, 0x4c, true);
cpu_test!(instr_4d, 0x4d, true);
cpu_test!(instr_4e, 0x4e, true);
cpu_test!(instr_4f, 0x4f, true);
cpu_test!(instr_50, 0x50, true);
cpu_test!(instr_51, 0x51, true);
cpu_test!(instr_52, 0x52, true);
cpu_test!(instr_53, 0x53, true);
cpu_test!(instr_54, 0x54, true);
cpu_test!(instr_55, 0x55, true);
cpu_test!(instr_56, 0x56, true);
cpu_test!(instr_57, 0x57, true);
cpu_test!(instr_58, 0x58, true);
cpu_test!(instr_59, 0x59, true);
cpu_test!(instr_5a, 0x5a, true);
cpu_test!(instr_5b, 0x5b, true);
cpu_test!(instr_5c, 0x5c, true);
cpu_test!(instr_5d, 0x5d, true);
cpu_test!(instr_5e, 0x5e, true);
cpu_test!(instr_5f, 0x5f, true);
cpu_test!(instr_60, 0x60, true);
cpu_test!(instr_61, 0x61, true);
cpu_test!(instr_62, 0x62, true);
cpu_test!(instr_63, 0x63, true);
cpu_test!(instr_64, 0x64, true);
cpu_test!(instr_65, 0x65, true);
cpu_test!(instr_66, 0x66, true);
cpu_test!(instr_67, 0x67, true);
cpu_test!(instr_68, 0x68, true);
cpu_test!(instr_69, 0x69, true);
cpu_test!(instr_6a, 0x6a, true);
cpu_test!(instr_6b, 0x6b, true);
cpu_test!(instr_6c, 0x6c, true);
cpu_test!(instr_6d, 0x6d, true);
cpu_test!(instr_6e, 0x6e, true);
cpu_test!(instr_6f, 0x6f, true);
cpu_test!(instr_70, 0x70, true);
cpu_test!(instr_71, 0x71, true);
cpu_test!(instr_72, 0x72, true);
cpu_test!(instr_73, 0x73, true);
cpu_test!(instr_74, 0x74, true);
cpu_test!(instr_75, 0x75, true);
cpu_test!(instr_76, 0x76, false);
cpu_test!(instr_77, 0x77, true);
cpu_test!(instr_78, 0x78, true);
cpu_test!(instr_79, 0x79, true);
cpu_test!(instr_7a, 0x7a, true);
cpu_test!(instr_7b, 0x7b, true);
cpu_test!(instr_7c, 0x7c, true);
cpu_test!(instr_7d, 0x7d, true);
cpu_test!(instr_7e, 0x7e, true);
cpu_test!(instr_7f, 0x7f, true);
cpu_test!(instr_80, 0x80, true);
cpu_test!(instr_81, 0x81, true);
cpu_test!(instr_82, 0x82, true);
cpu_test!(instr_83, 0x83, true);
cpu_test!(instr_84, 0x84, true);
cpu_test!(instr_85, 0x85, true);
cpu_test!(instr_86, 0x86, true);
cpu_test!(instr_87, 0x87, true);
cpu_test!(instr_88, 0x88, true);
cpu_test!(instr_89, 0x89, true);
cpu_test!(instr_8a, 0x8a, true);
cpu_test!(instr_8b, 0x8b, true);
cpu_test!(instr_8c, 0x8c, true);
cpu_test!(instr_8d, 0x8d, true);
cpu_test!(instr_8e, 0x8e, true);
cpu_test!(instr_8f, 0x8f, true);
cpu_test!(instr_90, 0x90, true);
cpu_test!(instr_91, 0x91, true);
cpu_test!(instr_92, 0x92, true);
cpu_test!(instr_93, 0x93, true);
cpu_test!(instr_94, 0x94, true);
cpu_test!(instr_95, 0x95, true);
cpu_test!(instr_96, 0x96, true);
cpu_test!(instr_97, 0x97, true);
cpu_test!(instr_98, 0x98, true);
cpu_test!(instr_99, 0x99, true);
cpu_test!(instr_9a, 0x9a, true);
cpu_test!(instr_9b, 0x9b, true);
cpu_test!(instr_9c, 0x9c, true);
cpu_test!(instr_9d, 0x9d, true);
cpu_test!(instr_9e, 0x9e, true);
cpu_test!(instr_9f, 0x9f, true);
cpu_test!(instr_a0, 0xa0, true);
cpu_test!(instr_a1, 0xa1, true);
cpu_test!(instr_a2, 0xa2, true);
cpu_test!(instr_a3, 0xa3, true);
cpu_test!(instr_a4, 0xa4, true);
cpu_test!(instr_a5, 0xa5, true);
cpu_test!(instr_a6, 0xa6, true);
cpu_test!(instr_a7, 0xa7, true);
cpu_test!(instr_a8, 0xa8, true);
cpu_test!(instr_a9, 0xa9, true);
cpu_test!(instr_aa, 0xaa, true);
cpu_test!(instr_ab, 0xab, true);
cpu_test!(instr_ac, 0xac, true);
cpu_test!(instr_ad, 0xad, true);
cpu_test!(instr_ae, 0xae, true);
cpu_test!(instr_af, 0xaf, true);
cpu_test!(instr_b0, 0xb0, true);
cpu_test!(instr_b1, 0xb1, true);
cpu_test!(instr_b2, 0xb2, true);
cpu_test!(instr_b3, 0xb3, true);
cpu_test!(instr_b4, 0xb4, true);
cpu_test!(instr_b5, 0xb5, true);
cpu_test!(instr_b6, 0xb6, true);
cpu_test!(instr_b7, 0xb7, true);
cpu_test!(instr_b8, 0xb8, true);
cpu_test!(instr_b9, 0xb9, true);
cpu_test!(instr_ba, 0xba, true);
cpu_test!(instr_bb, 0xbb, true);
cpu_test!(instr_bc, 0xbc, true);
cpu_test!(instr_bd, 0xbd, true);
cpu_test!(instr_be, 0xbe, true);
cpu_test!(instr_bf, 0xbf, true);
cpu_test!(instr_c0, 0xc0, true);
cpu_test!(instr_c1, 0xc1, true);
cpu_test!(instr_c2, 0xc2, true);
cpu_test!(instr_c3, 0xc3, true);
cpu_test!(instr_c4, 0xc4, true);
cpu_test!(instr_c5, 0xc5, true);
cpu_test!(instr_c6, 0xc6, true);
cpu_test!(instr_c7, 0xc7, true);
cpu_test!(instr_c8, 0xc8, true);
cpu_test!(instr_c9, 0xc9, true);
cpu_test!(instr_ca, 0xca, true);
cpu_test!(instr_cb, 0xcb, true);
cpu_test!(instr_cc, 0xcc, true);
cpu_test!(instr_cd, 0xcd, true);
cpu_test!(instr_ce, 0xce, true);
cpu_test!(instr_cf, 0xcf, true);
cpu_test!(instr_d0, 0xd0, true);
cpu_test!(instr_d1, 0xd1, true);
cpu_test!(instr_d2, 0xd2, true);
// d3
cpu_test!(instr_d4, 0xd4, true);
cpu_test!(instr_d5, 0xd5, true);
cpu_test!(instr_d6, 0xd6, true);
cpu_test!(instr_d7, 0xd7, true);
cpu_test!(instr_d8, 0xd8, true);
cpu_test!(instr_d9, 0xd9, true);
cpu_test!(instr_da, 0xda, true);
// db
cpu_test!(instr_dc, 0xdc, true);
// dd
cpu_test!(instr_de, 0xde, true);
cpu_test!(instr_df, 0xdf, true);
cpu_test!(instr_e0, 0xe0, true);
cpu_test!(instr_e1, 0xe1, true);
cpu_test!(instr_e2, 0xe2, true);
// e3
// e4
cpu_test!(instr_e5, 0xe5, true);
cpu_test!(instr_e6, 0xe6, true);
cpu_test!(instr_e7, 0xe7, true);
cpu_test!(instr_e8, 0xe8, true);
cpu_test!(instr_e9, 0xe9, true);
cpu_test!(instr_ea, 0xea, true);
// eb
// ec
// ed
cpu_test!(instr_ee, 0xee, true);
cpu_test!(instr_ef, 0xef, true);
cpu_test!(instr_f0, 0xf0, true);
cpu_test!(instr_f1, 0xf1, true);
cpu_test!(instr_f2, 0xf2, true);
cpu_test!(instr_f3, 0xf3, true);
// f4
cpu_test!(instr_f5, 0xf5, true);
cpu_test!(instr_f6, 0xf6, true);
cpu_test!(instr_f7, 0xf7, true);
cpu_test!(instr_f8, 0xf8, true);
cpu_test!(instr_f9, 0xf9, true);
cpu_test!(instr_fa, 0xfa, true);
cpu_test!(instr_fb, 0xfb, true);
// fc
// fd
cpu_test!(instr_fe, 0xfe, true);
// ff - RST 38h - no test file
