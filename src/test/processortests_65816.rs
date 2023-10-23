use dbg_hex::dbg_hex;
use serde_json::Value;
use std::collections::HashMap;
use std::fs;

use crate::snes::bus::testbus::{Access, Testbus};
use crate::snes::bus::{Address, Bus};
use crate::snes::cpu_65816::cpu::Cpu65816;
use crate::snes::cpu_65816::regs::RegisterFile;

macro_rules! _cpu_test {
    ($testfn:ident, $instr:expr, $trace:expr, $steps:expr) => {
        #[test]
        fn $testfn() {
            assert_eq!(stringify!($testfn), format!("instr_{:02x}", $instr));

            let filename = format!(
                "../souper_tests/ProcessorTests/65816/v1/{:02x}.n.json",
                $instr
            );
            let testcases: Value =
                serde_json::from_str(fs::read_to_string(filename).unwrap().as_str()).unwrap();

            for testcase in testcases.as_array().unwrap() {
                run_testcase(testcase, $trace, $steps);
            }
        }
    };
}

macro_rules! cpu_test {
    ($testfn:ident, $instr:expr) => {
        _cpu_test!($testfn, $instr, true, false);
    };
}

macro_rules! cpu_test_steps {
    ($testfn:ident, $instr:expr) => {
        _cpu_test!($testfn, $instr, true, true);
    };
}

macro_rules! cpu_test_no_trace {
    ($testfn:ident, $instr:expr) => {
        _cpu_test!($testfn, $instr, false, false);
    };
}

fn parse_regs(v: &Value) -> RegisterFile {
    let p8 = |r| v[r].as_u64().unwrap().try_into().unwrap();
    let p16 = |r| v[r].as_u64().unwrap().try_into().unwrap();
    RegisterFile {
        c: p16("a"),
        dbr: p8("dbr"),
        d: p16("d"),
        k: p8("pbr"),
        pc: p16("pc"),
        p: p8("p"),
        s: p16("s"),
        x: p16("x"),
        y: p16("y"),
        emulation: match p8("e") {
            0 => false,
            1 => true,
            _ => unreachable!(),
        },
    }
}

fn parse_ram(v: &Value) -> HashMap<Address, u8> {
    HashMap::from_iter(v.as_array().unwrap().into_iter().map(|a| {
        (
            a[0].as_u64().unwrap().try_into().unwrap(),
            a[1].as_u64().unwrap().try_into().unwrap(),
        )
    }))
}

fn run_testcase(testcase: &Value, check_trace: bool, multi_steps: bool) {
    let regs_initial = parse_regs(&testcase["initial"]);
    let regs_final = parse_regs(&testcase["final"]);
    let ram_initial = parse_ram(&testcase["initial"]["ram"]);
    let ram_final = parse_ram(&testcase["final"]["ram"]);
    let testcase_cycles = testcase["cycles"].as_array().unwrap();
    let test_cycles = testcase_cycles.len();

    let mut bus = Testbus::new();
    for (addr, val) in ram_initial {
        bus.write(addr, val);
    }

    let mut cpu = Cpu65816::<Testbus>::new(bus, 0);
    cpu.regs = regs_initial.clone();
    cpu.bus.reset_trace();

    cpu.step().unwrap();
    let per_step = cpu.cycles;
    // Keep stepping until we reach a consistent state for
    // instructions that are executed multiple times for some
    // test cases (MVN/MVP).
    while multi_steps && (cpu.cycles + per_step) <= test_cycles {
        cpu.step().unwrap();
    }

    // Extract the bus trace now so we don't record the
    // verification later.
    let bus = std::mem::replace(&mut cpu.bus, Testbus::new());
    let bus_trace = bus.get_trace();

    if multi_steps {
        // Ignore PC for instructions that run in multiple steps
        // (MVN, MVP).
        cpu.regs.pc = regs_final.pc;
    }
    if cpu.regs != regs_final {
        dbg!(testcase);
        println!("Initial: {}", regs_initial);
        println!("Expected:{}", regs_final);
        println!("Actual:  {}", cpu.regs);
        panic!("Registers incorrect");
    }

    for (addr, exp_val) in ram_final {
        let val = bus.read(addr);
        if exp_val != val {
            dbg!(testcase);
            dbg_hex!(bus_trace);
            panic!(
                "Addr {:06X} - expected {:02X}, saw {:02X}",
                addr, exp_val, val
            );
        }
    }

    if !multi_steps && cpu.cycles != test_cycles {
        dbg!(&testcase);
        dbg_hex!(&bus_trace);
        println!("{}", cpu.dump_state());
        panic!("Saw {} cycles, should be {}", cpu.cycles, test_cycles);
    }

    if !check_trace {
        return;
    }

    for trace in &bus_trace {
        let expected = &testcase_cycles[trace.cycle];
        let exp_addr: Address = expected[0].as_u64().unwrap().try_into().unwrap();

        let exp_access = match expected[2].as_str().unwrap().chars().nth(3).unwrap() {
            'r' => Access::Read,
            'w' => Access::Write,
            _ => unreachable!(),
        };

        if trace.addr != exp_addr || trace.access != exp_access {
            dbg!(&testcase);
            dbg_hex!(&bus_trace);
            dbg!(&trace);
            dbg!(&expected);
            panic!("Invalid trace");
        }
        if !expected[1].is_null() {
            let exp_val: u8 = expected[1].as_u64().unwrap().try_into().unwrap();
            if trace.val != exp_val {
                dbg!(&testcase);
                dbg_hex!(&bus_trace);
                dbg!(&trace);
                dbg!(&expected);
                panic!("Invalid trace");
            }
        }
    }
}

cpu_test!(instr_00, 0x00);
cpu_test!(instr_01, 0x01);
cpu_test!(instr_02, 0x02);
cpu_test!(instr_03, 0x03);
cpu_test!(instr_04, 0x04);
cpu_test!(instr_05, 0x05);
cpu_test!(instr_06, 0x06);
cpu_test!(instr_07, 0x07);
cpu_test!(instr_08, 0x08);
cpu_test!(instr_09, 0x09);
cpu_test!(instr_0a, 0x0a);
cpu_test!(instr_0b, 0x0b);
cpu_test!(instr_0c, 0x0c);
cpu_test!(instr_0d, 0x0d);
cpu_test!(instr_0e, 0x0e);
cpu_test!(instr_0f, 0x0f);
cpu_test!(instr_10, 0x10);
cpu_test!(instr_11, 0x11);
cpu_test!(instr_12, 0x12);
cpu_test!(instr_13, 0x13);
cpu_test!(instr_14, 0x14);
cpu_test!(instr_15, 0x15);
cpu_test!(instr_16, 0x16);
cpu_test!(instr_17, 0x17);
cpu_test!(instr_18, 0x18);
cpu_test!(instr_19, 0x19);
cpu_test!(instr_1a, 0x1a);
cpu_test!(instr_1b, 0x1b);
cpu_test!(instr_1c, 0x1c);
cpu_test!(instr_1d, 0x1d);
cpu_test!(instr_1e, 0x1e);
cpu_test!(instr_1f, 0x1f);
cpu_test!(instr_20, 0x20);
cpu_test!(instr_21, 0x21);
// Inaccuracy: JSL fetches the last byte later; fetcher cannot do that.
cpu_test_no_trace!(instr_22, 0x22);
cpu_test!(instr_23, 0x23);
cpu_test!(instr_24, 0x24);
cpu_test!(instr_25, 0x25);
cpu_test!(instr_26, 0x26);
cpu_test!(instr_27, 0x27);
cpu_test!(instr_28, 0x28);
cpu_test!(instr_29, 0x29);
cpu_test!(instr_2a, 0x2a);
cpu_test!(instr_2b, 0x2b);
cpu_test!(instr_2c, 0x2c);
cpu_test!(instr_2d, 0x2d);
cpu_test!(instr_2e, 0x2e);
cpu_test!(instr_2f, 0x2f);
cpu_test!(instr_30, 0x30);
cpu_test!(instr_31, 0x31);
cpu_test!(instr_32, 0x32);
cpu_test!(instr_33, 0x33);
cpu_test!(instr_34, 0x34);
cpu_test!(instr_35, 0x35);
cpu_test!(instr_36, 0x36);
cpu_test!(instr_37, 0x37);
cpu_test!(instr_38, 0x38);
cpu_test!(instr_39, 0x39);
cpu_test!(instr_3a, 0x3a);
cpu_test!(instr_3b, 0x3b);
cpu_test!(instr_3c, 0x3c);
cpu_test!(instr_3d, 0x3d);
cpu_test!(instr_3e, 0x3e);
cpu_test!(instr_3f, 0x3f);
cpu_test!(instr_40, 0x40);
cpu_test!(instr_41, 0x41);
cpu_test!(instr_42, 0x42);
cpu_test!(instr_43, 0x43);
cpu_test_steps!(instr_44, 0x44);
cpu_test!(instr_45, 0x45);
cpu_test!(instr_46, 0x46);
cpu_test!(instr_47, 0x47);
cpu_test!(instr_48, 0x48);
cpu_test!(instr_49, 0x49);
cpu_test!(instr_4a, 0x4a);
cpu_test!(instr_4b, 0x4b);
cpu_test!(instr_4c, 0x4c);
cpu_test!(instr_4d, 0x4d);
cpu_test!(instr_4e, 0x4e);
cpu_test!(instr_4f, 0x4f);
cpu_test!(instr_50, 0x50);
cpu_test!(instr_51, 0x51);
cpu_test!(instr_52, 0x52);
cpu_test!(instr_53, 0x53);
cpu_test_steps!(instr_54, 0x54);
cpu_test!(instr_55, 0x55);
cpu_test!(instr_56, 0x56);
cpu_test!(instr_57, 0x57);
cpu_test!(instr_58, 0x58);
cpu_test!(instr_59, 0x59);
cpu_test!(instr_5a, 0x5a);
cpu_test!(instr_5b, 0x5b);
cpu_test!(instr_5c, 0x5c);
cpu_test!(instr_5d, 0x5d);
cpu_test!(instr_5e, 0x5e);
cpu_test!(instr_5f, 0x5f);
cpu_test!(instr_60, 0x60);
cpu_test!(instr_61, 0x61);
cpu_test!(instr_62, 0x62);
cpu_test!(instr_63, 0x63);
cpu_test!(instr_64, 0x64);
cpu_test!(instr_65, 0x65);
cpu_test!(instr_66, 0x66);
cpu_test!(instr_67, 0x67);
cpu_test!(instr_68, 0x68);
cpu_test!(instr_69, 0x69);
cpu_test!(instr_6a, 0x6a);
cpu_test!(instr_6b, 0x6b);
cpu_test!(instr_6c, 0x6c);
cpu_test!(instr_6d, 0x6d);
cpu_test!(instr_6e, 0x6e);
cpu_test!(instr_6f, 0x6f);
cpu_test!(instr_70, 0x70);
cpu_test!(instr_71, 0x71);
cpu_test!(instr_72, 0x72);
cpu_test!(instr_73, 0x73);
cpu_test!(instr_74, 0x74);
cpu_test!(instr_75, 0x75);
cpu_test!(instr_76, 0x76);
cpu_test!(instr_77, 0x77);
cpu_test!(instr_78, 0x78);
cpu_test!(instr_79, 0x79);
cpu_test!(instr_7a, 0x7a);
cpu_test!(instr_7b, 0x7b);
cpu_test!(instr_7c, 0x7c);
cpu_test!(instr_7d, 0x7d);
cpu_test!(instr_7e, 0x7e);
cpu_test!(instr_7f, 0x7f);
cpu_test!(instr_80, 0x80);
cpu_test!(instr_81, 0x81);
cpu_test!(instr_82, 0x82);
cpu_test!(instr_83, 0x83);
cpu_test!(instr_84, 0x84);
cpu_test!(instr_85, 0x85);
cpu_test!(instr_86, 0x86);
cpu_test!(instr_87, 0x87);
cpu_test!(instr_88, 0x88);
cpu_test!(instr_89, 0x89);
cpu_test!(instr_8a, 0x8a);
cpu_test!(instr_8b, 0x8b);
cpu_test!(instr_8c, 0x8c);
cpu_test!(instr_8d, 0x8d);
cpu_test!(instr_8e, 0x8e);
cpu_test!(instr_8f, 0x8f);
cpu_test!(instr_90, 0x90);
cpu_test!(instr_91, 0x91);
cpu_test!(instr_92, 0x92);
cpu_test!(instr_93, 0x93);
cpu_test!(instr_94, 0x94);
cpu_test!(instr_95, 0x95);
cpu_test!(instr_96, 0x96);
cpu_test!(instr_97, 0x97);
cpu_test!(instr_98, 0x98);
cpu_test!(instr_99, 0x99);
cpu_test!(instr_9a, 0x9a);
cpu_test!(instr_9b, 0x9b);
cpu_test!(instr_9c, 0x9c);
cpu_test!(instr_9d, 0x9d);
cpu_test!(instr_9e, 0x9e);
cpu_test!(instr_9f, 0x9f);
cpu_test!(instr_a0, 0xa0);
cpu_test!(instr_a1, 0xa1);
cpu_test!(instr_a2, 0xa2);
cpu_test!(instr_a3, 0xa3);
cpu_test!(instr_a4, 0xa4);
cpu_test!(instr_a5, 0xa5);
cpu_test!(instr_a6, 0xa6);
cpu_test!(instr_a7, 0xa7);
cpu_test!(instr_a8, 0xa8);
cpu_test!(instr_a9, 0xa9);
cpu_test!(instr_aa, 0xaa);
cpu_test!(instr_ab, 0xab);
cpu_test!(instr_ac, 0xac);
cpu_test!(instr_ad, 0xad);
cpu_test!(instr_ae, 0xae);
cpu_test!(instr_af, 0xaf);
cpu_test!(instr_b0, 0xb0);
cpu_test!(instr_b1, 0xb1);
cpu_test!(instr_b2, 0xb2);
cpu_test!(instr_b3, 0xb3);
cpu_test!(instr_b4, 0xb4);
cpu_test!(instr_b5, 0xb5);
cpu_test!(instr_b6, 0xb6);
cpu_test!(instr_b7, 0xb7);
cpu_test!(instr_b8, 0xb8);
cpu_test!(instr_b9, 0xb9);
cpu_test!(instr_ba, 0xba);
cpu_test!(instr_bb, 0xbb);
cpu_test!(instr_bc, 0xbc);
cpu_test!(instr_bd, 0xbd);
cpu_test!(instr_be, 0xbe);
cpu_test!(instr_bf, 0xbf);
cpu_test!(instr_c0, 0xc0);
cpu_test!(instr_c1, 0xc1);
cpu_test!(instr_c2, 0xc2);
cpu_test!(instr_c3, 0xc3);
cpu_test!(instr_c4, 0xc4);
cpu_test!(instr_c5, 0xc5);
cpu_test!(instr_c6, 0xc6);
cpu_test!(instr_c7, 0xc7);
cpu_test!(instr_c8, 0xc8);
cpu_test!(instr_c9, 0xc9);
cpu_test!(instr_ca, 0xca);
//cpu_test!(instr_cb, 0xcb);
cpu_test!(instr_cc, 0xcc);
cpu_test!(instr_cd, 0xcd);
cpu_test!(instr_ce, 0xce);
cpu_test!(instr_cf, 0xcf);
cpu_test!(instr_d0, 0xd0);
cpu_test!(instr_d1, 0xd1);
cpu_test!(instr_d2, 0xd2);
cpu_test!(instr_d3, 0xd3);
cpu_test!(instr_d4, 0xd4);
cpu_test!(instr_d5, 0xd5);
cpu_test!(instr_d6, 0xd6);
cpu_test!(instr_d7, 0xd7);
cpu_test!(instr_d8, 0xd8);
cpu_test!(instr_d9, 0xd9);
cpu_test!(instr_da, 0xda);
//cpu_test!(instr_db, 0xdb);
cpu_test!(instr_dc, 0xdc);
cpu_test!(instr_dd, 0xdd);
cpu_test!(instr_de, 0xde);
cpu_test!(instr_df, 0xdf);
cpu_test!(instr_e0, 0xe0);
cpu_test!(instr_e1, 0xe1);
cpu_test!(instr_e2, 0xe2);
cpu_test!(instr_e3, 0xe3);
cpu_test!(instr_e4, 0xe4);
cpu_test!(instr_e5, 0xe5);
cpu_test!(instr_e6, 0xe6);
cpu_test!(instr_e7, 0xe7);
cpu_test!(instr_e8, 0xe8);
cpu_test!(instr_e9, 0xe9);
cpu_test!(instr_ea, 0xea);
cpu_test!(instr_eb, 0xeb);
cpu_test!(instr_ec, 0xec);
cpu_test!(instr_ed, 0xed);
cpu_test!(instr_ee, 0xee);
cpu_test!(instr_ef, 0xef);
cpu_test!(instr_f0, 0xf0);
cpu_test!(instr_f1, 0xf1);
cpu_test!(instr_f2, 0xf2);
cpu_test!(instr_f3, 0xf3);
cpu_test!(instr_f4, 0xf4);
cpu_test!(instr_f5, 0xf5);
cpu_test!(instr_f6, 0xf6);
cpu_test!(instr_f7, 0xf7);
cpu_test!(instr_f8, 0xf8);
cpu_test!(instr_f9, 0xf9);
cpu_test!(instr_fa, 0xfa);
cpu_test!(instr_fb, 0xfb);
// Inaccuracy: JSR fetches the last byte later; fetcher cannot do that.
cpu_test_no_trace!(instr_fc, 0xfc);
cpu_test!(instr_fd, 0xfd);
cpu_test!(instr_fe, 0xfe);
cpu_test!(instr_ff, 0xff);
