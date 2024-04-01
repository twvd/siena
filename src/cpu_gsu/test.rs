use super::cpu::{CpuGsu, GsuMap};
use super::regs::{Flag, Register};

const STOP: u8 = 0x00;
const NOP: u8 = 0x01;
const ALT1: u8 = 0x3D;
const ALT2: u8 = 0x3E;
const ALT3: u8 = 0x3F;
const BRA: u8 = 0x05;
const TO: u8 = 0x10;
const WITH: u8 = 0x20;
const FROM: u8 = 0xB0;
const IBT: u8 = 0xA0;
const IWT: u8 = 0xF0;
const LDW: u8 = 0x40; // 0..11
const LINK: u8 = 0x90;
const STW: u8 = 0x30; // 0..11
const STB: u8 = 0x30; // 0..11
const LM: u8 = 0xF0;
const LMS: u8 = 0xA0;
const SM: u8 = 0xF0;
const SMS: u8 = 0xA0;
const SBK: u8 = 0x90;
const ROMB: u8 = 0xDF;
const GETB: u8 = 0xEF;

fn cpu(code: &[u8]) -> CpuGsu {
    let mut c = CpuGsu::new(code, GsuMap::SuperFX1, usize::MAX);
    c.rom_mask = usize::MAX;
    c
}

fn cpu_ram(code: &[u8], ram: &[(usize, u8)]) -> CpuGsu {
    let mut c = cpu(code);
    for (addr, val) in ram {
        c.ram[*addr] = *val;
    }
    c
}

fn cpu_ram_steps(code: &[u8], ram: &[(usize, u8)], steps: usize) -> CpuGsu {
    let mut c = cpu_ram(code, ram);
    c.regs.write_flags(&[(Flag::G, true)]);
    for _ in 0..steps {
        c.step().unwrap();
    }
    c
}

fn cpu_ram_reg_steps(
    code: &[u8],
    ram: &[(usize, u8)],
    regs: &[(Register, u16)],
    steps: usize,
) -> CpuGsu {
    let mut c = cpu_ram(code, ram);
    for (reg, val) in regs {
        c.regs.write(*reg, *val);
    }
    c.regs.write_flags(&[(Flag::G, true)]);
    for _ in 0..steps {
        c.step().unwrap();
    }
    c
}

fn cpu_steps(code: &[u8], steps: usize) -> CpuGsu {
    let mut c = cpu(code);
    c.regs.write_flags(&[(Flag::G, true)]);
    for _ in 0..steps {
        c.step().unwrap();
    }
    c
}

fn cpu_run(code: &[u8]) -> CpuGsu {
    let mut c = cpu(code);
    c.regs.write_flags(&[(Flag::G, true)]);
    while c.regs.test_flag(Flag::G) {
        c.step().unwrap();
    }
    c
}

#[test]
fn retain_alt() {
    let t = |op| {
        let c = cpu_steps(&[NOP, op, 5], 2);
        assert_ne!(c.regs.read(Register::R15), 0);
        assert!(!c.regs.test_flag(Flag::ALT1));
        assert!(!c.regs.test_flag(Flag::ALT2));
        let c = cpu_steps(&[ALT1, op, 5, NOP], 2);
        assert_ne!(c.regs.read(Register::R15), 0);
        assert!(c.regs.test_flag(Flag::ALT1));
        assert!(!c.regs.test_flag(Flag::ALT2));
        let c = cpu_steps(&[ALT2, op, 5, NOP], 2);
        assert_ne!(c.regs.read(Register::R15), 0);
        assert!(!c.regs.test_flag(Flag::ALT1));
        assert!(c.regs.test_flag(Flag::ALT2));
        let c = cpu_steps(&[ALT3, op, 5, NOP], 2);
        assert_ne!(c.regs.read(Register::R15), 0);
        assert!(c.regs.test_flag(Flag::ALT1));
        assert!(c.regs.test_flag(Flag::ALT2));

        let c = cpu_steps(&[ALT2, op, NOP, NOP], 3);
        assert_ne!(c.regs.read(Register::R15), 0);
        assert!(!c.regs.test_flag(Flag::ALT1));
        assert!(!c.regs.test_flag(Flag::ALT2));
    };

    t(BRA);
    t(0x06); // BGE
    t(0x07); // BLT
    t(0x08); // BNE
    t(0x09); // BEQ
    t(0x0A); // BPL
    t(0x0B); // BMI
    t(0x0C); // BCC
    t(0x0D); // BCS
    t(0x0E); // BVC
    t(0x0F); // BVS
    t(WITH | 2);
    t(FROM | 2);
    t(TO | 2);
}

#[test]
fn retain_sdreg() {
    let t = |op| {
        let c = cpu_steps(&[WITH | 2, op, 5], 2);
        assert_ne!(c.regs.read(Register::R15), 0);
        assert_eq!(c.sreg, 2);
        assert_eq!(c.dreg, 2);
        //assert!(c.regs.test_flag(Flag::B));

        let c = cpu_steps(&[FROM | 2, op, 5], 2);
        assert_ne!(c.regs.read(Register::R15), 0);
        assert_eq!(c.sreg, 2);
        assert_eq!(c.dreg, 0);
        assert!(!c.regs.test_flag(Flag::B));

        let c = cpu_steps(&[TO | 2, op, 5], 2);
        assert_ne!(c.regs.read(Register::R15), 0);
        assert_eq!(c.sreg, 0);
        assert_eq!(c.dreg, 2);
        assert!(!c.regs.test_flag(Flag::B));
    };

    t(ALT1);
    t(ALT2);
    t(ALT3);
    t(BRA);
    t(0x06); // BGE
    t(0x07); // BLT
    t(0x08); // BNE
    t(0x09); // BEQ
    t(0x0A); // BPL
    t(0x0B); // BMI
    t(0x0C); // BCC
    t(0x0D); // BCS
    t(0x0E); // BVC
    t(0x0F); // BVS
}

#[test]
fn op_move() {
    let c = cpu_run(&[IWT | 2, 0xBB, 0xAA, WITH | 2, TO | 3, STOP]);
    assert_eq!(c.regs.read(Register::R2), 0xAABB);
    assert_eq!(c.regs.read(Register::R3), 0xAABB);
    assert!(!c.regs.test_flag(Flag::B));
}

#[test]
fn op_ldw() {
    let c = cpu_ram_steps(
        &[IWT | 4, 0x22, 0x11, TO | 3, LDW | 4],
        &[(0x1122, 0xBB), (0x1123, 0xAA)],
        3,
    );
    assert_eq!(c.regs.read(Register::R3), 0xAABB);

    let c = cpu_ram_steps(
        &[IWT | 4, 0x23, 0x11, TO | 3, LDW | 4],
        &[(0x1122, 0xBB), (0x1123, 0xAA)],
        3,
    );
    assert_eq!(c.regs.read(Register::R3), 0xBBAA);
}

#[test]
fn op_lm() {
    let c = cpu_ram_steps(
        &[ALT1, LM | 4, 0x22, 0x11],
        &[(0x1122, 0xBB), (0x1123, 0xAA)],
        2,
    );
    assert_eq!(c.regs.read(Register::R4), 0xAABB);

    let c = cpu_ram_steps(
        &[ALT1, LM | 4, 0x23, 0x11],
        &[(0x1122, 0xBB), (0x1123, 0xAA)],
        2,
    );
    assert_eq!(c.regs.read(Register::R4), 0xBBAA);
}

#[test]
fn op_lms() {
    let c = cpu_ram_steps(&[ALT1, LMS | 3, 0x08], &[(0x10, 0xBB), (0x11, 0xAA)], 2);
    assert_eq!(c.regs.read(Register::R3), 0xAABB);
}

#[test]
fn op_ldb() {
    let c = cpu_ram_reg_steps(
        &[IWT | 4, 0x22, 0x11, TO | 3, ALT1, LDW | 4],
        &[(0x1122, 0xBB), (0x1123, 0xAA)],
        &[(Register::R3, 0xFFFF)],
        4,
    );
    assert_eq!(c.regs.read(Register::R3), 0x00BB);

    let c = cpu_ram_reg_steps(
        &[IWT | 4, 0x23, 0x11, ALT1, TO | 3, LDW | 4],
        &[(0x1122, 0xBB), (0x1123, 0xAA)],
        &[(Register::R3, 0xFFFF)],
        4,
    );
    assert_eq!(c.regs.read(Register::R3), 0xAA);
}

#[test]
fn op_stw() {
    let c = cpu_steps(
        &[IWT | 4, 0x22, 0x11, IWT | 5, 0xBB, 0xAA, FROM | 5, STW | 4],
        4,
    );
    assert_eq!(c.ram[0x1122], 0xBB);
    assert_eq!(c.ram[0x1123], 0xAA);

    let c = cpu_steps(
        &[IWT | 4, 0x23, 0x11, IWT | 5, 0xBB, 0xAA, FROM | 5, STW | 4],
        4,
    );
    assert_eq!(c.ram[0x1122], 0xAA);
    assert_eq!(c.ram[0x1123], 0xBB);
}

#[test]
fn op_sm() {
    let c = cpu_steps(&[IWT | 5, 0xBB, 0xAA, ALT2, SM | 5, 0x22, 0x11], 3);
    assert_eq!(c.ram[0x1122], 0xBB);
    assert_eq!(c.ram[0x1123], 0xAA);

    let c = cpu_steps(&[IWT | 5, 0xBB, 0xAA, ALT2, SM | 5, 0x23, 0x11], 3);
    assert_eq!(c.ram[0x1122], 0xAA);
    assert_eq!(c.ram[0x1123], 0xBB);
}

#[test]
fn op_sms() {
    let c = cpu_steps(&[IWT | 4, 0xBB, 0xAA, ALT2, SMS | 4, 0x08], 4);
    assert_eq!(c.ram[0x10], 0xBB);
    assert_eq!(c.ram[0x11], 0xAA);
}

#[test]
fn op_sbk() {
    let c = cpu_ram_steps(
        &[
            IWT | 4,
            0x22,
            0x11,
            IWT | 5,
            0xDD,
            0xCC,
            TO | 3,
            LDW | 4,
            FROM | 5,
            SBK,
        ],
        &[(0x1122, 0xBB), (0x1123, 0xAA)],
        6,
    );
    assert_eq!(c.regs.read(Register::R3), 0xAABB);
    assert_eq!(c.ram[0x1122], 0xDD);
    assert_eq!(c.ram[0x1123], 0xCC);

    let c = cpu_ram_steps(
        &[
            IWT | 4,
            0x23,
            0x11,
            IWT | 5,
            0xDD,
            0xCC,
            TO | 3,
            LDW | 4,
            FROM | 5,
            SBK,
        ],
        &[(0x1122, 0xBB), (0x1123, 0xAA)],
        6,
    );
    assert_eq!(c.regs.read(Register::R3), 0xBBAA);
    assert_eq!(c.ram[0x1122], 0xCC);
    assert_eq!(c.ram[0x1123], 0xDD);
}

#[test]
fn op_stb() {
    let c = cpu_steps(
        &[
            IWT | 4,
            0x22,
            0x11,
            IWT | 5,
            0xBB,
            0xAA,
            FROM | 5,
            ALT1,
            STB | 4,
        ],
        5,
    );
    assert_eq!(c.ram[0x1122], 0xBB);
    assert_eq!(c.ram[0x1123], 0xFF);
}

#[test]
fn op_link() {
    let c = cpu_steps(&[LINK | 1], 1);
    assert_eq!(c.regs.read(Register::R11), 0x02);
    let c = cpu_steps(&[LINK | 4], 1);
    assert_eq!(c.regs.read(Register::R11), 0x05);
}

#[test]
fn rom_buffer() {
    let prog = [
        // R1 = ROM bank 1
        IBT | 1,
        0x41,
        // R2 = ROM bank 2
        IBT | 2,
        0x42,
        // ROMBR = 0x41
        FROM | 1,
        ALT3,
        ROMB,
        // R14 = 0x01
        IBT | 14,
        0x01,
        // R3 = result 1 (should be 0x10, because we wrote R14)
        TO | 3,
        GETB,
        // ROMBR = 0x42
        FROM | 2,
        ALT3,
        ROMB,
        // R4 = result 2 (still 0x10, R14 not written yet)
        TO | 4,
        GETB,
        // R14 = 0x01
        IBT | 14,
        0x01,
        // R5 = result 3 (now 0x20)
        TO | 5,
        GETB,
        STOP,
    ];
    let mut code = vec![0; 0x2FFFF];
    code[0..prog.len()].copy_from_slice(&prog);
    code[0x10001] = 0x10;
    code[0x20001] = 0x20;
    let c = cpu_run(&code);

    assert_eq!(c.regs.read(Register::R3), 0x10);
    assert_eq!(c.regs.read(Register::R4), 0x10);
    assert_eq!(c.regs.read(Register::R5), 0x20);
}

#[test]
fn delay_slot_mb_op() {
    let c = cpu_run(&[
        BRA,
        0x04,
        // delay slot, which should be one fetch
        IWT | 1,
        // Below should not happen
        IWT | 2,
        0x11,
        0x22,
        // fetching continues here
        0x01,
        0x02,
        STOP,
    ]);
    assert_eq!(c.regs.read(Register::R1), 0x0201);
    assert_eq!(c.regs.read(Register::R2), 0);
}
