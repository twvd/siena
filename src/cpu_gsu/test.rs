use super::cpu::CpuGsu;
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
const IWT: u8 = 0xF0;

fn cpu(code: &[u8]) -> CpuGsu {
    let c = CpuGsu::new(code);
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
        assert!(c.regs.test_flag(Flag::B));

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
}
