use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::tickable::Ticks;

use super::instruction::*;
use super::regs::{Flag, Flags, Register, RegisterFile, SR};

const SR_MASK: u16 = 0x907C;

/// NEC uPD77C25
#[derive(Serialize, Deserialize)]
pub struct CpuUpd77c25 {
    pub regs: RegisterFile,
    pub rodata: Vec<u16>,
    pub code: Vec<u8>,
    pub ram: Vec<u16>,
    pub stack: Vec<u16>,

    pub verbose: bool,
}

impl CpuUpd77c25 {
    pub fn new() -> Self {
        Self {
            regs: RegisterFile::from_pc(0),
            code: vec![0xFF; 16 * 1024],
            rodata: vec![0; 1024],
            ram: vec![0; 256],
            stack: vec![0; 16],

            verbose: false,
        }
    }

    pub fn load_rom(&mut self, code: &[u8], rodata: &[u8]) {
        assert_eq!(self.regs.pc, 0);

        self.code[0..code.len()].copy_from_slice(code);
        self.rodata = Vec::from_iter(
            rodata
                .chunks_exact(2)
                .map(|s| u16::from_le_bytes(s.try_into().unwrap())),
        );
    }

    pub fn load_rom_combined(&mut self, rom: &[u8]) {
        assert_eq!(rom.len(), 8192);
        self.load_rom(&rom[0..6144], &rom[6144..]);
    }

    pub fn dump_state(&self) -> String {
        format!("{}\n --> {}", self.regs, self.peek_next_instr().unwrap())
    }

    /// Fetches and decodes the next instruction at PC
    pub fn peek_next_instr(&self) -> Result<Instruction> {
        // No read side-effects on this CPU so just use the same code
        // as fetch.
        self.fetch_next_instr()
    }

    /// Fetches and decodes the next instruction at PC
    pub fn fetch_next_instr(&self) -> Result<Instruction> {
        let pc8b = self.regs.read_pc_8b();
        Instruction::decode(&mut self.code[pc8b..].iter().copied())
    }

    /// Push 16-bits onto the stack
    fn push(&mut self, val: u16) {
        let sp = self.regs.read(Register::SP);
        self.stack[sp as usize] = val;
        self.regs.write(Register::SP, sp + 1);
    }

    /// Pop 16-bits from the stack
    fn pop(&mut self) -> u16 {
        let sp = self.regs.read(Register::SP).checked_sub(1).unwrap();
        let val = self.stack[sp as usize];
        self.regs.write(Register::SP, sp);
        val
    }

    /// Executes one CPU step (one instruction).
    pub fn step(&mut self) -> Result<Ticks> {
        let instr = self.fetch_next_instr()?;

        if self.verbose {
            println!("{}", self.dump_state());
        }

        self.regs.write(Register::PC, self.regs.pc.wrapping_add(1));
        self.execute_instruction(&instr)?;
        self.execute_mul();

        Ok(1)
    }

    /// Executes the multiplication at occurs after every instruction.
    fn execute_mul(&mut self) {
        let k = (self.regs.read(Register::K) as i16) as i32;
        let l = (self.regs.read(Register::L) as i16) as i32;
        let result = k * l;
        self.regs.write(Register::M, (result >> 15) as u16);
        self.regs.write(Register::N, (result << 1) as u16);
    }

    fn write_to_dst(&mut self, dst: DST, val: u16) {
        match dst {
            DST::NON => (),
            DST::ACCA => self.regs.write(Register::ACCA, val),
            DST::ACCB => self.regs.write(Register::ACCB, val),
            DST::TR => self.regs.write(Register::TR, val),
            DST::DP => self.regs.write(Register::DP, val),
            DST::RP => self.regs.write(Register::RP, val),
            DST::DR => {
                self.regs.write_sr(&[(SR::RQM, true)]);
                self.regs.write(Register::DR, val)
            }
            DST::SR => {
                let sr = self.regs.read(Register::SR);
                self.regs
                    .write(Register::SR, (sr & SR_MASK) | (val & !SR_MASK))
            }
            DST::SIM => todo!(),
            DST::SIL => todo!(),
            DST::K => self.regs.write(Register::K, val),
            DST::KLR => {
                let rp = self.regs.read(Register::RP);
                self.regs.write(Register::K, val);
                self.regs.write(Register::L, self.rodata[rp as usize]);
            }
            DST::KLM => {
                let dp = self.regs.read(Register::DP);
                self.regs.write(Register::L, val);
                self.regs
                    .write(Register::K, self.ram[(dp | (1 << 6)) as usize]);
            }
            DST::L => self.regs.write(Register::L, val),
            DST::TRB => self.regs.write(Register::TRB, val),
            DST::MEM => self.write_to_ram(val),
        }
    }

    fn read_from_src(&mut self, src: SRC) -> u16 {
        match src {
            SRC::TRB => self.regs.read(Register::TRB),
            SRC::ACCA => self.regs.read(Register::ACCA),
            SRC::ACCB => self.regs.read(Register::ACCB),
            SRC::TR => self.regs.read(Register::TR),
            SRC::DP => self.regs.read(Register::DP),
            SRC::RP => self.regs.read(Register::RP),
            SRC::RO => {
                let rp = self.regs.read(Register::RP);
                self.rodata[rp as usize]
            }
            SRC::SGN => self.regs.read(Register::SGN),
            SRC::DR => {
                self.regs.write_sr(&[(SR::RQM, true)]);
                self.regs.read(Register::DR)
            }
            SRC::DRNF => self.regs.read(Register::DR),
            SRC::SR => self.regs.read(Register::SR),
            SRC::SIM => todo!(),
            SRC::SIL => todo!(),
            SRC::K => self.regs.read(Register::K),
            SRC::L => self.regs.read(Register::L),
            SRC::MEM => self.read_from_ram(),
        }
    }

    fn read_from_ram(&self) -> u16 {
        let dp = self.regs.read(Register::DP);
        self.ram[dp as usize]
    }

    fn write_to_ram(&mut self, val: u16) {
        let dp = self.regs.read(Register::DP);
        self.ram[dp as usize] = val;
    }

    /// Executes an instruction.
    fn execute_instruction(&mut self, instr: &Instruction) -> Result<()> {
        match instr {
            Instruction::Jp(i) if i.branch_cond() == BrchCnd::CALL => {
                // CALL
                self.push(self.regs.read(Register::PC));
                self.regs.write(Register::PC, i.next_address());
                Ok(())
            }
            Instruction::Jp(i) => self.op_jp(i),
            Instruction::Ld(i) => self.op_ld(i),
            Instruction::Rt(i) => {
                self.op_op(i)?;
                let pc = self.pop();
                self.regs.write(Register::PC, pc);
                Ok(())
            }
            Instruction::Op(i) => self.op_op(i),
        }
    }

    /// JP*
    fn op_jp(&mut self, instr: &InstructionJp) -> Result<()> {
        let cond = match instr.branch_cond() {
            BrchCnd::JMP => true,
            BrchCnd::CALL => unreachable!(),

            BrchCnd::JNCA => !self.regs.test_flag(Flags::A, Flag::C),
            BrchCnd::JCA => self.regs.test_flag(Flags::A, Flag::C),
            BrchCnd::JNCB => !self.regs.test_flag(Flags::B, Flag::C),
            BrchCnd::JCB => self.regs.test_flag(Flags::B, Flag::C),
            BrchCnd::JNZA => !self.regs.test_flag(Flags::A, Flag::Z),
            BrchCnd::JZA => self.regs.test_flag(Flags::A, Flag::Z),
            BrchCnd::JNZB => !self.regs.test_flag(Flags::B, Flag::Z),
            BrchCnd::JZB => self.regs.test_flag(Flags::B, Flag::Z),
            BrchCnd::JNOVA0 => !self.regs.test_flag(Flags::A, Flag::OV0),
            BrchCnd::JOVA0 => self.regs.test_flag(Flags::A, Flag::OV0),
            BrchCnd::JNOVB0 => !self.regs.test_flag(Flags::B, Flag::OV0),
            BrchCnd::JOVB0 => self.regs.test_flag(Flags::B, Flag::OV0),
            BrchCnd::JNOVA1 => !self.regs.test_flag(Flags::A, Flag::OV1),
            BrchCnd::JOVA1 => self.regs.test_flag(Flags::A, Flag::OV1),
            BrchCnd::JNOVB1 => !self.regs.test_flag(Flags::B, Flag::OV1),
            BrchCnd::JOVB1 => self.regs.test_flag(Flags::B, Flag::OV1),
            BrchCnd::JNSA0 => !self.regs.test_flag(Flags::A, Flag::S0),
            BrchCnd::JSA0 => self.regs.test_flag(Flags::A, Flag::S0),
            BrchCnd::JNSB0 => !self.regs.test_flag(Flags::B, Flag::S0),
            BrchCnd::JSB0 => self.regs.test_flag(Flags::B, Flag::S0),
            BrchCnd::JNSA1 => !self.regs.test_flag(Flags::A, Flag::S1),
            BrchCnd::JSA1 => self.regs.test_flag(Flags::A, Flag::S1),
            BrchCnd::JNSB1 => !self.regs.test_flag(Flags::B, Flag::S1),
            BrchCnd::JSB1 => self.regs.test_flag(Flags::B, Flag::S1),
            BrchCnd::JDPL0 => self.regs.read(Register::DP) & 0x0F == 0,
            BrchCnd::JDPLN0 => self.regs.read(Register::DP) & 0x0F != 0,
            BrchCnd::JDPLF => self.regs.read(Register::DP) & 0x0F == 0x0F,
            BrchCnd::JDPLNF => self.regs.read(Register::DP) & 0x0F != 0x0F,

            // Serial stuff, not used on SNES
            BrchCnd::JNSIAK => todo!(),
            BrchCnd::JSIAK => todo!(),
            BrchCnd::JNSOAK => todo!(),
            BrchCnd::JSOAK => todo!(),

            BrchCnd::JNRQM => !self.regs.test_sr(SR::RQM),
            BrchCnd::JRQM => self.regs.test_sr(SR::RQM),
        };

        if cond {
            // Branch taken
            self.regs.write(Register::PC, instr.next_address());
        }

        Ok(())
    }

    fn op_ld(&mut self, instr: &InstructionLd) -> Result<()> {
        self.write_to_dst(instr.dst(), instr.imm16());
        Ok(())
    }

    /// (ALU) ops
    fn op_op(&mut self, instr: &InstructionOpRt) -> Result<()> {
        let ld_val = self.read_from_src(instr.src());

        // ALU stage
        if instr.alu() != AluFunction::Nop {
            self.op_op_alu(instr)?;
        }

        // Load stage
        self.write_to_dst(instr.dst(), ld_val);

        // Update DP
        if instr.dst() != DST::DP {
            // DP action
            let dp = self.regs.read(Register::DP);
            let dplow = match instr.dpl() {
                DPL::DPINC => dp.wrapping_add(1),
                DPL::DPDEC => dp.wrapping_sub(1),
                DPL::DPNOP => dp,
                DPL::DPCLR => 0,
            } & 0x0F;
            let dphigh = (dp ^ (instr.dphm() << 4)) & 0xF0;
            self.regs.write(Register::DP, dplow | dphigh);
        }

        // Update RP
        if instr.dst() != DST::RP && instr.rpdcr() {
            let rp = self.regs.read(Register::RP);
            self.regs.write(Register::RP, rp.wrapping_sub(1));
        }

        Ok(())
    }

    fn op_op_alu(&mut self, instr: &InstructionOpRt) -> Result<()> {
        // First operand
        let (a_reg, a_flags, carry_flags) = match instr.asl() {
            ASL::ACCA => (Register::ACCA, Flags::A, Flags::B),
            ASL::ACCB => (Register::ACCB, Flags::B, Flags::A),
        };
        let a = self.regs.read(a_reg);

        // Second operand
        let b = match instr.pselect() {
            PSelect::RAM => self.read_from_ram(),
            PSelect::IDB => self.read_from_src(instr.src()),
            PSelect::M => self.regs.read(Register::M),
            PSelect::N => self.regs.read(Register::N),
        };

        // Change INC/DEC to ADD/SUB of 1 for ease
        let b = match instr.alu() {
            AluFunction::Inc | AluFunction::Dec => 1,
            _ => b,
        };

        let carry = if self.regs.test_flag(carry_flags, Flag::C) {
            1_u16
        } else {
            0_u16
        };

        let c = match instr.alu() {
            AluFunction::Nop => unreachable!(),
            AluFunction::Or => a | b,
            AluFunction::And => a & b,
            AluFunction::Xor => a ^ b,
            AluFunction::Sub | AluFunction::Dec => a.wrapping_sub(b),
            AluFunction::Add | AluFunction::Inc => a.wrapping_add(b),
            AluFunction::Sbr => a.wrapping_sub(b).wrapping_sub(carry),
            AluFunction::Adc => a.wrapping_add(b).wrapping_add(carry),
            AluFunction::Cmp => !a,
            AluFunction::Shr1 => a >> 1 | a & 0x8000,
            AluFunction::Shl1 => a << 1 | carry,
            AluFunction::Shl2 => a << 2 | 0x03,
            AluFunction::Shl4 => a << 4 | 0x0F,
            AluFunction::Xchg => a << 8 | a >> 8,
        };

        // Z, S0, S1 flags apply the same to all operations
        self.regs
            .write_flags(a_flags, &[(Flag::Z, c == 0), (Flag::S0, c & 0x8000 != 0)]);
        if !self.regs.test_flag(a_flags, Flag::OV1) {
            self.regs.write_flags(
                a_flags,
                &[(Flag::S1, self.regs.test_flag(a_flags, Flag::S0))],
            );
        }

        self.regs.write(a_reg, c);

        match instr.alu() {
            AluFunction::Nop => unreachable!(),
            AluFunction::And
            | AluFunction::Cmp
            | AluFunction::Or
            | AluFunction::Xor
            | AluFunction::Shl2
            | AluFunction::Shl4
            | AluFunction::Xchg => self.regs.write_flags(
                a_flags,
                &[(Flag::C, false), (Flag::OV0, false), (Flag::OV1, false)],
            ),
            AluFunction::Sub | AluFunction::Sbr | AluFunction::Dec => {
                let fcarry = a ^ b ^ c;
                let foverflow = (a ^ c) & (b ^ a);
                let ov0 = foverflow & 0x8000 != 0;
                let ov1 = self.regs.test_flag(a_flags, Flag::OV1);
                let ov1 = if ov0 && ov1 {
                    self.regs.test_flag(a_flags, Flag::S0) == self.regs.test_flag(a_flags, Flag::S1)
                } else {
                    ov0 || ov1
                };
                self.regs.write_flags(
                    a_flags,
                    &[
                        (Flag::C, (fcarry ^ foverflow) & 0x8000 != 0),
                        (Flag::OV0, ov0),
                        (Flag::OV1, ov1),
                    ],
                )
            }
            AluFunction::Add | AluFunction::Adc | AluFunction::Inc => {
                let fcarry = a ^ b ^ c;
                let foverflow = (a ^ c) & (b ^ c);
                let ov0 = foverflow & 0x8000 != 0;
                let ov1 = self.regs.test_flag(a_flags, Flag::OV1);
                let ov1 = if ov0 && ov1 {
                    self.regs.test_flag(a_flags, Flag::S0) == self.regs.test_flag(a_flags, Flag::S1)
                } else {
                    ov0 || ov1
                };
                self.regs.write_flags(
                    a_flags,
                    &[
                        (Flag::C, (fcarry ^ foverflow) & 0x8000 != 0),
                        (Flag::OV0, ov0),
                        (Flag::OV1, ov1),
                    ],
                )
            }
            AluFunction::Shr1 => self.regs.write_flags(
                a_flags,
                &[
                    (Flag::C, (a & 0x01) != 0),
                    (Flag::OV0, false),
                    (Flag::OV1, false),
                ],
            ),
            AluFunction::Shl1 => self.regs.write_flags(
                a_flags,
                &[
                    (Flag::C, (a & 0x8000) != 0),
                    (Flag::OV0, false),
                    (Flag::OV1, false),
                ],
            ),
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use num_traits::ToPrimitive;
    use strum::IntoEnumIterator;

    fn cpu(op: u32) -> CpuUpd77c25 {
        let mut c = CpuUpd77c25::new();
        c.code[2] = (op >> 16) as u8;
        c.code[1] = (op >> 8) as u8;
        c.code[0] = op as u8;
        c
    }

    fn cpu_run(op: u32) -> CpuUpd77c25 {
        let mut c = cpu(op);
        c.step().unwrap();
        c
    }

    fn cpu_run_sr(op: u32, sr: SR) -> CpuUpd77c25 {
        let mut c = cpu(op);
        c.regs.sr = 1 << sr.to_u16().unwrap();
        c.step().unwrap();
        c
    }

    fn cpu_run_reg(op: u32, reg: Register, val: u16) -> CpuUpd77c25 {
        let mut c = cpu(op);
        c.regs.write(reg, val);
        c.step().unwrap();
        c
    }

    fn cpu_run_flag(op: u32, flags: Flags, flag: Flag) -> CpuUpd77c25 {
        let mut c = cpu(op);
        c.regs.flags[flags.to_usize().unwrap()] = 1 << flag.to_u16().unwrap();
        println!("{}", c.dump_state());
        c.step().unwrap();
        println!("{}", c.dump_state());
        c
    }

    #[test]
    fn jp_flags() {
        let test = |flg, opcode: u32| {
            println!("{:?} {:06X}", flg, opcode);

            // Flags A
            // D.14 indicates true/false
            // D.15 indicates A/B
            let opcode = opcode & !(1 << 15);
            let nopcode = opcode & !(1 << 14);
            let yopcode = opcode | (1 << 14);
            assert_eq!(cpu_run(nopcode).regs.pc, 0x0000);
            assert_ne!(cpu_run_flag(nopcode, Flags::A, flg).regs.pc, 0x0000);
            assert_eq!(cpu_run_flag(nopcode, Flags::B, flg).regs.pc, 0x0000);
            assert_ne!(cpu_run(yopcode).regs.pc, 0x0000);
            assert_eq!(cpu_run_flag(yopcode, Flags::A, flg).regs.pc, 0x0000);
            assert_ne!(cpu_run_flag(yopcode, Flags::B, flg).regs.pc, 0x0000);

            // Flags B
            let opcode = opcode | (1 << 15);
            let nopcode = opcode & !(1 << 14);
            let yopcode = opcode | (1 << 14);
            assert_eq!(cpu_run(nopcode).regs.pc, 0x0000);
            assert_eq!(cpu_run_flag(nopcode, Flags::A, flg).regs.pc, 0x0000);
            assert_ne!(cpu_run_flag(nopcode, Flags::B, flg).regs.pc, 0x0000);
            assert_ne!(cpu_run(yopcode).regs.pc, 0x0000);
            assert_ne!(cpu_run_flag(yopcode, Flags::A, flg).regs.pc, 0x0000);
            assert_eq!(cpu_run_flag(yopcode, Flags::B, flg).regs.pc, 0x0000);
        };
        test(Flag::C, 0b10_010000000_00000000000_00);
        test(Flag::Z, 0b10_010001000_00000000000_00);
        test(Flag::OV0, 0b10_010010000_00000000000_00);
        test(Flag::OV1, 0b10_010011000_00000000000_00);
        test(Flag::S0, 0b10_010100000_00000000000_00);
        test(Flag::S1, 0b10_010101000_00000000000_00);
    }

    #[test]
    fn jp_sr_rqm() {
        assert_ne!(cpu_run(0b10_010111110_00000000000_00).regs.pc, 0x0000);
        assert_eq!(
            cpu_run_sr(0b10_010111110_00000000000_00, SR::RQM).regs.pc,
            0x0000
        );
    }

    #[test]
    fn jp_sr_rqmn() {
        assert_eq!(cpu_run(0b10_010111100_00000000000_00).regs.pc, 0x0000);
        assert_ne!(
            cpu_run_sr(0b10_010111100_00000000000_00, SR::RQM).regs.pc,
            0x0000
        );
    }

    #[test]
    fn jp_jdpl0() {
        assert_eq!(
            cpu_run_reg(0b10_010110000_00000000000_00, Register::DP, 0)
                .regs
                .pc,
            0x0000
        );
        assert_eq!(
            cpu_run_reg(0b10_010110000_00000000000_00, Register::DP, 0x0010)
                .regs
                .pc,
            0x0000
        );
        assert_ne!(
            cpu_run_reg(0b10_010110000_00000000000_00, Register::DP, 0x0001)
                .regs
                .pc,
            0x0000
        );
    }

    #[test]
    fn jp_jdpln0() {
        assert_ne!(
            cpu_run_reg(0b10_010110001_00000000000_00, Register::DP, 0)
                .regs
                .pc,
            0x0000
        );
        assert_ne!(
            cpu_run_reg(0b10_010110001_00000000000_00, Register::DP, 0x0010)
                .regs
                .pc,
            0x0000
        );
        assert_eq!(
            cpu_run_reg(0b10_010110001_00000000000_00, Register::DP, 0x0001)
                .regs
                .pc,
            0x0000
        );
    }

    #[test]
    fn jp_jdplnf() {
        assert_ne!(
            cpu_run_reg(0b10_010110011_00000000000_00, Register::DP, 0x000F)
                .regs
                .pc,
            0x0000
        );
        assert_ne!(
            cpu_run_reg(0b10_010110011_00000000000_00, Register::DP, 0x001F)
                .regs
                .pc,
            0x0000
        );
        assert_eq!(
            cpu_run_reg(0b10_010110011_00000000000_00, Register::DP, 0x0001)
                .regs
                .pc,
            0x0000
        );
    }

    #[test]
    fn jp_jdplf() {
        assert_eq!(
            cpu_run_reg(0b10_010110010_00000000000_00, Register::DP, 0x000F)
                .regs
                .pc,
            0x0000
        );
        assert_eq!(
            cpu_run_reg(0b10_010110010_00000000000_00, Register::DP, 0x001F)
                .regs
                .pc,
            0x0000
        );
        assert_ne!(
            cpu_run_reg(0b10_010110010_00000000000_00, Register::DP, 0x0001)
                .regs
                .pc,
            0x0000
        );
    }

    #[test]
    fn jmp() {
        assert_eq!(cpu_run(0b10_100000000_00000000000_00).regs.pc, 0x0000);
        assert_eq!(cpu_run(0b10_100000000_00000000000_00).regs.sp, 0);
        assert_eq!(cpu_run(0b10_100000000_00000001000_00).regs.pc, 0x0008);
    }

    #[test]
    fn call() {
        let c = cpu_run(0b10_101000000_00000001000_00);
        assert_eq!(c.regs.pc, 0x0008);
        assert_eq!(c.regs.sp, 0x0001);
        assert_eq!(c.stack[0], 0x0001);
    }

    #[test]
    fn ld_nop() {
        let c = cpu_run(0b11_1010101010101010_00_0000);
        assert!(Register::iter()
            .filter(|&r| r != Register::PC && r != Register::SGN)
            .all(|r| c.regs.read(r) == 0));
    }

    #[test]
    fn ld_regs() {
        let test = |op, reg| {
            let c = cpu_run(op);
            println!("{}", c.dump_state());
            assert!(Register::iter()
                .filter(|&r| r != Register::PC
                    && r != Register::SGN
                    && r != reg
                    && !(r == Register::SR && reg == Register::DR))
                .all(|r| c.regs.read(r) == 0));
            assert_ne!(c.regs.read(reg), 0);
        };

        test(0b11_1010101010101010_00_0001, Register::ACCA);
        test(0b11_1010101010101010_00_0010, Register::ACCB);
        test(0b11_1010101010101010_00_0011, Register::TR);
        test(0b11_1010101010101010_00_0100, Register::DP);
        test(0b11_1010101010101010_00_0101, Register::RP);
        test(0b11_1010101010101010_00_0110, Register::DR);
        test(0b11_1010101010101010_00_0111, Register::SR);
        test(0b11_1010101010101010_00_1010, Register::K);
        test(0b11_1010101010101010_00_1101, Register::L);
        test(0b11_1010101010101010_00_1110, Register::TRB);
    }
}
