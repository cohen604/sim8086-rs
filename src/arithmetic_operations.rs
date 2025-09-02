use anyhow::anyhow;
use std::{
    f32::consts::PI,
    fmt::{Display, write},
};

use crate::instructions_table::{Direction, Mode, Operation, Rm, Width, parse_immediate};

#[derive(Debug)]
pub enum ArithmeticOperation {
    ImmToRem,
    ImmToAcc,
    RmWithReg,
}

#[derive(Debug)]
pub enum ArithmeticType {
    Add,
    Sub,
    Cmp,
}

impl Display for ArithmeticType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithmeticType::Add => write!(f, "add"),
            ArithmeticType::Cmp => write!(f, "cmp"),
            ArithmeticType::Sub => write!(f, "sub"),
        }
    }
}

impl ArithmeticType {
    fn parse(op: u8) -> Self {
        match op {
            0x0 => ArithmeticType::Add,
            0x5 => ArithmeticType::Sub,
            0x7 => ArithmeticType::Cmp,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub struct ImmToRm {
    width: Width,
    mode: Mode,
    operation: ArithmeticType,
    rm: Rm,
    immediate: u16,
    sign: u8,
}

impl Display for ImmToRm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.width {
            Width::Byte => write!(f, "{} {}, byte {}", self.operation, self.rm, self.immediate),
            Width::Word => write!(f, "{} {}, word {}", self.operation, self.rm, self.immediate),
        }
    }
}

impl Operation for ImmToRm {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut crate::instructions_table::ByteIterator,
    ) -> anyhow::Result<ImmToRm> {
        let byte1 = opcode;
        let byte2 = iter.next().ok_or_else(|| anyhow!("Expected second byte"))?;

        let width = Width::parse(byte1 & 0x1);
        let sign = (byte1 >> 1) & 0x1;
        let mode = Mode::parse((byte2 >> 6) & 0x3);
        let rm = Rm::parse(byte2 & 0x7, &width, &mode, iter)?;
        let immediate = match (sign, &width) {
            (0x0, Width::Word) => {
                let imm_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of immediate"))?;
                let imm_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of immediate"))?;
                u16::from_le_bytes([*imm_lo, *imm_hi])
            }
            _ => {
                let imm_byte = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected immediate byte"))?;
                *imm_byte as u16
            }
        };

        let operation = ArithmeticType::parse((byte2 >> 3) & 0x7);

        Ok(ImmToRm {
            width,
            mode,
            operation,
            rm,
            immediate,
            sign,
        })
    }
}

#[derive(Debug)]
pub struct RmWithReg {
    operation: ArithmeticType,
    direction: Direction,
    width: Width,
    mode: Mode,
    reg: Rm,
    rm: Rm,
}

impl Display for RmWithReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.direction {
            Direction::ToRM => write!(f, "{} {}, {}", self.operation, self.rm, self.reg),
            Direction::ToReg => write!(f, "{} {}, {}", self.operation, self.reg, self.rm),
        }
    }
}

impl Operation for RmWithReg {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut crate::instructions_table::ByteIterator,
    ) -> anyhow::Result<Self> {
        let byte1 = opcode;
        let operation = ArithmeticType::parse((byte1 >> 3) & 0x7);
        let direction = Direction::parse((byte1 >> 1) & 0x1);
        let width = Width::parse(byte1 & 0x1);

        let byte2 = iter.next().ok_or(anyhow!("Expected the Mod Reg Byte"))?;
        let mode = Mode::parse((byte2 >> 6) & 0x3);
        let reg = Rm::decode_register_with_width(&width, (byte2 >> 3) & 0x7);
        let rm = Rm::parse(byte2 & 0x7, &width, &mode, iter)?;

        Ok(RmWithReg {
            operation,
            direction,
            width,
            mode,
            reg,
            rm,
        })
    }
}

#[derive(Debug)]
pub struct ImmToAcc {
    operation: ArithmeticType,
    width: Width,
    immediate: u16,
}

impl Display for ImmToAcc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.width {
            Width::Byte => write!(f, "{} al, {}", self.operation, self.immediate),
            Width::Word => write!(f, "{} ax, {}", self.operation, self.immediate),
        }
    }
}

impl Operation for ImmToAcc {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut crate::instructions_table::ByteIterator,
    ) -> anyhow::Result<Self>
    where
        Self: Display + Sized,
    {
        let byte1 = opcode;
        let operation = ArithmeticType::parse((byte1 >> 3) & 0x7);
        let width = Width::parse(byte1 & 0x1);
        let immediate = parse_immediate(&width, iter)?;

        Ok(ImmToAcc {
            operation,
            width,
            immediate,
        })
    }
}
