use anyhow::anyhow;
use std::fmt::Display;

use crate::instructions_table::{ByteIterator, Direction, Mode, Operation, Rm, Width};

#[derive(Debug)]
pub enum MoveInstruction {
    RmToFromReg,
    ImmToRm,
    ImmToReg,
    MemToAcc,
    AccToMem,
    RmToSegReg,
    SegRegToRm,
}

#[derive(Debug)]
pub struct RmToFromReg {
    direction: Direction,
    width: Width,
    mode: Mode,
    reg: Rm,
    rm: Rm,
}

impl Display for RmToFromReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.direction {
            Direction::ToRM => write!(f, "{}, {}", self.rm, self.reg),
            Direction::ToReg => write!(f, "{}, {}", self.reg, self.rm),
        }
    }
}

impl Operation<RmToFromReg> for RmToFromReg {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut ByteIterator,
    ) -> anyhow::Result<RmToFromReg>
    where
        RmToFromReg: std::fmt::Display,
    {
        let byte1 = opcode;
        let byte2 = iter.next().ok_or_else(|| anyhow!("Expected second byte"))?;

        let direction = Direction::parse((byte1 >> 1) & 0x1);
        let width = Width::parse(byte1 & 0x1);
        let mode = Mode::parse((byte2 >> 6) & 0x3);
        let reg = Rm::decode_register_with_width(&width, (byte2 >> 3) & 0x7);
        let rm_byte = byte2 & 0x7;

        match mode {
            Mode::Memory => {
                // Special case for direct address mode
                let rm: Rm = if rm_byte == 0x6 {
                    let displacement_lo = iter
                        .next()
                        .ok_or_else(|| anyhow!("Expected low byte of direct address"))?;
                    let displacement_hi = iter
                        .next()
                        .ok_or_else(|| anyhow!("Expected high byte of direct address"))?;
                    Rm::decode_memory_addressing(
                        &mode,
                        rm_byte,
                        Some(*displacement_lo),
                        Some(*displacement_hi),
                    )
                } else {
                    Rm::decode_memory_addressing(&mode, rm_byte, None, None)
                };
                Ok(RmToFromReg {
                    direction,
                    width,
                    mode,
                    reg,
                    rm,
                })
            }
            Mode::Memory8BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected displacement byte"))?;
                let rm = Rm::decode_memory_addressing(&mode, rm_byte, Some(*displacement_lo), None);
                Ok(RmToFromReg {
                    direction,
                    width,
                    mode,
                    reg,
                    rm,
                })
            }
            Mode::Memory16BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of displacement"))?;
                let displacement_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of displacement"))?;
                let rm = Rm::decode_memory_addressing(
                    &mode,
                    rm_byte,
                    Some(*displacement_lo),
                    Some(*displacement_hi),
                );
                Ok(RmToFromReg {
                    direction,
                    width,
                    mode,
                    reg,
                    rm,
                })
            }
            _ => {
                let rm = Rm::decode_register_with_width(&width, rm_byte);
                Ok(RmToFromReg {
                    direction,
                    width,
                    mode,
                    reg,
                    rm,
                })
            }
        }
    }
}

#[derive(Debug)]
pub struct ImmToReg {
    width: Width,
    reg: Rm,
    immediate: u16,
}

impl Display for ImmToReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.width {
            Width::Byte => write!(f, "{}, {}", self.reg, self.immediate as u8),
            Width::Word => write!(f, "{}, {}", self.reg, self.immediate),
        }
    }
}

impl Operation<ImmToReg> for ImmToReg {
    fn parse_opcode_to_instruction(opcode: &u8, iter: &mut ByteIterator) -> anyhow::Result<ImmToReg>
    where
        ImmToReg: std::fmt::Display,
    {
        let byte1 = opcode;

        let width = Width::parse((byte1 >> 3) & 0x1);
        let reg = Rm::decode_register_with_width(&width, byte1 & 0x7);

        let immediate = match width {
            Width::Byte => {
                let imm_byte = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected immediate byte"))?;
                *imm_byte as u16
            }
            Width::Word => {
                let imm_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of immediate"))?;
                let imm_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of immediate"))?;
                ((*imm_hi as u16) << 8) | (*imm_lo as u16)
            }
        };

        Ok(ImmToReg {
            width,
            reg,
            immediate,
        })
    }
}

#[derive(Debug)]
pub struct ImmToRm {
    width: Width,
    mode: Mode,
    rm: Rm,
    immediate: u16,
}

impl Display for ImmToRm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.width {
            Width::Byte => write!(f, "{}, byte {}", self.rm, self.immediate as u8),
            Width::Word => write!(f, "{}, word {}", self.rm, self.immediate),
        }
    }
}

impl Operation<ImmToRm> for ImmToRm {
    fn parse_opcode_to_instruction(opcode: &u8, iter: &mut ByteIterator) -> anyhow::Result<ImmToRm>
    where
        ImmToRm: std::fmt::Display,
    {
        let byte1 = opcode;
        let byte2 = iter.next().ok_or_else(|| anyhow!("Expected second byte"))?;

        let width = Width::parse(byte1 & 0x1);
        let mode = Mode::parse((byte2 >> 6) & 0x3);
        let rm_byte = byte2 & 0x7;

        let rm = match mode {
            Mode::Memory => Rm::decode_memory_addressing(&mode, rm_byte, None, None),
            Mode::Memory8BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected displacement byte"))?;
                Rm::decode_memory_addressing(&mode, rm_byte, Some(*displacement_lo), None)
            }
            Mode::Memory16BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of displacement"))?;
                let displacement_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of displacement"))?;
                Rm::decode_memory_addressing(
                    &mode,
                    rm_byte,
                    Some(*displacement_lo),
                    Some(*displacement_hi),
                )
            }
            _ => {
                return Err(anyhow!(
                    "Invalid mode for Imm to Rm instruction: {:?}",
                    mode
                ));
            }
        };

        let immediate = match width {
            Width::Byte => {
                let imm_byte = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected immediate byte"))?;
                *imm_byte as u16
            }
            Width::Word => {
                let imm_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of immediate"))?;
                let imm_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of immediate"))?;
                u16::from_le_bytes([*imm_lo, *imm_hi])
            }
        };

        Ok(ImmToRm {
            width,
            mode,
            rm,
            immediate,
        })
    }
}

#[derive(Debug)]
pub struct MemToAcc {
    width: Width,
    address: u16,
}

impl Display for MemToAcc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.width {
            Width::Byte => write!(f, "al, [{}]", self.address),
            Width::Word => write!(f, "ax, [{}]", self.address),
        }
    }
}

impl Operation<MemToAcc> for MemToAcc {
    fn parse_opcode_to_instruction(opcode: &u8, iter: &mut ByteIterator) -> anyhow::Result<MemToAcc>
    where
        MemToAcc: std::fmt::Display,
    {
        let byte1 = opcode;
        let lo = iter
            .next()
            .ok_or_else(|| anyhow!("Expected low byte of address"))?;
        let hi = iter
            .next()
            .ok_or_else(|| anyhow!("Expected high byte of address"))?;

        let width = Width::parse(byte1 & 0x1);
        let address = u16::from_le_bytes([*lo, *hi]);

        Ok(MemToAcc { width, address })
    }
}

#[derive(Debug)]
pub struct AccToMem {
    width: Width,
    address: u16,
}

impl Display for AccToMem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.width {
            Width::Byte => write!(f, "[{}], al", self.address),
            Width::Word => write!(f, "[{}], ax", self.address),
        }
    }
}

impl Operation<AccToMem> for AccToMem {
    fn parse_opcode_to_instruction(opcode: &u8, iter: &mut ByteIterator) -> anyhow::Result<AccToMem>
    where
        AccToMem: Display,
    {
        let byte1 = opcode;
        let lo = iter
            .next()
            .ok_or_else(|| anyhow!("Expected low byte of address"))?;
        let hi = iter
            .next()
            .ok_or_else(|| anyhow!("Expected high byte of address"))?;

        let width = Width::parse(byte1 & 0x1);
        let address = u16::from_le_bytes([*lo, *hi]);

        Ok(AccToMem { width, address })
    }
}
