use anyhow::{Ok, anyhow};
use std::fmt::Display;

use crate::{
    instructions_table::{ByteIterator, Direction, Mode, Operation, Rm, Width},
    simulator::{Operand, Simulate},
};

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

    pub op_size: u8,
}

impl Display for RmToFromReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.direction {
            Direction::ToRM => write!(f, "{}, {}", self.rm, self.reg),
            Direction::ToReg => write!(f, "{}, {}", self.reg, self.rm),
        }
    }
}

impl Operation for RmToFromReg {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut ByteIterator,
    ) -> anyhow::Result<RmToFromReg> {
        let byte1 = opcode;
        let byte2 = iter.next().ok_or_else(|| anyhow!("Expected second byte"))?;
        let mut op_size = 2;

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
                    op_size += 2;
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
                    op_size,
                })
            }
            Mode::Memory8BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected displacement byte"))?;
                op_size += 1;
                let rm = Rm::decode_memory_addressing(&mode, rm_byte, Some(*displacement_lo), None);
                Ok(RmToFromReg {
                    direction,
                    width,
                    mode,
                    reg,
                    rm,
                    op_size,
                })
            }
            Mode::Memory16BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of displacement"))?;
                let displacement_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of displacement"))?;
                op_size += 2;
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
                    op_size,
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
                    op_size,
                })
            }
        }
    }
}

impl Simulate for RmToFromReg {
    fn simulate(&self, state: &mut crate::simulator::Simulator) -> anyhow::Result<()> {
        let reg = match &self.reg {
            Rm::Reg(reg) => reg,
            Rm::Memory(_) => unimplemented!(),
        };

        let rm = match &self.rm {
            Rm::Reg(reg) => reg,
            Rm::Memory(mem) => match mem.split_regs() {
                (None, None) => todo!(),
                (None, Some(mem_loc)) => {
                    state.load_from_mem(mem_loc, reg.clone(), &self.width)?;
                    println!("{:?}", state);
                    return Ok(());
                }
                (Some(_), None) => todo!(),
                (Some(_), Some(_)) => todo!(),
            },
        };
        match self.direction {
            Direction::ToReg => state.modify_reg(reg.clone(), Operand::Reg(rm.clone()))?,
            Direction::ToRM => state.modify_reg(rm.clone(), Operand::Reg(reg.clone()))?,
        };
        println!("{:?}", state);
        Ok(())
    }
}

#[derive(Debug)]
pub struct ImmToReg {
    width: Width,
    reg: Rm,
    immediate: u16,

    pub op_size: u8,
}

impl Display for ImmToReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.width {
            Width::Byte => write!(f, "{}, {}", self.reg, self.immediate as u8),
            Width::Word => write!(f, "{}, {}", self.reg, self.immediate),
        }
    }
}

impl Operation for ImmToReg {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut ByteIterator,
    ) -> anyhow::Result<ImmToReg> {
        let byte1 = opcode;
        let mut op_size: u8 = 1;

        let width = Width::parse((byte1 >> 3) & 0x1);
        let reg = Rm::decode_register_with_width(&width, byte1 & 0x7);

        let immediate = match width {
            Width::Byte => {
                let imm_byte = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected immediate byte"))?;
                op_size += 1;
                *imm_byte as u16
            }
            Width::Word => {
                let imm_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of immediate"))?;
                let imm_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of immediate"))?;
                op_size += 2;
                ((*imm_hi as u16) << 8) | (*imm_lo as u16)
            }
        };

        Ok(ImmToReg {
            width,
            reg,
            immediate,
            op_size,
        })
    }
}

impl Simulate for ImmToReg {
    fn simulate(&self, state: &mut crate::simulator::Simulator) -> anyhow::Result<()> {
        match &self.reg {
            Rm::Reg(reg) => {
                state.modify_reg(reg.clone(), Operand::Data(self.immediate))?;
                println!("{:?}", state);
                Ok(())
            }
            Rm::Memory(_) => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub struct ImmToRm {
    width: Width,
    mode: Mode,
    rm: Rm,
    immediate: u16,

    pub op_size: u8,
}

impl Display for ImmToRm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.width {
            Width::Byte => write!(f, "{}, byte {}", self.rm, self.immediate as u8),
            Width::Word => write!(f, "{}, word {}", self.rm, self.immediate),
        }
    }
}

impl Operation for ImmToRm {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut ByteIterator,
    ) -> anyhow::Result<ImmToRm> {
        let byte1 = opcode;
        let byte2 = iter.next().ok_or_else(|| anyhow!("Expected second byte"))?;
        let mut op_size = 2;

        let width = Width::parse(byte1 & 0x1);
        let mode = Mode::parse((byte2 >> 6) & 0x3);
        let rm_byte = byte2 & 0x7;

        let rm = match mode {
            Mode::Memory => {
                if rm_byte == 0x6 {
                    let displacement_lo = iter
                        .next()
                        .ok_or_else(|| anyhow!("Expected low byte of direct address"))?;
                    let displacement_hi = iter
                        .next()
                        .ok_or_else(|| anyhow!("Expected high byte of direct address"))?;
                    op_size += 2;
                    Rm::decode_memory_addressing(
                        &mode,
                        rm_byte,
                        Some(*displacement_lo),
                        Some(*displacement_hi),
                    )
                } else {
                    Rm::decode_memory_addressing(&mode, rm_byte, None, None)
                }
            }
            Mode::Memory8BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected displacement byte"))?;
                op_size += 1;
                Rm::decode_memory_addressing(&mode, rm_byte, Some(*displacement_lo), None)
            }
            Mode::Memory16BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of displacement"))?;
                let displacement_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of displacement"))?;
                op_size += 2;
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
                op_size += 1;
                *imm_byte as u16
            }
            Width::Word => {
                let imm_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of immediate"))?;
                let imm_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of immediate"))?;
                op_size += 2;
                u16::from_le_bytes([*imm_lo, *imm_hi])
            }
        };

        Ok(ImmToRm {
            width,
            mode,
            rm,
            immediate,
            op_size,
        })
    }
}

impl Simulate for ImmToRm {
    fn simulate(&self, state: &mut crate::simulator::Simulator) -> anyhow::Result<()> {
        match &self.rm {
            Rm::Reg(reg) => todo!(),
            Rm::Memory(memory_field) => match memory_field.split_regs() {
                (None, None) => todo!(),
                (None, Some(mem)) => {
                    let immediate_bytes: [u8; 2] = self.immediate.to_be_bytes();
                    state.save_to_mem(mem, immediate_bytes, &self.width)?;
                }
                (Some(_), None) => todo!(),
                (Some(regs), Some(rm_data)) => {
                    let immediate_bytes: [u8; 2] = self.immediate.to_be_bytes();
                    let regs_loc: u16 = regs
                        .iter()
                        .map(|reg| state.get_reg_data(reg).unwrap())
                        .sum();
                    state.save_to_mem(regs_loc + rm_data, immediate_bytes, &self.width)?;
                }
            },
        }

        println!("{:?}", state);
        Ok(())
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

impl Operation for MemToAcc {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut ByteIterator,
    ) -> anyhow::Result<MemToAcc> {
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

impl Operation for AccToMem {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut ByteIterator,
    ) -> anyhow::Result<AccToMem> {
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
