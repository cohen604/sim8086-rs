use anyhow::{Result, anyhow};
use std::fmt::Display;
use tracing::error;

use crate::{
    arithmetic_operations::{self, ArithmeticOperation},
    move_operations::{AccToMem, ImmToReg, ImmToRm, MemToAcc, MoveInstruction, RmToFromReg},
};

pub type ByteIterator<'a> = std::slice::Iter<'a, u8>;

pub trait Operation {
    fn parse_opcode_to_instruction(opcode: &u8, iter: &mut ByteIterator) -> Result<Self>
    where
        Self: Display + Sized;
}

#[derive(Debug)]
pub enum OpCode {
    Mov(MoveInstruction),
    Arithmetic(ArithmeticOperation),
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let opcode_str = match self {
            OpCode::Mov(_) => "mov",
            OpCode::Arithmetic(_) => "",
        };
        write!(f, "{}", opcode_str)
    }
}

impl OpCode {
    fn parse_opcode(opcode: &u8) -> OpCode {
        match opcode {
            0x88..=0x8B => OpCode::Mov(MoveInstruction::RmToFromReg),
            0xB0..=0xBF => OpCode::Mov(MoveInstruction::ImmToReg),
            0xC6..=0xC7 => OpCode::Mov(MoveInstruction::ImmToRm),
            0xA0..=0xA1 => OpCode::Mov(MoveInstruction::MemToAcc),
            0xA2..=0xA3 => OpCode::Mov(MoveInstruction::AccToMem),
            0x80..=0x83 => OpCode::Arithmetic(ArithmeticOperation::ImmToRem),
            0x0..=0x3 | 0x28..=0x2B | 0x38..=0x3B => {
                OpCode::Arithmetic(ArithmeticOperation::RmWithReg)
            }
            0x4..=0x5 | 0x2C..=0x2D | 0x3C..=0x3D => {
                OpCode::Arithmetic(ArithmeticOperation::ImmToAcc)
            }
            _ => {
                error!("Unsupported opcode: {:02X}", opcode);
                panic!("Unsupported opcode");
            }
        }
    }
}

#[derive(Debug)]
pub enum Direction {
    ToReg,
    ToRM,
}

impl Direction {
    pub fn parse(direction: u8) -> Self {
        match direction {
            0 => Direction::ToRM,
            1 => Direction::ToReg,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum Width {
    Byte,
    Word,
}

impl Width {
    pub fn parse(width: u8) -> Self {
        match width {
            0 => Width::Byte,
            1 => Width::Word,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum Mode {
    Register,
    Memory,
    Memory8BitDisplacement,
    Memory16BitDisplacement,
}

impl Mode {
    pub fn parse(mode: u8) -> Self {
        match mode {
            0b00 => Mode::Memory,
            0b01 => Mode::Memory8BitDisplacement,
            0b10 => Mode::Memory16BitDisplacement,
            0b11 => Mode::Register,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum Reg {
    Ax,
    Al,
    Ah,
    Bx,
    Bl,
    Bh,
    Cx,
    Cl,
    Ch,
    Dx,
    Dl,
    Dh,
    Sp,
    Bp,
    Si,
    Di,
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg_str = match self {
            Reg::Ax => "ax",
            Reg::Al => "al",
            Reg::Ah => "ah",
            Reg::Bx => "bx",
            Reg::Bl => "bl",
            Reg::Bh => "bh",
            Reg::Cx => "cx",
            Reg::Cl => "cl",
            Reg::Ch => "ch",
            Reg::Dx => "dx",
            Reg::Dl => "dl",
            Reg::Dh => "dh",
            Reg::Sp => "sp",
            Reg::Bp => "bp",
            Reg::Si => "si",
            Reg::Di => "di",
        };
        write!(f, "{}", reg_str)
    }
}

#[derive(Debug)]
pub enum MemoryField {
    BxSi,
    BxDi,
    BpSi,
    BpDi,
    Si,
    Di,
    DirectAddress(u16),
    Bx,
    BxSi8bitDisplacement(u8),
    BxDi8bitDisplacement(u8),
    BpSi8bitDisplacement(u8),
    BpDi8bitDisplacement(u8),
    Si8bitDisplacement(u8),
    Di8bitDisplacement(u8),
    Bp8bitDisplacement(u8),
    Bx8bitDisplacement(u8),
    BxSi16bitDisplacement(u16),
    BxDi16bitDisplacement(u16),
    BpSi16bitDisplacement(u16),
    BpDi16bitDisplacement(u16),
    Si16bitDisplacement(u16),
    Di16bitDisplacement(u16),
    Bp16bitDisplacement(u16),
    Bx16bitDisplacement(u16),
}

impl Display for MemoryField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mem_str = match self {
            MemoryField::BxSi => "[bx + si]",
            MemoryField::BxDi => "[bx + di]",
            MemoryField::BpSi => "[bp + si]",
            MemoryField::BpDi => "[bp + di]",
            MemoryField::Si => "[si]",
            MemoryField::Di => "[di]",
            MemoryField::DirectAddress(value) => &format!("[{}]", value),
            MemoryField::Bx => "[bx]",
            MemoryField::BxSi8bitDisplacement(disp) => &format!("[bx + si + {}]", disp),
            MemoryField::BxDi8bitDisplacement(disp) => &format!("[bx + di + {}]", disp),
            MemoryField::BpSi8bitDisplacement(disp) => &format!("[bp + si + {}]", disp),
            MemoryField::BpDi8bitDisplacement(disp) => &format!("[bp + di + {}]", disp),
            MemoryField::Si8bitDisplacement(disp) => &format!("[si + {}]", disp),
            MemoryField::Di8bitDisplacement(disp) => &format!("[di + {}]", disp),
            MemoryField::Bp8bitDisplacement(disp) => &format!("[bp + {}]", disp),
            MemoryField::Bx8bitDisplacement(disp) => &format!("[bx + {}]", disp),
            MemoryField::BxSi16bitDisplacement(disp) => &format!("[bx + si + {}]", disp),
            MemoryField::BxDi16bitDisplacement(disp) => &format!("[bx + di + {}]", disp),
            MemoryField::BpSi16bitDisplacement(disp) => &format!("[bp + si + {}]", disp),
            MemoryField::BpDi16bitDisplacement(disp) => &format!("[bp + di + {}]", disp),
            MemoryField::Si16bitDisplacement(disp) => &format!("[si + {}]", disp),
            MemoryField::Di16bitDisplacement(disp) => &format!("[di + {}]", disp),
            MemoryField::Bp16bitDisplacement(disp) => &format!("[bp + {}]", disp),
            MemoryField::Bx16bitDisplacement(disp) => &format!("[bx + {}]", disp),
        };
        write!(f, "{}", mem_str)
    }
}

#[derive(Debug)]
pub enum Rm {
    Reg(Reg),
    Memory(MemoryField),
}

impl Display for Rm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rm::Reg(reg) => write!(f, "{}", reg),
            Rm::Memory(mem) => write!(f, "{}", mem),
        }
    }
}

impl Rm {
    pub fn decode_register_with_width(width: &Width, reg: u8) -> Rm {
        match (width, reg) {
            (Width::Byte, 0b000) => Rm::Reg(Reg::Al),
            (Width::Byte, 0b001) => Rm::Reg(Reg::Cl),
            (Width::Byte, 0b010) => Rm::Reg(Reg::Dl),
            (Width::Byte, 0b011) => Rm::Reg(Reg::Bl),
            (Width::Byte, 0b100) => Rm::Reg(Reg::Ah),
            (Width::Byte, 0b101) => Rm::Reg(Reg::Ch),
            (Width::Byte, 0b110) => Rm::Reg(Reg::Dh),
            (Width::Byte, 0b111) => Rm::Reg(Reg::Bh),
            (Width::Word, 0b000) => Rm::Reg(Reg::Ax),
            (Width::Word, 0b001) => Rm::Reg(Reg::Cx),
            (Width::Word, 0b010) => Rm::Reg(Reg::Dx),
            (Width::Word, 0b011) => Rm::Reg(Reg::Bx),
            (Width::Word, 0b100) => Rm::Reg(Reg::Sp),
            (Width::Word, 0b101) => Rm::Reg(Reg::Bp),
            (Width::Word, 0b110) => Rm::Reg(Reg::Si),
            (Width::Word, 0b111) => Rm::Reg(Reg::Di),
            _ => unreachable!(),
        }
    }

    pub fn decode_memory_addressing(
        mode: &Mode,
        rm: u8,
        displacement_lo: Option<u8>,
        displacement_hi: Option<u8>,
    ) -> Rm {
        let displacement = match (displacement_lo, displacement_hi) {
            (Some(lo), Some(hi)) => Some(u16::from_le_bytes([lo, hi])),
            (Some(lo), None) => Some(u16::from(lo)),
            _ => None,
        };

        match (mode, rm) {
            (Mode::Register, _) => unreachable!(),
            (Mode::Memory, 0b000) => Rm::Memory(MemoryField::BxSi),
            (Mode::Memory, 0b001) => Rm::Memory(MemoryField::BxDi),
            (Mode::Memory, 0b010) => Rm::Memory(MemoryField::BpSi),
            (Mode::Memory, 0b011) => Rm::Memory(MemoryField::BpDi),
            (Mode::Memory, 0b100) => Rm::Memory(MemoryField::Si),
            (Mode::Memory, 0b101) => Rm::Memory(MemoryField::Di),
            (Mode::Memory, 0b110) => Rm::Memory(MemoryField::DirectAddress(displacement.unwrap())),
            (Mode::Memory, 0b111) => Rm::Memory(MemoryField::Bx),
            (Mode::Memory8BitDisplacement, 0b000) => Rm::Memory(MemoryField::BxSi8bitDisplacement(
                displacement.unwrap() as u8,
            )),
            (Mode::Memory8BitDisplacement, 0b001) => Rm::Memory(MemoryField::BxDi8bitDisplacement(
                displacement.unwrap() as u8,
            )),
            (Mode::Memory8BitDisplacement, 0b010) => Rm::Memory(MemoryField::BpSi8bitDisplacement(
                displacement.unwrap() as u8,
            )),
            (Mode::Memory8BitDisplacement, 0b011) => Rm::Memory(MemoryField::BpDi8bitDisplacement(
                displacement.unwrap() as u8,
            )),
            (Mode::Memory8BitDisplacement, 0b100) => {
                Rm::Memory(MemoryField::Si8bitDisplacement(displacement.unwrap() as u8))
            }
            (Mode::Memory8BitDisplacement, 0b101) => {
                Rm::Memory(MemoryField::Di8bitDisplacement(displacement.unwrap() as u8))
            }
            (Mode::Memory8BitDisplacement, 0b110) => {
                Rm::Memory(MemoryField::Bp8bitDisplacement(displacement.unwrap() as u8))
            }
            (Mode::Memory8BitDisplacement, 0b111) => {
                Rm::Memory(MemoryField::Bx8bitDisplacement(displacement.unwrap() as u8))
            }
            (Mode::Memory16BitDisplacement, 0b000) => {
                Rm::Memory(MemoryField::BxSi16bitDisplacement(displacement.unwrap()))
            }
            (Mode::Memory16BitDisplacement, 0b001) => {
                Rm::Memory(MemoryField::BxDi16bitDisplacement(displacement.unwrap()))
            }
            (Mode::Memory16BitDisplacement, 0b010) => {
                Rm::Memory(MemoryField::BpSi16bitDisplacement(displacement.unwrap()))
            }
            (Mode::Memory16BitDisplacement, 0b011) => {
                Rm::Memory(MemoryField::BpDi16bitDisplacement(displacement.unwrap()))
            }
            (Mode::Memory16BitDisplacement, 0b100) => {
                Rm::Memory(MemoryField::Si16bitDisplacement(displacement.unwrap()))
            }
            (Mode::Memory16BitDisplacement, 0b101) => {
                Rm::Memory(MemoryField::Di16bitDisplacement(displacement.unwrap()))
            }
            (Mode::Memory16BitDisplacement, 0b110) => {
                Rm::Memory(MemoryField::Bp16bitDisplacement(displacement.unwrap()))
            }
            (Mode::Memory16BitDisplacement, 0b111) => {
                Rm::Memory(MemoryField::Bx16bitDisplacement(displacement.unwrap()))
            }
            _ => unreachable!(),
        }
    }

    pub fn parse(rm_byte: u8, width: &Width, mode: &Mode, iter: &mut ByteIterator) -> Result<Rm> {
        match mode {
            Mode::Memory => {
                // Special case for direct address mode
                if rm_byte == 0x6 {
                    let displacement_lo = iter
                        .next()
                        .ok_or_else(|| anyhow!("Expected low byte of direct address"))?;
                    let displacement_hi = iter
                        .next()
                        .ok_or_else(|| anyhow!("Expected high byte of direct address"))?;
                    Ok(Rm::decode_memory_addressing(
                        mode,
                        rm_byte,
                        Some(*displacement_lo),
                        Some(*displacement_hi),
                    ))
                } else {
                    Ok(Rm::decode_memory_addressing(mode, rm_byte, None, None))
                }
            }
            Mode::Memory8BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected displacement byte"))?;
                Ok(Rm::decode_memory_addressing(
                    mode,
                    rm_byte,
                    Some(*displacement_lo),
                    None,
                ))
            }
            Mode::Memory16BitDisplacement => {
                let displacement_lo = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected low byte of displacement"))?;
                let displacement_hi = iter
                    .next()
                    .ok_or_else(|| anyhow!("Expected high byte of displacement"))?;
                Ok(Rm::decode_memory_addressing(
                    mode,
                    rm_byte,
                    Some(*displacement_lo),
                    Some(*displacement_hi),
                ))
            }
            Mode::Register => Ok(Rm::decode_register_with_width(width, rm_byte)),
        }
    }
}

#[derive(Debug)]
pub struct DecodedInstruction {
    pub opcode: OpCode,
    pub operand_data: OperandData,
}

#[derive(Debug)]
pub enum OperandData {
    RmToFromReg(RmToFromReg),
    ImmToReg(ImmToReg),
    ImmToRm(ImmToRm),
    MemToAcc(MemToAcc),
    AccToMem(AccToMem),
    AOImmToRm(arithmetic_operations::ImmToRm),
    AORmWithReg(arithmetic_operations::RmWithReg),
    AOImmToAcc(arithmetic_operations::ImmToAcc),
}

impl Display for OperandData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperandData::RmToFromReg(data) => write!(f, "{}", data),
            OperandData::ImmToReg(data) => write!(f, "{}", data),
            OperandData::ImmToRm(data) => write!(f, "{}", data),
            OperandData::MemToAcc(data) => write!(f, "{}", data),
            OperandData::AccToMem(data) => write!(f, "{}", data),
            OperandData::AOImmToRm(data) => write!(f, "{}", data),
            OperandData::AORmWithReg(data) => write!(f, "{}", data),
            OperandData::AOImmToAcc(data) => write!(f, "{}", data),
        }
    }
}

pub fn decode_instructions(content: Vec<u8>) -> Result<Vec<String>> {
    let mut reader = content.iter();
    let mut instructions: Vec<String> = Vec::new();
    loop {
        let opcode = reader.next();
        if opcode.is_none() {
            break;
        }
        let opcode = opcode.unwrap();
        let op_code = OpCode::parse_opcode(opcode);
        let instruction = match op_code {
            OpCode::Mov(MoveInstruction::RmToFromReg) => {
                let operation = RmToFromReg::parse_opcode_to_instruction(opcode, &mut reader)?;
                DecodedInstruction {
                    opcode: op_code,
                    operand_data: OperandData::RmToFromReg(operation),
                }
            }
            OpCode::Mov(MoveInstruction::ImmToReg) => {
                let operation = ImmToReg::parse_opcode_to_instruction(opcode, &mut reader)?;
                DecodedInstruction {
                    opcode: op_code,
                    operand_data: OperandData::ImmToReg(operation),
                }
            }
            OpCode::Mov(MoveInstruction::ImmToRm) => {
                let operation = ImmToRm::parse_opcode_to_instruction(opcode, &mut reader)?;
                DecodedInstruction {
                    opcode: op_code,
                    operand_data: OperandData::ImmToRm(operation),
                }
            }
            OpCode::Mov(MoveInstruction::MemToAcc) => {
                let operation = MemToAcc::parse_opcode_to_instruction(opcode, &mut reader)?;
                DecodedInstruction {
                    opcode: op_code,
                    operand_data: OperandData::MemToAcc(operation),
                }
            }
            OpCode::Mov(MoveInstruction::AccToMem) => {
                let operation = AccToMem::parse_opcode_to_instruction(opcode, &mut reader)?;
                DecodedInstruction {
                    opcode: op_code,
                    operand_data: OperandData::AccToMem(operation),
                }
            }
            OpCode::Arithmetic(ArithmeticOperation::ImmToRem) => {
                let operation = arithmetic_operations::ImmToRm::parse_opcode_to_instruction(
                    opcode,
                    &mut reader,
                )?;
                DecodedInstruction {
                    opcode: op_code,
                    operand_data: OperandData::AOImmToRm(operation),
                }
            }
            OpCode::Arithmetic(ArithmeticOperation::RmWithReg) => {
                let operation = arithmetic_operations::RmWithReg::parse_opcode_to_instruction(
                    opcode,
                    &mut reader,
                )?;
                DecodedInstruction {
                    opcode: op_code,
                    operand_data: OperandData::AORmWithReg(operation),
                }
            }
            OpCode::Arithmetic(ArithmeticOperation::ImmToAcc) => {
                let operation = arithmetic_operations::ImmToAcc::parse_opcode_to_instruction(
                    opcode,
                    &mut reader,
                )?;
                DecodedInstruction {
                    opcode: op_code,
                    operand_data: OperandData::AOImmToAcc(operation),
                }
            }
            _ => {
                error!("Unsupported opcode: {:02X}", opcode);
                break;
            }
        };
        println!(
            "Decoded instruction: {} {}",
            instruction.opcode, instruction.operand_data
        );
        instructions.push(format!(
            "{} {}",
            instruction.opcode, instruction.operand_data
        ));
    }

    Ok(instructions)
}

pub fn parse_immediate(width: &Width, iter: &mut ByteIterator) -> Result<u16> {
    match width {
        Width::Byte => {
            let imm_byte = iter
                .next()
                .ok_or_else(|| anyhow!("Expected immediate byte"))?;
            Ok(*imm_byte as u16)
        }
        Width::Word => {
            let imm_lo = iter
                .next()
                .ok_or_else(|| anyhow!("Expected low byte of immediate"))?;
            let imm_hi = iter
                .next()
                .ok_or_else(|| anyhow!("Expected high byte of immediate"))?;
            Ok(u16::from_le_bytes([*imm_lo, *imm_hi]))
        }
    }
}
