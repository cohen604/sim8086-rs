use std::fmt::Display;

#[derive(Debug)]
pub enum OpCode {
    Mov,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let opcode_str = match self {
            OpCode::Mov => "mov",
        };
        write!(f, "{}", opcode_str)
    }
}

#[derive(Debug)]
pub enum Direction {
    ToReg,
    ToRM,
}

#[derive(Debug)]
pub enum Width {
    Byte,
    Word,
}

#[derive(Debug)]
pub enum Mode {
    Register,
    Memory,
    Memory8BitDisplacement,
    Memory16BitDisplacement,
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
pub enum Rm {
    Reg(Reg),
}

impl Display for Rm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rm::Reg(reg) => write!(f, "{}", reg),
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub opcode: OpCode,
    pub direction: Direction,
    pub width: Width,

    pub mode: Mode,
    pub reg: Rm,
    pub rm: Rm,
}

impl From<[u8; 2]> for Instruction {
    fn from(value: [u8; 2]) -> Self {
        let opcode = match value[0] >> 2 {
            0b00100010 => OpCode::Mov,
            _ => panic!("Unknown opcode"),
        };

        let direction = match (value[0] & 0b00000010) >> 1 {
            0 => Direction::ToRM,
            1 => Direction::ToReg,
            _ => unreachable!(),
        };

        let width = match value[0] & 0b00000001 {
            0 => Width::Byte,
            1 => Width::Word,
            _ => unreachable!(),
        };

        let mode = match value[1] >> 6 {
            0b0000011 => Mode::Register,
            0b0000000 => Mode::Memory,
            0b0000001 => Mode::Memory8BitDisplacement,
            0b0000010 => Mode::Memory16BitDisplacement,
            _ => unreachable!(),
        };

        let reg = decode_register(&width, (value[1] & 0b00111000) >> 3);

        let rm = decode_register(&width, value[1] & 0b00000111);

        Instruction {
            opcode,
            direction,
            width,
            mode,
            reg,
            rm,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}, {}", self.opcode, self.reg, self.rm)
    }
}

pub fn decode_register(width: &Width, reg: u8) -> Rm {
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
