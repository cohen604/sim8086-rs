use std::fmt::Display;

use anyhow::anyhow;

use crate::instructions_table::Operation;

#[derive(Debug)]
pub enum JumpType {
    Jo,
    Jno,
    JbJnaeJc,
    JnbJaeJnc,
    JeJz,
    JneJnz,
    JbeJna,
    JnbeJa,
    Js,
    Jns,
    JpJpe,
    JnpJpo,
    JlJnge,
    JnlJge,
    JleJng,
    JnleJg,
    Loop,
    LoopzLoope,
    LoopnzLoopne,
    Jcxz,
}

impl Display for JumpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let operation = match self {
            JumpType::Jo => "jo",
            JumpType::Jno => "jno",
            JumpType::JbJnaeJc => "jb",
            JumpType::JnbJaeJnc => "jnb",
            JumpType::JeJz => "je",
            JumpType::JneJnz => "jne",
            JumpType::JbeJna => "jbe",
            JumpType::JnbeJa => "jnbe",
            JumpType::Js => "js",
            JumpType::Jns => "jns",
            JumpType::JpJpe => "jp",
            JumpType::JnpJpo => "jnp",
            JumpType::JlJnge => "jl",
            JumpType::JnlJge => "jnl",
            JumpType::JleJng => "jle",
            JumpType::JnleJg => "jnle",
            JumpType::Loop => "loop",
            JumpType::LoopzLoope => "loopz",
            JumpType::LoopnzLoopne => "loopnz",
            JumpType::Jcxz => "jcxz",
        };
        write!(f, "{}", operation)
    }
}

impl JumpType {
    fn parse(op: u8) -> Self {
        match op {
            0x70 => Self::Jo,
            0x71 => Self::Jno,
            0x72 => Self::JbJnaeJc,
            0x73 => Self::JnbJaeJnc,
            0x74 => Self::JeJz,
            0x75 => Self::JneJnz,
            0x76 => Self::JbeJna,
            0x77 => Self::JnbeJa,
            0x78 => Self::Js,
            0x79 => Self::Jns,
            0x7A => Self::JpJpe,
            0x7B => Self::JnpJpo,
            0x7C => Self::JlJnge,
            0x7D => Self::JnlJge,
            0x7E => Self::JleJng,
            0x7F => Self::JnleJg,
            0xE0 => Self::LoopnzLoopne,
            0xE1 => Self::LoopzLoope,
            0xE2 => Self::Loop,
            0xE3 => Self::Jcxz,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub struct ReturnCall {
    operation: JumpType,
    inc: i8,
}

impl Display for ReturnCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ${}", self.operation, self.inc)
    }
}

impl Operation for ReturnCall {
    fn parse_opcode_to_instruction(
        opcode: &u8,
        iter: &mut crate::instructions_table::ByteIterator,
    ) -> anyhow::Result<Self>
    where
        Self: Display + Sized,
    {
        let operation = JumpType::parse(*opcode);
        let byte2 = iter.next().ok_or(anyhow!("Expecting IP-INC8 byte"))?;
        let inc = *byte2 as i8;

        Ok(ReturnCall { operation, inc })
    }
}
