use anyhow::{Ok, Result};
use std::{collections::HashMap, hash::Hash};

use crate::{
    instructions_table::{DecodedInstruction, Reg},
    models::flag::Flag,
};

#[derive(Debug)]
pub struct Orchestrator {
    pub instructions: HashMap<u32, DecodedInstruction>,
    state: Simulator,
}

impl Default for Orchestrator {
    fn default() -> Self {
        let instructions: HashMap<u32, DecodedInstruction> = HashMap::new();
        let state = Simulator::default();

        Self {
            instructions,
            state,
        }
    }
}

impl Orchestrator {
    pub fn simulate(mut self) -> Result<()> {
        while let Some(instruction) = self.instructions.get(&self.state.ip) {
            self.state.ip += instruction.get_op_size() as u32;
            instruction.simulate(&mut self.state)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Simulator {
    registers: HashMap<Reg, u16>,
    flags: HashMap<Flag, u8>,
    ip: u32,
}

#[derive(Debug)]
pub enum Operand {
    Reg(Reg),
    Data(u16),
}

impl Default for Simulator {
    fn default() -> Self {
        let mut registers = HashMap::new();
        registers.insert(Reg::Ax, 0x0);
        registers.insert(Reg::Bx, 0x0);
        registers.insert(Reg::Cx, 0x0);
        registers.insert(Reg::Dx, 0x0);
        registers.insert(Reg::Si, 0x0);
        registers.insert(Reg::Sp, 0x0);
        registers.insert(Reg::Bp, 0x0);
        registers.insert(Reg::Di, 0x0);

        let mut flags = HashMap::new();
        flags.insert(Flag::Sign, 0x0);
        flags.insert(Flag::Zero, 0x0);

        Self {
            registers,
            flags,
            ip: 0,
        }
    }
}

impl Simulator {
    pub fn modify_reg(&mut self, reg: Reg, data: Operand) -> Result<()> {
        match data {
            Operand::Reg(rm) => {
                let data = self.registers.get(&rm).unwrap();
                self.registers.insert(reg, *data)
            }
            Operand::Data(data) => self.registers.insert(reg, data),
        };
        Ok(())
    }

    pub fn add(&mut self, reg: Reg, operand: Operand) -> Result<()> {
        let operand = match operand {
            Operand::Reg(reg) => *self.registers.get(&reg).unwrap(),
            Operand::Data(data) => data,
        };
        let reg_data = *self.registers.get(&reg).unwrap();
        let result = reg_data + operand;

        self.set_flags(result)?;
        self.registers.insert(reg, result);

        Ok(())
    }

    pub fn sub(&mut self, reg: Reg, operand: Operand) -> Result<()> {
        let operand = match operand {
            Operand::Reg(reg) => *self.registers.get(&reg).unwrap(),
            Operand::Data(data) => data,
        };
        let reg_data = *self.registers.get(&reg).unwrap();
        let result = reg_data.wrapping_sub(operand);

        self.set_flags(result)?;
        self.registers.insert(reg, result);

        Ok(())
    }

    pub fn cmp(&mut self, reg: Reg, operand: Operand) -> Result<()> {
        let operand = match operand {
            Operand::Reg(reg) => *self.registers.get(&reg).unwrap(),
            Operand::Data(data) => data,
        };

        let reg_data = *self.registers.get(&reg).unwrap();
        let result = reg_data - operand;

        self.set_flags(result)?;

        Ok(())
    }

    pub fn set_flags(&mut self, data: u16) -> Result<()> {
        // Check Zero flag
        if data == 0x0 {
            self.flags.insert(Flag::Zero, 0x1);
        } else {
            self.flags.insert(Flag::Zero, 0x0);
        }

        // Check Sign flag
        if (data as i16) < 0 {
            self.flags.insert(Flag::Sign, 0x1);
        } else {
            self.flags.insert(Flag::Sign, 0x0);
        }

        Ok(())
    }

    pub fn modify_pi(&mut self, inc: i8) -> Result<()> {
        let ip = self.ip as i32 + inc as i32;
        self.ip = ip as u32;
        Ok(())
    }

    pub fn is_zero_flag_set(&self) -> Result<bool> {
        Ok(*self.flags.get(&Flag::Zero).unwrap() == 1)
    }
}

pub trait Simulate {
    fn simulate(&self, state: &mut Simulator) -> Result<()>;
}
