use anyhow::Result;
use std::collections::HashMap;

use crate::instructions_table::Reg;

#[derive(Debug)]
pub struct Simulator {
    registers: HashMap<Reg, u16>,
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
        Self { registers }
    }
}

impl Simulator {
    pub fn modify_reg(&mut self, reg: &Reg, data: u16) -> Result<()> {
        match reg {
            Reg::Ax => self.registers.insert(Reg::Ax, data),
            Reg::Al => todo!(),
            Reg::Ah => todo!(),
            Reg::Bx => self.registers.insert(Reg::Bx, data),
            Reg::Bl => todo!(),
            Reg::Bh => todo!(),
            Reg::Cx => self.registers.insert(Reg::Cx, data),
            Reg::Cl => todo!(),
            Reg::Ch => todo!(),
            Reg::Dx => self.registers.insert(Reg::Dx, data),
            Reg::Dl => todo!(),
            Reg::Dh => todo!(),
            Reg::Sp => self.registers.insert(Reg::Sp, data),
            Reg::Bp => self.registers.insert(Reg::Bp, data),
            Reg::Si => self.registers.insert(Reg::Si, data),
            Reg::Di => self.registers.insert(Reg::Di, data),
        };
        Ok(())
    }
}

pub trait Simulate {
    fn simulate(&self, state: &mut Simulator) -> Result<()>;
}
