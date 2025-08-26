use anyhow::Result;
use std::path::PathBuf;

use clap::{Parser, Subcommand, arg, command};

use crate::instructions_table::Instruction;

mod instructions_table;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Decode {
        #[arg(short, long)]
        file: PathBuf,
        output: Option<PathBuf>,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Decode { file, output }) => {
            let decoded_content = decode(file).expect("Decoding failed");
            if let Some(output_path) = output {
                save_to_file(decoded_content, output_path).expect("Failed to save to file");
            } else {
                println!(
                    "{}",
                    decoded_content
                        .iter()
                        .map(|instr| instr.to_string())
                        .collect::<Vec<String>>()
                        .join("\n")
                );
            }
        }
        None => {
            println!("No command provided. Use --help for more information.");
        }
    }
}

fn decode(file: &PathBuf) -> Result<Vec<Instruction>> {
    let content = std::fs::read(file).expect("Failed to read file");
    let (chunks, _remainder) = content.as_chunks::<2>();
    let mut decoded_instructions: Vec<Instruction> = Vec::new();
    for byte in chunks {
        let instruction = Instruction::from(*byte);
        decoded_instructions.push(instruction);
    }

    Ok(decoded_instructions)
}

fn save_to_file(content: Vec<Instruction>, output: &PathBuf) -> Result<()> {
    let header = "bits 16\n\n".to_string();
    let content = content
        .iter()
        .map(|instr| instr.to_string())
        .collect::<Vec<String>>()
        .join("\n");
    let content = format!("{}{}", header, content);
    std::fs::write(output, content).expect("Failed to write to file");
    Ok(())
}
