use anyhow::Result;
use std::path::PathBuf;

use clap::{Parser, Subcommand, arg, command};

mod arithmetic_operations;
mod instructions_table;
mod move_operations;

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
    tracing_subscriber::fmt::init();
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Decode { file, output }) => {
            let decoded_content = decode(file).expect("Decoding failed");
            if let Some(output_path) = output {
                save_to_file(decoded_content, output_path).expect("Failed to save to file");
            } else {
                println!("{}", decoded_content.join("\n"));
            }
        }
        None => {
            println!("No command provided. Use --help for more information.");
        }
    }
}

fn decode(file: &PathBuf) -> Result<Vec<String>> {
    let content = std::fs::read(file).expect("Failed to read file");
    for byte in &content {
        println!("{:08b} ", byte);
    }
    let decoded_instructions = instructions_table::decode_instructions(content)?;

    Ok(decoded_instructions)
}

fn save_to_file(content: Vec<String>, output: &PathBuf) -> Result<()> {
    let header = "bits 16\n\n".to_string();
    let content = content.join("\n");
    let content = format!("{}{}", header, content);
    std::fs::write(output, content).expect("Failed to write to file");
    Ok(())
}
