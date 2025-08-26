use std::path::PathBuf;

use clap::{Parser, Subcommand, arg, command};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Disassemble {
        #[arg(short, long)]
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Disassemble { file }) => {
            todo!()
        }
        None => {
            println!("No command provided. Use --help for more information.");
        }
    }
}
