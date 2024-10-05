use std::{fs::File, io::{BufReader, BufWriter, Write}, path::{Path, PathBuf}};

use clap::Parser;
use rLuauDec::{bytecode_reader::{self, LuauBytecodeReader}, decompiler::Decompiler, serializer::Serializer };

#[derive(Debug, Parser)]
struct CliArgs {
    input: PathBuf,
    output: PathBuf,
}

fn main() -> bytecode_reader::Result<()> {
    let args = CliArgs::parse();
    let file = BufReader::new(File::open(args.input)?);
    let reader = LuauBytecodeReader::new();
    let bytecode = reader.read(file)?;
    let decompiler = Decompiler::new(bytecode);
    let decompiled = decompiler.decompile().expect("Decomp error");
    let mut out_file = File::create(args.output)?;
    let ser = Serializer {
        chunk: decompiled,
        locals: Vec::new(),
        indent_level: 0,
    };
    ser.write(&mut out_file).expect("write err");
    Ok(())
}
