use std::{io::{Read, BufRead, self}, collections::HashMap, num::Wrapping, ops::Deref};
use num_derive::FromPrimitive;

use crate::{bytecode::{Bytecode, ConstImport, ConstTable, Constant, DebugInfo, DebugLocalVar, Function}, opcode::{Opcode, OpcodeKind}};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("Invalid Opcode: {}!", .byte)]
    InvalidOpcode { byte: u8 },
    #[error("Invalid Constant: {}!", .byte)]
    InvalidConstant { byte: u8 },
    #[error("Tried to convert byte to bool! This is supposed to be a bool\
        what is the byte {} doing here? Uhh should this be true or false or\
        should i panic?", .byte)]
    ByteToBoolErr { byte: u8 },
    #[error("Generic Dev Error Lol: {}", .0)]
    Generic(String)
}

#[derive(Default)]
pub struct LuauBytecodeReader {
    opcode_decoder: Option<Box<dyn Fn(u8) -> u8>>
}

impl LuauBytecodeReader {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_opcode_decoder<F: Fn(u8) -> u8 + 'static>(self, decoder: F) -> Self {
        Self {
            opcode_decoder: Some(Box::new(decoder)),
            ..self
        }
    }
}

impl LuauBytecodeReader {
    pub fn read<R: Read + BufRead>(self, mut reader: R) -> Result<Bytecode> {
        let version = Self::read_u8(&mut reader)?;
        let strings = Self::read_string_table(&mut reader)?;
        let functions = self.read_all_functions(&mut reader)?;
        let main_func_id = Self::read_var_int(&mut reader)?;
        Ok(Bytecode {
            version,
            strings,
            functions,
            main_func_id,
        })
    }

    fn read_string_table<R: Read + BufRead>(reader: &mut R) -> io::Result<Vec<String>>{
        let string_count = Self::read_var_int(reader)?;
        let mut strings = Vec::with_capacity(string_count as usize);
        for _ in 0..string_count {
            strings.push(Self::read_string(reader)?);
        }
        Ok(strings)
    }

    fn read_all_functions<R: Read + BufRead>(&self, reader: &mut R) -> Result<HashMap<u32, Function>> {
        let func_count = Self::read_var_int(reader)?;
        let mut functions: HashMap<u32, Function> = HashMap::with_capacity(func_count as usize);
        for i in 0..func_count {
            functions.insert(i, self.read_function(reader)?);
        }
        Ok(functions)
    }

    fn read_function<R: Read + BufRead>(&self, reader: &mut R) -> Result<Function> {
        let max_stack_size = Self::read_u8(reader)?;
        let num_params = Self::read_u8(reader)?;
        let num_up_vals = Self::read_u8(reader)?;
        let is_vargs = Self::read_u8(reader)?;

        let size_code = Self::read_var_int(reader)?;

        // this will overestimate... may need to reconsider this.
        let mut bytecode: Vec<Opcode> = Vec::with_capacity(size_code as usize);
        let mut index: u32 = 0;
        while index < size_code {
            let opcode = Self::read_opcode(reader, &self.opcode_decoder)?;
            index += if opcode.has_aux() { 2 } else { 1 };
            bytecode.push(opcode);
        }

        let consts = Self::read_consts(reader)?;

        let proto_size = Self::read_var_int(reader)?;
        let mut protos = Vec::new();
        for _ in 0 .. proto_size {
            protos.push(Self::read_var_int(reader)?);
        }

        let debug_line_defined = Self::read_var_int(reader)?;
        let debug_name = Self::read_var_int(reader)?;

        let line_info = Self::read_line_info(reader, &bytecode, size_code)?;

        let debug_info = Self::read_debug_info(reader)?;

        Ok(Function {
            max_stack_size,
            num_params,
            num_up_vals,
            is_vargs,
            size_code,
            bytecode,
            consts,
            debug_info,
            line_info,
            debug_name,
        })
    }

    fn read_consts<R: Read + BufRead>(reader: &mut R) -> Result<Vec<Constant>> {
        let const_count = Self::read_var_int(reader)?;
        let mut const_vec = Vec::with_capacity(const_count as usize);

        for _ in 0..const_count {
            let const_type = Self::read_u8(reader)?;
            const_vec.push(match const_type {
                0 => Constant::Nil,
                1 => Constant::Boolean(Self::read_u8(reader)? != 0),
                2 => Constant::Number(Self::read_f64(reader)?),
                // NOTE: 0 is reserved for nil in strings so indexing starts at 1
                3 => Constant::StringIndex(Self::read_var_int(reader)?),
                4 => {
                    let info = Self::read_u32(reader)?;
                    let count = info >> 30;

                    let id0 = ((info >> 20) & 1023) as u16;
                    let id1 = ((info >> 10) & 1023) as u16;
                    let id2 = (info & 1023) as u16;

                    Constant::Import(match count {
                        1 => ConstImport::One(id0),
                        2 => ConstImport::Two(id0, id1),
                        3 => ConstImport::Three(id0, id1, id2),
                        _ => return Err(Error::Generic("Invalid Const import type!".into()))
                    })
                },
                5 => {
                    let len: u32 = Self::read_var_int(reader)?;
                    let mut keys = Vec::with_capacity(len as usize);
                    for _ in 0..len {
                        keys.push(Self::read_var_int(reader)?);
                    }
                    Constant::Table(ConstTable {
                        len,
                        keys,
                    })
                }
                6 => Constant::Closure(Self::read_var_int(reader)?),
                byte => return Err(Error::InvalidConstant { byte }),
            })
        }

        Ok(const_vec)
    }

    fn read_line_info<R: Read + BufRead>(reader: &mut R, opcodes: &Vec<Opcode>, size_code: u32) -> Result<Option<Vec<i32>>> {
        let has_line_info = Self::read_u8(reader)?;

        match has_line_info {
            0 => Ok(None),
            1 => {
                let line_gap_log_2 = Self::read_u8(reader)?;
                let intervals = ((size_code - 1) >> line_gap_log_2) + 1;

                let mut line_info = Vec::new();
                let mut last_offset = Wrapping(0_u8);

                // what the shit is going on here???? why is it wrapping??
                for _ in 0..size_code {
                    last_offset += Self::read_u8(reader)?;
                    line_info.push(last_offset.0);
                }

                let mut abs_line_info = Vec::with_capacity(intervals as usize);
                let mut last_line = 0_i32;
                for _ in 0..intervals {
                    last_line += Self::read_i32(reader)?;
                    abs_line_info.push(last_line);
                }

                let mut new_line_info = Vec::with_capacity(opcodes.len());
                let mut offset = 0;
                for (index, op) in opcodes.iter().enumerate() {
                    let fake_index = index + offset;
                    new_line_info.push(abs_line_info[(fake_index as i32 >> line_gap_log_2 as i32) as usize] + line_info[fake_index] as i32);
                    if op.has_aux() {
                        offset += 1;
                    }
                }

                Ok(Some(new_line_info))
            }

            byte => Err(Error::ByteToBoolErr { byte })
        }
    }

    fn read_debug_info<R: Read + BufRead>(reader: &mut R) -> Result<Option<DebugInfo>> {
        let has_debug_info = Self::read_u8(reader)?;

        match has_debug_info {
            0 => Ok(None),
            1 => {
                let size_loc_vars = Self::read_var_int(reader)?;
                let mut local_vars = Vec::new();
                for _ in 0..size_loc_vars {
                    local_vars.push(
                        DebugLocalVar {
                            var_name: Self::read_string(reader)?,
                            start_pc: Self::read_var_int(reader)?,
                            end_pc: Self::read_var_int(reader)?,
                            reg: Self::read_u8(reader)?,
                        }
                    )
                }
                let size_up_vals = Self::read_var_int(reader)?;
                let mut up_vals = Vec::new();
                for _ in 0..size_up_vals {
                    up_vals.push(Self::read_string(reader)?);
                }

                Ok(Some(DebugInfo {
                    local_vars,
                    up_vals,
                }))
            },
            _ => Err(Error::Generic("Invalid Debug Info".into()))
        }
    }
}

// i know &Option is an anti pattern but this is genuinely a correct use for it i think...
impl LuauBytecodeReader {
    fn read_opcode<R: Read + BufRead>(reader: &mut R, decoder: &Option<Box<dyn Fn(u8) -> u8>>) -> Result<Opcode> {
        let raw_byte = Self::read_u8(reader)?;

        let byte = match decoder {
            Some(func) => func(raw_byte),
            None => raw_byte,
        };

        let kind: OpcodeKind = match num_traits::FromPrimitive::from_u8(byte) {
            Some(kind) => kind,
            None => return Err(Error::InvalidOpcode { byte })
        };

        let mut args = [0_u8; 3];
        reader.read_exact(&mut args)?;

        let aux = match kind.has_aux() {
            true => Some(Self::read_u32(reader)?),
            false => None,
        };

        Ok(Opcode {
            kind,
            args,
            aux,
        })
    }

    fn read_u8<R: Read + BufRead>(reader: &mut R) -> io::Result<u8> {
        let mut buf = [0_u8; 1];
        reader.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    fn read_var_int<R: Read + BufRead>(reader: &mut R) -> io::Result<u32> {
        let mut result: u32 = 0;
        let mut shift: u32 = 0;
        loop {
            let byte: u8 = Self::read_u8(reader)?;
            result |= (byte as u32 & 127) << shift;
            shift += 7;

            if byte & 128 == 0 {
                break Ok(result);
            }
        }
    }

    fn read_string<R: Read + BufRead>(reader: &mut R) -> io::Result<String> {
        let len = Self::read_var_int(reader)?;
        let mut buf = String::new();
        reader.take(len.into()).read_to_string(&mut buf)?;
        Ok(buf)
    }

    // endianness might not be respected in these! It's probably LE, but could be BE! Double check!
    fn read_i32<R: Read + BufRead>(reader: &mut R) -> io::Result<i32> {
        let mut buf = [0_u8; 4];
        reader.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    fn read_u32<R: Read + BufRead>(reader: &mut R) -> io::Result<u32> {
        let mut buf = [0_u8; 4];
        reader.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    fn read_f64<R: Read + BufRead>(reader: &mut R) -> io::Result<f64> {
        let mut buf = [0_u8; 8];
        reader.read_exact(&mut buf)?;
        Ok(f64::from_ne_bytes(buf))
    }
}
