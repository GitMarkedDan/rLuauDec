use std::collections::HashMap;

use crate::opcode::Opcode;

const MAX_CONST_TABLE_LEN: usize = 32;

#[derive(Debug)]
pub struct Bytecode {
    pub version: u8,
    pub strings: Vec<String>,
    pub functions: HashMap<u32, Function>,
    pub main_func_id: u32,
}

#[derive(Debug)]
pub struct Function {
    pub max_stack_size: u8,
    pub num_params: u8,
    pub num_up_vals: u8,
    pub is_vargs: u8,
    pub size_code: u32,
    pub bytecode: Vec<Opcode>,
    pub consts: Vec<Constant>,
    pub debug_info: Option<DebugInfo>,
    pub line_info: Option<Vec<i32>>,
    pub debug_name: u32 // index to constant table
}

#[derive(Debug, Clone)]
pub enum Constant {
    Nil,
    Boolean(bool),
    Number(f64),
    StringIndex(u32),
    Import(ConstImport),
    Table(ConstTable),
    Closure(u32),
}

#[derive(Debug, Clone)]
pub enum ConstImport {
    One(u16),
    Two(u16, u16),
    Three(u16, u16, u16)
}

#[derive(Debug, Clone)]
pub struct ConstTable {
    pub len: u32,
    pub keys: Vec<u32>,
}

#[derive(Debug)]
pub struct DebugInfo {
    pub local_vars: Vec<DebugLocalVar>,
    pub up_vals: Vec<String>
}

#[derive(Debug)]
pub struct DebugLocalVar {
    pub var_name: String,
    pub start_pc: u32,
    pub end_pc: u32,
    pub reg: u8
}
