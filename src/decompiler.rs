use std::{cell::RefCell, collections::HashMap, default, fmt, mem, rc::Rc, result, sync::Arc};

use crate::{bytecode::{self, Bytecode, ConstImport, Constant, Function}, luau_ast::{BinOp, BinOpKind, Binding, Block, Call, Chunk, ConsStringTableIndex, ConstantString, Expression, GenericAssign, GlobalVar, LocalAssign, LocalVar, MultiAssign, NameCall, Return, SetList, Statement, Table, TableIndex, UnOp, UnOpKind}, opcode::{Opcode, OpcodeKind}, };

type UpValue = LocalVar;

const NONE_REG: Option<RegInfo> = None;

const NONE_UPVAL: Option<UpValue> = None;
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Expected constant string, got: {:?}", .cons)]
    ConstantStringIsNotString { cons: Constant },
    #[error("String Index {} does not exist!", .index)]
    StringIndexDoesNotExist { index: u32 },
    #[error("Uhhh why doesn't this have a main function?")]
    MainFunctionDoesNotExist,
    #[error("Dev Error {}", .0)]
    Generic(String),
}

type Result<T> = result::Result<T, Error>;

pub struct Decompiler {
    bytecode: Bytecode,
    options: DecompilerOptions
}

#[derive(Default)]
pub struct DecompilerOptions { }

#[allow(unused)]
struct DecompilerState {
    version: u8,
    main_func_id: u32,
    functions: HashMap<u32, Function>,
    decompiled_funcs: HashMap<u32, Block>,
    strings: Vec<ConstantString>, // if we feel like changing the strings later
    options: DecompilerOptions,
    registers: [Option<RegInfo>; 255],
    upvalues: [Option<LocalVar>; 255],
    temp_binding_num: u32
}

impl DecompilerState {
    fn decompile(mut self) -> Result<Chunk> {
        let mut chunk = Chunk::default();
        let main_func_id = self.main_func_id;
        let func = self.functions.remove(&self.main_func_id)
            .ok_or(Error::MainFunctionDoesNotExist)?;
        chunk.blocks.insert(main_func_id, self.decompile_function(func)?);
        chunk.main_block_id = main_func_id;
        chunk.strings = self.strings;
        Ok(chunk)
    }

    fn decompile_function(&mut self, func: Function) -> Result<Block> {
        let mut block = Block::default();

        let mut i = 0;
        while i < func.bytecode.len() {
            let opcode = &func.bytecode[i];
            match opcode.kind() {
                OpcodeKind::LOP_NOP => eprintln!("Unexpected NOP at {i}! Ignoring..."),
                OpcodeKind::LOP_BREAK => eprintln!("Unexpected breakpoint at {i}! Ignoring..."),

                OpcodeKind::LOP_LOADNIL => self.set_reg(opcode.A(), Expression::Nil, &mut block),
                OpcodeKind::LOP_LOADN => self.set_reg(opcode.A(), Expression::Number(opcode.D()), &mut block),
                OpcodeKind::LOP_LOADB => {
                    let (a, b, c) = opcode.ABC();
                    if c != 0 { panic!("unsupported!") }
                    self.set_reg(opcode.A(), Expression::Bool(opcode.B() == 1), &mut block);
                }
                OpcodeKind::LOP_LOADK => self.set_reg(opcode.A(), Expression::Constant(func.consts[opcode.D() as usize].clone()), &mut block),
                OpcodeKind::LOP_MOVE => self.set_reg(opcode.A(), self.get_reg_var_expr(opcode.B()), &mut block),
                OpcodeKind::LOP_GETGLOBAL => self.set_reg(opcode.A(), Expression::Global(self.get_global(&func.consts, opcode.aux)?), &mut block),
                OpcodeKind::LOP_SETGLOBAL => block.push(Statement::GenericAssign(GenericAssign {
                    var: Expression::Global(self.get_global(&func.consts, opcode.aux)?),
                    expr: self.get_reg_var_expr(opcode.A()),
                })),
                OpcodeKind::LOP_GETUPVAL => todo!(),
                OpcodeKind::LOP_SETUPVAL => todo!(),
                OpcodeKind::LOP_CLOSEUPVALS => todo!(),
                OpcodeKind::LOP_GETIMPORT => {
                    self.set_reg(opcode.A(), Expression::Constant(func.consts[opcode.D() as usize].clone()), &mut block);
                    i += 1;
                    continue;

                    let target_reg = opcode.A();

                    let cons = &func.consts[opcode.D() as usize];
                    let import = match cons {
                        Constant::Import(import) => import,
                        _ => panic!("erm")
                    };

                    match import {
                        ConstImport::One(i1) => {
                            match self.strings.get(*i1 as usize) {
                                Some(string) => panic!("{}", string),
                                None => todo!(""),
                            }
                        }
                        ConstImport::Two(i1, i2) => todo!(),
                        ConstImport::Three(i1, i2, i3) => todo!(),
                    }
                },
                OpcodeKind::LOP_GETTABLE => {
                    let (a, b, c) = opcode.ABC();
                    self.set_reg(a, Expression::TableIndex(TableIndex {
                        table: Box::new(self.get_reg_var_expr(b)),
                        index: Box::new(self.get_reg_var_expr(c)),
                    }), &mut block)
                },
                OpcodeKind::LOP_SETTABLE => {
                    let (a, b, c) = opcode.ABC();
                    block.push(Statement::GenericAssign(GenericAssign {
                        var: Expression::TableIndex(TableIndex {
                            table: Box::new(self.get_reg_var_expr(b)),
                            index: Box::new(self.get_reg_var_expr(c)),
                        }),
                        expr: self.get_reg_var_expr(a)
                    }))
                },
                OpcodeKind::LOP_GETTABLEKS => {
                    let (a, b) = opcode.AB();
                    let aux = opcode.aux.unwrap();

                    self.set_reg(a, Expression::ConstStringTableIndex(ConsStringTableIndex {
                        table: Box::new(self.get_reg_var_expr(b)),
                        str_index: self.get_string(func.consts.get(aux as usize))?
                    }), &mut block)
                },
                OpcodeKind::LOP_SETTABLEKS => {
                    let (a, b) = opcode.AB();
                    let aux = opcode.aux.unwrap();
                    block.push(Statement::GenericAssign(GenericAssign {
                        var: Expression::ConstStringTableIndex(ConsStringTableIndex {
                            table: Box::new(self.get_reg_var_expr(b)),
                            str_index: self.get_string(func.consts.get(aux as usize))?
                        }),
                        expr: self.get_reg_var_expr(a)
                    }))
                },

                OpcodeKind::LOP_GETTABLEN => self.set_reg(opcode.A(), Expression::TableIndex(TableIndex {
                    table: Box::new(self.get_reg_var_expr(opcode.B())),
                    index: Box::new(Expression::Number(opcode.C() as i16 + 1)),
                }), &mut block),

                OpcodeKind::LOP_SETTABLEN => block.push(Statement::GenericAssign(GenericAssign {
                    var: Expression::TableIndex(TableIndex {
                        table: Box::new(self.get_reg_var_expr(opcode.B())),
                        index: Box::new(Expression::Number(opcode.C() as i16 + 1)),
                    }),
                    expr: self.get_reg_var_expr(opcode.A()),
                })),

                OpcodeKind::LOP_NEWCLOSURE => todo!(),
                OpcodeKind::LOP_NAMECALL => {
                    let next_opcode = &func.bytecode[i + 1];
                    match next_opcode.kind() {
                        OpcodeKind::LOP_CALL => {}
                        _ => panic!("bruhhhhhh")
                    };

                    let (a, b) = opcode.AB();
                    let aux = opcode.aux.unwrap();


                    // self.consume_reg(a + 1)?; // consumer the "self" var

                    let args = match next_opcode.B() {
                        0 => todo!(""),
                        1 => panic!("should not be possible!"),
                        2 => Vec::new(),
                        args => {
                            let mut vec = Vec::new();
                            vec.reserve_exact((args as usize) - 1);
                            for i in 2..args {
                                let reg = self.consume_reg(a + i)?;
                                vec.push(Expression::Local(reg.binding))
                            }
                            vec
                        }
                    };

                    let namecall = NameCall {
                        table: Box::new(Expression::Local(self.consume_reg(a)?.binding)),
                        index_call: self.get_string(func.consts.get(aux as usize - 1))?,
                        args,
                    };

                    block.push(match next_opcode.C() {
                        0 => todo!(""),
                        1 => Statement::NameCall(namecall),
                        2 => {
                            let expr = Expression::NameCall(namecall);
                            let local = self.generate_local();
                            self.registers[a as usize] = Some(RegInfo {
                                expr: expr.clone(),
                                binding: local.clone(),
                            });
                            Statement::LocalAssign(LocalAssign {
                                var: local,
                                expr,
                            })
                        },
                        args => {
                            let mut vec = Vec::new();
                            vec.reserve_exact((args as usize) - 1);
                            for i in 0..(args - 1) {
                                let local = self.generate_local();
                                self.registers[(a + i) as usize] = Some(RegInfo {
                                    expr: Expression::Nil,
                                    binding: local.clone(),
                                });

                                vec.push(local)
                            }

                            Statement::MultiAssign(MultiAssign {
                                lhs: vec,
                                rhs: Expression::NameCall(namecall),
                            })
                        }
                    });

                    i += 2;
                    continue;
                }

                OpcodeKind::LOP_CALL => {
                    let (a, b, c) = opcode.ABC();
                    let func = Box::new(Expression::Local(self.consume_reg(a)?.binding));
                    let call = match b {
                        0 => todo!(""),
                        1 => Call {
                            func,
                            args: Vec::new()
                        },
                        args => {
                            let mut vec = Vec::new();
                            vec.reserve_exact((args as usize) - 1);
                            for i in 1..args {
                                let reg = self.consume_reg(a + i)?;
                                vec.push(Expression::Local(reg.binding))
                            }

                            Call {
                                func,
                                args: vec
                            }
                        }
                    };

                    block.push(match c {
                        0 => todo!(""),
                        1 => Statement::Call(call),
                        2 => {
                            let expr = Expression::Call(call);
                            let local = self.generate_local();
                            self.registers[a as usize] = Some(RegInfo {
                                expr: expr.clone(),
                                binding: local.clone(),
                            });
                            Statement::LocalAssign(LocalAssign {
                                var: local,
                                expr,
                            })
                        },
                        args => {
                            let mut vec = Vec::new();
                            vec.reserve_exact((args as usize) - 1);
                            for i in 0..(args - 1) {
                                let local = self.generate_local();
                                self.registers[(a + i) as usize] = Some(RegInfo {
                                    expr: Expression::Nil,
                                    binding: local.clone(),
                                });

                                vec.push(local)
                            }

                            Statement::MultiAssign(MultiAssign {
                                lhs: vec,
                                rhs: Expression::Call(call),
                            })
                        }
                    })
                }

                OpcodeKind::LOP_RETURN => {
                    let (a, b) = opcode.AB();
                    match b {
                        0 => unimplemented!("multret"),
                        1 => block.push(Statement::Return(Return { vars: Vec::new() })),
                        args => {
                            let mut vars = Vec::new();
                            vars.reserve_exact(args as usize);
                            for i in 1..b {
                                vars.push(self.get_reg_var_expr(a + i - 1));
                            }
                            block.push(Statement::Return(Return { vars }))
                        }
                    }
                }

                OpcodeKind::LOP_ADD => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Add, self.get_reg_var_expr(opcode.C()), &mut block),
                OpcodeKind::LOP_SUB => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Minus, self.get_reg_var_expr(opcode.C()), &mut block),
                OpcodeKind::LOP_MUL => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Multiply, self.get_reg_var_expr(opcode.C()), &mut block),
                OpcodeKind::LOP_DIV => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Divide, self.get_reg_var_expr(opcode.C()), &mut block),
                OpcodeKind::LOP_MOD => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Modulo, self.get_reg_var_expr(opcode.C()), &mut block),
                OpcodeKind::LOP_POW => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Power, self.get_reg_var_expr(opcode.C()), &mut block),

                // this is cloned... bad...
                OpcodeKind::LOP_ADDK => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Add, Expression::Constant(func.consts[opcode.C() as usize].clone()), &mut block),
                OpcodeKind::LOP_SUBK => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Minus, Expression::Constant(func.consts[opcode.C() as usize].clone()), &mut block),
                OpcodeKind::LOP_MULK => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Multiply, Expression::Constant(func.consts[opcode.C() as usize].clone()), &mut block),
                OpcodeKind::LOP_DIVK => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Divide, Expression::Constant(func.consts[opcode.C() as usize].clone()), &mut block),
                OpcodeKind::LOP_MODK => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Modulo, Expression::Constant(func.consts[opcode.C() as usize].clone()), &mut block),
                OpcodeKind::LOP_POWK => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Power, Expression::Constant(func.consts[opcode.C() as usize].clone()), &mut block),

                OpcodeKind::LOP_AND => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::And, self.get_reg_var_expr(opcode.C()), &mut block),
                OpcodeKind::LOP_OR => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Or, self.get_reg_var_expr(opcode.C()), &mut block),

                OpcodeKind::LOP_ANDK => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::And, Expression::Constant(func.consts[opcode.C() as usize].clone()), &mut block),
                OpcodeKind::LOP_ORK => self.bin_op(opcode.A(), self.get_reg_var_expr(opcode.B()), BinOpKind::Or, Expression::Constant(func.consts[opcode.C() as usize].clone()), &mut block),

                OpcodeKind::LOP_NOT => self.un_op(opcode.A(), UnOpKind::Not, self.get_reg_var_expr(opcode.B()), &mut block),
                OpcodeKind::LOP_MINUS => self.un_op(opcode.A(), UnOpKind::Negative, self.get_reg_var_expr(opcode.B()), &mut block),
                OpcodeKind::LOP_LENGTH => self.un_op(opcode.A(), UnOpKind::Length, self.get_reg_var_expr(opcode.B()), &mut block),

                OpcodeKind::LOP_CONCAT => {
                    let (a, b, c) = opcode.ABC();
                    let mut expr = self.get_reg_var_expr(b);
                    for reg in (b+1)..=c {
                        expr = Expression::BinOp(BinOp {
                            lhs: Box::new(expr),
                            op: BinOpKind::Concat,
                            rhs: Box::new(self.get_reg_var_expr(reg))
                        })
                    }

                    self.set_reg(a, expr, &mut block);
                },

                OpcodeKind::LOP_NEWTABLE => self.set_reg(opcode.A(), Expression::Table(Table::empty()), &mut block),
                OpcodeKind::LOP_DUPTABLE => {
                    self.set_reg(opcode.A(), Expression::Constant(func.consts[opcode.D() as usize].clone()), &mut block)
                },

                OpcodeKind::LOP_SETLIST => {
                    let (a, b, c) = opcode.ABC();
                    let aux = opcode.aux.unwrap();
                    match c {
                        0 => todo!(),
                        1 => panic!("shouldnt happen"),
                        args => {
                            let mut vec = Vec::new();
                            vec.reserve_exact(args as usize - 1);
                            for i in 0..(args - 1) {
                                vec.push(Expression::Local(self.consume_reg(b + i)?.binding))
                            }

                            if aux == 0 {
                                self.set_reg(a, Expression::Table(Table(vec)), &mut block)
                            } else {
                                return Err(Error::Generic("Unsupported aux =/= 0 rn!".into()))
                            }
                        }
                    }
                }

                OpcodeKind::LOP_PREPVARARGS => {/* absolutely nothing! */}


                thing => panic!("{:?}", thing)
                // LOP_JUMP, LOP_JUMPBACK, LOP_JUMPIF, LOP_JUMPIFNOT, LOP_JUMPIFEQ, LOP_JUMPIFLE, LOP_JUMPIFLT, LOP_JUMPIFNOTEQ, LOP_JUMPIFNOTLE, LOP_JUMPIFNOTLT,
                // LOP_ADD, LOP_SUB, LOP_MUL, LOP_DIV, LOP_MOD, LOP_POW,
                // LOP_ADDK, LOP_SUBK, LOP_MULK, LOP_DIVK, LOP_MODK, LOP_POWK,
                // LOP_AND, LOP_OR,
                // LOP_ANDK, LOP_ORK,
                // LOP_CONCAT,
                // LOP_NOT,
                // LOP_MINUS,
                // LOP_LENGTH,
                // LOP_NEWTABLE,
                // LOP_DUPTABLE,
                // LOP_SETLIST,
                // LOP_FORNPREP, LOP_FORNLOOP, LOP_FORGLOOP,
                // LOP_FORGPREP_INEXT,
                // LOP_DEP_FORGLOOP_INEXT,
                // LOP_FORGPREP_NEXT,
                // LOP_DEP_FORGLOOP_NEXT,
                // LOP_GETVARARGS,
                // LOP_DUPCLOSURE,
                // LOP_PREPVARARGS,
                // LOP_LOADKX,
                // LOP_JUMPX,
                // LOP_FASTCALL,
                // LOP_COVERAGE,
                // LOP_CAPTURE,
                // LOP_DEP_JUMPIFEQK, LOP_DEP_JUMPIFNOTEQK,
                // LOP_FASTCALL1,
                // LOP_FASTCALL2,
                // LOP_FASTCALL2K,
                // LOP_FORGPREP,
                // LOP_JUMPXEQKNIL, LOP_JUMPXEQKB, LOP_JUMPXEQKN, LOP_JUMPXEQKS,
            }
            i += 1;
        }

        block.consts = func.consts;
        Ok(block)
    }

    fn bin_op(&mut self, reg: u8, lhs: Expression, op: BinOpKind, rhs: Expression, block: &mut Block) {
        self.set_reg(reg, Expression::BinOp(BinOp {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }), block)
    }

    fn un_op(&mut self, reg: u8, op: UnOpKind, expr: Expression, block: &mut Block) {
        self.set_reg(reg, Expression::UnOp(UnOp {
            value: Box::new(expr),
            op,
        }), block)
    }

    // TODO: fix this it has unwrap
    fn get_global(&self, consts: &[Constant], aux: Option<u32>) -> Result<GlobalVar> {
        Ok(GlobalVar(self.get_string(consts.get(aux.unwrap() as usize))?))
    }

    fn get_string(&self, cons: Option<&Constant>) -> Result<ConstantString> {
        let str_index = match cons {
            Some(Constant::StringIndex(index)) => index,
            // TODO: Fix this it can't be good
            Some(cons) => return Err(Error::ConstantStringIsNotString { cons: cons.clone()}),
            None => return Err(Error::Generic("Invalid Constant!".into()))
        };

        match self.strings.get(*str_index as usize - 1) {
            Some(string) => Ok(string.clone()),
            None => Err(Error::StringIndexDoesNotExist { index: *str_index })
        }
    }

    fn generate_local(&mut self) -> LocalVar {
        self.temp_binding_num += 1;
        LocalVar::new(Binding {
            name: format!("var{}", self.temp_binding_num),
            ty: None,
        })
    }

    fn consume_reg(&mut self, reg: u8) -> Result<RegInfo> {
        match self.registers[reg as usize].take() {
            Some(reg) => Ok(reg),
            None => panic!("test")
        }
    }

    // TODO: fix these
    fn get_reg_val(&self, reg: u8) -> Expression {
        self.registers[reg as usize].as_ref().unwrap().expr.clone()
    }

    // fixme
    fn get_reg_var(&self, reg: u8) -> LocalVar {
        self.registers[reg as usize].as_ref().unwrap().binding.clone()
    }

    // fixme
    fn get_reg_var_expr(&self, reg: u8) -> Expression {
        Expression::Local((self.registers[reg as usize]).as_ref().unwrap().binding.clone())
    }

    fn set_reg(&mut self, reg: u8, expr: Expression, block: &mut Block) {
        match &mut self.registers[reg as usize] {
            Some(state) => {
                state.expr = expr.clone();
                block.push(Statement::LocalAssign(LocalAssign {
                    var: state.binding.clone(),
                    expr,
                }))
            },
            None => {
                let local = self.generate_local();
                self.registers[reg as usize] = Some(RegInfo {
                    expr: expr.clone(),
                    binding: local.clone(),
                });

                block.push(Statement::LocalAssign(LocalAssign {
                    var: local,
                    expr,
                }))
            }
        };
    }
}

impl Decompiler {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            bytecode,
            options: Default::default(),
        }
    }

    pub fn with_options(self, options: DecompilerOptions) -> Self {
        Self {
            options,
            ..self
        }
    }

    pub fn decompile(self) -> Result<Chunk> {
        let bytecode = self.bytecode;
        DecompilerState {
            version: bytecode.version,
            main_func_id: bytecode.main_func_id,
            functions: bytecode.functions,
            decompiled_funcs: HashMap::new(),
            strings: bytecode.strings.into_iter()
                .map(ConstantString::new)
                .collect(),
            options: self.options,
            registers: [NONE_REG; 255],
            upvalues: [NONE_UPVAL; 255],
            temp_binding_num: 0,
        }.decompile()
    }
}

struct RegInfo {
    binding: LocalVar,
    expr: Expression
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader, path::PathBuf};

    use crate::{bytecode_reader::{self, LuauBytecodeReader}, decompiler::Decompiler, serializer::Serializer};


    const TEST_PATH: &str = "tests";

    #[test]
    fn it_all_works() -> bytecode_reader::Result<()> {
        let entries = std::fs::read_dir(TEST_PATH).expect("Tests should exist!");
        for entry in entries.into_iter() {
            let mut path = entry.expect("Each entry should exist!").path();
            path.push("output");
            path.set_extension("out");
            println!("Decompiling {:?}", &path);
            let reader = BufReader::new(File::open(path)?);
            let bytecode_reader = LuauBytecodeReader::new();
            let proto = bytecode_reader.read(reader)?;
            println!("Bytecode Read!");
            let decomp = Decompiler::new(proto);
            let chunk = decomp.decompile().unwrap();
            println!("Decomp Success!");
            let ser = Serializer {
                chunk,
                locals: Vec::new(),
                indent_level: 0,
            };
            let mut buf = Vec::new();
            ser.write(&mut buf).unwrap();
            println!("Everything normal!");
        }
        Ok(())
    }
}
