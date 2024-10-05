use std::{borrow::Cow, error, fmt::Display, io::{self, Write}, rc::Rc, result};

use crate::{bytecode::{ConstImport, ConstTable, Constant}, luau_ast::{BinOp, Block, Call, Chunk, ConsStringTableIndex, ConstantString, Expression, GenericAssign, GlobalVar, LocalAssign, LocalVar, NameCall, SetList, Statement, Table, TableIndex, UnOp}};


pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Error {
    Io(Rc<io::Error>),
    Generic(String),
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Error::Io(Rc::new(value))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl error::Error for Error { }

pub struct Serializer {
    pub chunk: Chunk,
    pub locals: Vec<LocalVar>,
    pub indent_level: u8,
}

impl Serializer {
    pub fn write<W: Write>(mut self, f: &mut W) -> Result<()> {
        let block = self.chunk.blocks.remove(&self.chunk.main_block_id).unwrap();
        self.write_block(f, &block)
    }

    pub fn write_block<W: Write>(&mut self, f: &mut W, block: &Block) -> Result<()> {
        let mut i = 0;
        while i < block.body.len() {
            let stmt = &block.body[i];
            match stmt {
                Statement::Call(call) => write!(f, "{}", self.fmt_call(call, block)?)?,
                Statement::NameCall(namecall) => write!(f, "{}", self.fmt_namecall(namecall, block)?)?,
                Statement::LocalAssign(loc_assign) => self.write_local_assign(f, loc_assign, block)?,
                Statement::MultiAssign(_) => todo!(),
                Statement::MultiMultiAssign(_) => todo!(),
                Statement::GenericAssign(assign) => self.write_assign(f, assign, block)?,
                Statement::If(_) => todo!(),
                Statement::While(_) => todo!(),
                Statement::ForN(_) => todo!(),
                Statement::ForIn(_) => todo!(),
                Statement::RepeatUntil(_) => todo!(),
                Statement::Break => write!(f, "break")?,
                Statement::Continue => write!(f, "continue")?,
                Statement::Return(ret) => write!(f, "return {}", self.fmt_arg_list(&ret.vars, block)?)?,
                Statement::BackUpSetList(list) => self.write_set_list(f, list, block)?
                // stmt => todo!("{:?}", stmt)
            };

            f.write_all(b"\n")?;
            i += 1;
        }
        Ok(())
    }
}

impl Serializer {
    fn fmt_expr(&self, expr: &Expression, block: &Block) -> Result<Cow<str>> {
        Ok(match expr {
            Expression::Nil => Cow::Borrowed(self.fmt_nil()),
            Expression::Varargs => Cow::Borrowed(self.fmt_varargs()),

            Expression::Number(num) => self.fmt_i16(*num),
            Expression::Bool(bool) => self.fmt_bool(*bool),

            Expression::Local(local) => self.fmt_local(local, block),
            Expression::Global(global) => self.fmt_global(global, block),

            Expression::Table(table) => self.fmt_table(table, block)?,
            Expression::TableIndex(index) => self.fmt_index(index, block)?,
            Expression::ConstStringTableIndex(index) => self.fmt_const_index(index, block)?,

            Expression::UnOp(op) => self.fmt_un_op(op, block)?,
            Expression::BinOp(op) => self.fmt_bin_op(op, block)?,

            Expression::Call(call) => self.fmt_call(call, block)?,
            Expression::NameCall(namecall) => self.fmt_namecall(namecall, block)?,

            Expression::Constant(cons) => self.get_const(cons, block)?,

            Expression::Closure(_) => todo!(),
        })
    }

    fn get_const(&self, cons: &Constant, block: &Block) -> Result<Cow<str>> {
        let str = match cons {
            Constant::Nil => Cow::Borrowed(self.fmt_nil()),
            Constant::Boolean(bool) => self.fmt_bool(*bool),
            Constant::Number(num) => self.fmt_f64(*num),
            Constant::StringIndex(index) => self.fmt_string_literal(&self.string_from_index(*index as usize - 1)?),
            Constant::Import(import) => self.fmt_import(import, block)?,
            Constant::Table(ConstTable) => Cow::Borrowed("{}"),
            Constant::Closure(_) => todo!(),
        };

        Ok(str)
    }

    fn write_local_assign<W: Write>(&mut self, f: &mut W, loc_assign: &LocalAssign, block: &Block) -> Result<()> {
        match self.locals.contains(&loc_assign.var) {
            true => write!(f, "{} = {}", &loc_assign.var, self.fmt_expr(&loc_assign.expr, block)?)?,
            false => {
                write!(f, "local {} = {}", &loc_assign.var, self.fmt_expr(&loc_assign.expr, block)?)?;
                self.locals.push(loc_assign.var.clone());
            },
        };
        Ok(())
    }

    fn write_set_list<W: Write>(&mut self, f: &mut W, list: &SetList, block: &Block) -> Result<()> {
        write!(f, "{{{}}}", self.fmt_arg_list(&list.source_regs, block)?);
        Ok(())
    }

    fn write_assign<W: Write>(&mut self, f: &mut W, assign: &GenericAssign, block: &Block) -> Result<()> {
        write!(f, "{} = {}", self.fmt_expr(&assign.var, block)?, self.fmt_expr(&assign.expr, block)?)?;
        Ok(())
    }
}

impl Serializer {
    const fn fmt_nil(&self) -> &'static str {
        "nil"
    }

    const fn fmt_varargs(&self) -> &'static str {
        "..."
    }

    const fn fmt_bool(&self, bool: bool) -> Cow<str> {
        Cow::Borrowed(if bool { "true" } else { "false" })
    }

    fn fmt_f64(&self, num: f64) -> Cow<str> {
        num.to_string().into()
    }

    fn fmt_i16(&self, num: i16) -> Cow<str> {
        num.to_string().into()
    }

    fn fmt_string_literal(&self, string: &str) -> Cow<str> {
        format!("\"{}\"", string).into()
    }

    fn fmt_local(&self, local: &LocalVar, block: &Block) -> Cow<str> {
        local.name().to_string().into()
    }

    fn fmt_global(&self, global: &GlobalVar, block: &Block) -> Cow<str> {
        global.0.to_string().into()
    }

    fn fmt_un_op(&self, op: &UnOp, block: &Block) -> Result<Cow<str>> {
        Ok(format!("{}{}", op.op, self.fmt_expr(&op.value, block)?).into())
    }

    fn fmt_bin_op(&self, op: &BinOp, block: &Block) -> Result<Cow<str>> {
        Ok(format!("{} {} {}", self.fmt_expr(&op.lhs, block)?, op.op, self.fmt_expr(&op.rhs, block)?).into())
    }

    fn fmt_table(&self, table: &Table, block: &Block) -> Result<Cow<str>> {
        Ok(format!("{{{}}}", self.fmt_arg_list(&table.0, block)?).into())
    }

    fn fmt_index(&self, index: &TableIndex, block: &Block) -> Result<Cow<str>> {
        Ok(format!("{}[{}]", self.fmt_expr(&index.table, block)?, self.fmt_expr(&index.index, block)?).into())
    }

    fn fmt_const_index(&self, index: &ConsStringTableIndex, block: &Block) -> Result<Cow<str>> {
        Ok(format!("{}.{}", self.fmt_expr(&index.table, block)?, index.str_index).into())
    }

    fn fmt_call(&self, call: &Call, block: &Block) -> Result<Cow<str>> {
        Ok(format!("{}({})", self.fmt_expr(&call.func, block)?, self.fmt_arg_list(&call.args, block)?).into())
    }

    fn fmt_namecall(&self, namecall: &NameCall, block: &Block) -> Result<Cow<str>> {
        let string = format!("{}:{}({})", self.fmt_expr(&namecall.table, block)?, namecall.index_call, self.fmt_arg_list(&namecall.args, block)?);
        Ok(Cow::Owned(string))
    }

    fn fmt_arg_list(&self, args: &[Expression], block: &Block) -> Result<Cow<str>> {
        if !args.is_empty() {
            let mut out = String::new();
            for arg in args.iter() {
                out.push_str(&format!("{}, ", self.fmt_expr(arg, block)?))
            }
            out.pop();
            out.pop();
            Ok(Cow::Owned(out))
        } else {
            Ok(Cow::Borrowed(""))
        }
    }

    fn fmt_import(&self, import: &ConstImport, block: &Block) -> Result<Cow<str>> {
        let string = match import {
            ConstImport::One(i1) => self.get_const_string(*i1, block)?.into(),
            ConstImport::Two(i1, i2) => format!("{}.{}", self.get_const_string(*i1, block)?, self.get_const_string(*i2, block)?),
            ConstImport::Three(i1, i2, i3) => format!("{}.{}.{}", self.get_const_string(*i1, block)?, self.get_const_string(*i2, block)?, self.get_const_string(*i3, block)?),
        };

        Ok(Cow::Owned(string))
    }
}

impl Serializer {
    fn get_const_string(&self, index: u16, block: &Block) -> Result<String> {
        let index = match block.consts[index as usize].clone() {
            Constant::StringIndex(index) => index,
            _ => return Err(Error::Generic("Should be constant string!".into())),
        };

        Ok(self.string_from_index(index as usize - 1)?)
    }

    fn string_from_index(&self, str_index: usize) -> Result<String> {
        match self.chunk.strings.get(str_index) {
            Some(str) => Ok(str.to_string()),
            None => Err(Error::Generic(format!("{}", str_index))),
        }
    }

}
