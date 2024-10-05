// copied off of github lmao

use std::{borrow::Cow, cell::RefCell, collections::{HashMap, HashSet}, fmt::{Display, Write}, io, rc::Rc};

use crate::{bytecode::Constant, bytecode_reader::Result};

#[derive(Debug, Clone)]
pub struct ConstantString(Rc<RefCell<String>>);

impl ConstantString {
    pub (crate) fn new(string: String) -> Self {
        Self(Rc::new(RefCell::new(string)))
    }
}

impl PartialEq for ConstantString {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Display for ConstantString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.borrow())
    }
}

// Represents the ENTIRE decompiled thing
#[derive(Debug, Clone, Default)]
pub struct Chunk {
    pub version: u8,
    pub main_block_id: u32,
    pub blocks: HashMap<u32, Block>,
    pub strings: Vec<ConstantString>,
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub body: Vec<Statement>,
    pub consts: Vec<Constant>,
}

impl Block {
    pub (crate) fn push(&mut self, stmt: Statement) {
        self.body.push(stmt)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Call(Call),
    NameCall(NameCall),
    LocalAssign(LocalAssign),
    GenericAssign(GenericAssign),
    MultiAssign(MultiAssign),
    MultiMultiAssign(MultiMultiAssign),
    If(If),
    While(While),
    ForN(ForN),
    ForIn(ForIn),
    RepeatUntil(RepeatUntil),
    Break,
    Continue,
    Return(Return),

    // usually SETLIST == LocalAssign, but if it already exists we'll just use this
    BackUpSetList(SetList),
}

#[derive(Debug, Clone)]
pub struct NameCall {
    pub table: Box<Expression>,
    pub index_call: ConstantString,
    pub args: Vec<Expression>
}

#[derive(Debug, Clone)]
pub struct SetList {
    pub starting_index: u32,
    pub target_reg: LocalVar,
    pub source_regs: Vec<Expression>,
}

// local 1, 2, 3 = a()
#[derive(Debug, Clone)]
pub struct MultiAssign {
    pub lhs: Vec<LocalVar>,
    pub rhs : Expression,
}

// local 1, 2, 3 = a()
#[derive(Debug, Clone)]
pub struct MultiMultiAssign {
    pairs: Vec<(LocalVar, Expression)>
}

#[derive(Debug, Clone)]
pub struct LocalAssign {
    pub var: LocalVar,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct GenericAssign {
    pub var: Expression,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct If {}

#[derive(Debug, Clone)]
pub struct While {}

#[derive(Debug, Clone)]
pub struct ForN {}

#[derive(Debug, Clone)]
pub struct ForIn {}

#[derive(Debug, Clone)]
pub struct RepeatUntil {
    pub condition: Expression,
    pub body: Box<Block>
}

#[derive(Debug, Clone)]
pub struct Return {
    pub vars: Vec<Expression>
}

#[derive(Debug, Clone)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(i16),
    Constant(Constant),
    Global(GlobalVar),
    Local(LocalVar),
    TableIndex(TableIndex),
    ConstStringTableIndex(ConsStringTableIndex),
    Varargs,
    Call(Call),
    NameCall(NameCall),
    // StringInterp(StringInterp),
    BinOp(BinOp),
    UnOp(UnOp),
    Closure(Closure),
    Table(Table),
    // TypeAssertion(Box<TypeAssertion>),
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct LocalVar(Rc<Binding>);

impl Display for LocalVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.name.as_str())
    }
}

impl LocalVar {
    pub (crate) fn new(binding: Binding) -> Self {
        Self(Rc::new(binding))
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}

impl PartialEq for LocalVar {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVar(pub ConstantString);

#[derive(Debug, Clone, Hash)]
pub struct Type {}

#[derive(Debug, Clone)]
pub struct Call {
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone, Default)]
pub struct Table(pub Vec<Expression>);

impl Table {
    pub (crate) fn empty() -> Self {
        Self::default()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub (crate) struct TypeAssertion { }


#[derive(Debug, Clone, PartialEq)]
pub (crate) enum BinOpKind {
    Add,
    Minus,
    Multiply,
    Divide,
    FloorDivide,

    Power,
    Modulo,

    LessThan,
    LessThanOrEq,
    MoreThan,
    MoreThanOrEq,

    And,
    Or,

    // i will treat it as a bin op and no one can stop me
    Concat,
}

impl Display for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::Minus => write!(f, "-"),
            BinOpKind::Multiply => write!(f,"*"),
            BinOpKind::Divide => write!(f,"/"),
            BinOpKind::FloorDivide => write!(f,"//"),
            BinOpKind::Power => write!(f,"^"),
            BinOpKind::Modulo => write!(f,"%"),
            BinOpKind::LessThan => write!(f,"<"),
            BinOpKind::LessThanOrEq => write!(f,"<="),
            BinOpKind::MoreThan => write!(f,">"),
            BinOpKind::MoreThanOrEq => write!(f,">="),
            BinOpKind::And => write!(f,"and"),
            BinOpKind::Or => write!(f,"or"),
            BinOpKind::Concat => write!(f, "..")
        }
    }
}

#[derive(Debug, Clone)]
pub (crate) struct TableIndex {
    pub table: Box<Expression>,
    pub index: Box<Expression>
}

#[derive(Debug, Clone)]
pub (crate) struct ConsStringTableIndex {
    pub table: Box<Expression>,
    pub str_index: ConstantString
}

#[derive(Debug, Clone)]
pub (crate) struct BinOp {
    pub lhs: Box<Expression>,
    pub op: BinOpKind,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub (crate) enum UnOpKind {
    Not,
    Length,
    Negative,
}

impl Display for UnOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOpKind::Not => write!(f, "not"),
            UnOpKind::Length => write!(f, "#"),
            UnOpKind::Negative => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone)]
pub (crate) struct UnOp {
    pub value: Box<Expression>,
    pub op: UnOpKind,
}

#[derive(Debug, Clone)]
pub struct Closure {}
