use std::io::{BufRead, Read};

use num::FromPrimitive;
use num_derive::FromPrimitive;

#[derive(Debug, Clone)]
pub (crate) struct Opcode {
    pub kind: OpcodeKind,
    pub args: [u8; 3],
    pub aux: Option<u32>,
}

impl Opcode {
    pub fn kind(&self) -> OpcodeKind {
        self.kind
    }

    pub (crate) fn has_aux(&self) -> bool {
        self.aux.is_some()
    }

    #[allow(non_snake_case)]
    pub (crate) fn A(&self) -> u8 {
        self.args[0]
    }

    #[allow(non_snake_case)]
    pub (crate) fn B(&self) -> u8 {
        self.args[1]
    }

    #[allow(non_snake_case)]
    pub (crate) fn C(&self) -> u8 {
        self.args[2]
    }

    #[allow(non_snake_case)]
    pub (crate) fn D(&self) -> i16 {
        (((self.args[2] as u16) << 8) + (self.args[1] as u16)) as i16
    }

    #[allow(non_snake_case)]
    pub (crate) fn E(&self) -> i32 {
        ((self.args[2] as i32) << 16) + ((self.args[1] as i32) << 8) + (self.args[0] as i32)
    }

    #[allow(non_snake_case)]
    pub (crate) fn AB(&self) -> (u8, u8) {
        (self.args[0], self.args[1])
    }

    #[allow(non_snake_case)]
    pub (crate) fn ABC(&self) -> (u8, u8, u8) {
        self.args.into()
    }

    #[allow(non_snake_case)]
    pub (crate) fn AD(&self) -> (u8, i16) {
        (self.args[0], self.D())
    }
}

pub enum OpcodeArgKind { Empty, A, AB, AC, ABC, D, AD, E }

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, FromPrimitive)]
pub (crate) enum OpcodeKind {
    LOP_NOP = 0,
    LOP_BREAK,
    LOP_LOADNIL, LOP_LOADB, LOP_LOADN, LOP_LOADK,
    LOP_MOVE,
    LOP_GETGLOBAL, LOP_SETGLOBAL,
    LOP_GETUPVAL, LOP_SETUPVAL,
    LOP_CLOSEUPVALS,
    LOP_GETIMPORT,
    LOP_GETTABLE, LOP_SETTABLE,
    LOP_GETTABLEKS, LOP_SETTABLEKS,
    LOP_GETTABLEN, LOP_SETTABLEN,
    LOP_NEWCLOSURE,
    LOP_NAMECALL,
    LOP_CALL,
    LOP_RETURN, LOP_JUMP, LOP_JUMPBACK, LOP_JUMPIF, LOP_JUMPIFNOT, LOP_JUMPIFEQ, LOP_JUMPIFLE, LOP_JUMPIFLT, LOP_JUMPIFNOTEQ, LOP_JUMPIFNOTLE, LOP_JUMPIFNOTLT,
    LOP_ADD, LOP_SUB, LOP_MUL, LOP_DIV, LOP_MOD, LOP_POW,
    LOP_ADDK, LOP_SUBK, LOP_MULK, LOP_DIVK, LOP_MODK, LOP_POWK,
    LOP_AND, LOP_OR,
    LOP_ANDK, LOP_ORK,
    LOP_CONCAT,
    LOP_NOT,
    LOP_MINUS,
    LOP_LENGTH,
    LOP_NEWTABLE,
    LOP_DUPTABLE,
    LOP_SETLIST,
    LOP_FORNPREP, LOP_FORNLOOP, LOP_FORGLOOP,
    LOP_FORGPREP_INEXT,
    LOP_DEP_FORGLOOP_INEXT,
    LOP_FORGPREP_NEXT,
    LOP_DEP_FORGLOOP_NEXT,
    LOP_GETVARARGS,
    LOP_DUPCLOSURE,
    LOP_PREPVARARGS,
    LOP_LOADKX,
    LOP_JUMPX,
    LOP_FASTCALL,
    LOP_COVERAGE,
    LOP_CAPTURE,
    LOP_DEP_JUMPIFEQK, LOP_DEP_JUMPIFNOTEQK,
    LOP_FASTCALL1,
    LOP_FASTCALL2,
    LOP_FASTCALL2K,
    LOP_FORGPREP,
    LOP_JUMPXEQKNIL, LOP_JUMPXEQKB, LOP_JUMPXEQKN, LOP_JUMPXEQKS,
}

impl OpcodeKind {
    pub (crate) const fn has_aux(&self) -> bool {
        matches!(self, OpcodeKind::LOP_LOADKX |
            Self::LOP_NEWTABLE |
            Self::LOP_GETGLOBAL |
            Self::LOP_SETGLOBAL |
            Self::LOP_GETTABLEKS |
            Self::LOP_SETTABLEKS |
            Self::LOP_NAMECALL |
            Self::LOP_SETLIST |
            Self::LOP_FASTCALL2 |
            Self::LOP_FASTCALL2K |
            Self::LOP_GETIMPORT |
            Self::LOP_JUMPIFEQ |
            Self::LOP_JUMPIFLE |
            Self::LOP_JUMPIFLT |
            Self::LOP_JUMPIFNOTEQ |
            Self::LOP_JUMPIFNOTLE |
            Self::LOP_JUMPIFNOTLT |
            Self::LOP_FORGLOOP |
            Self::LOP_JUMPXEQKNIL |
            Self::LOP_JUMPXEQKB |
            Self::LOP_JUMPXEQKN |
            Self::LOP_JUMPXEQKS
        )
    }

    pub (crate) const fn arg_kind(&self) -> OpcodeArgKind {
        match self {
            Self::LOP_NOP |
            Self::LOP_BREAK |
            Self::LOP_DEP_FORGLOOP_NEXT |
            Self::LOP_DEP_JUMPIFEQK |
            Self::LOP_DEP_JUMPIFNOTEQK |
            Self::LOP_DEP_FORGLOOP_INEXT => OpcodeArgKind::Empty,

            // A
            Self::LOP_LOADNIL |
            Self::LOP_CLOSEUPVALS |
            Self::LOP_FORGPREP_INEXT |
            Self::LOP_PREPVARARGS |
            Self::LOP_FORGPREP_NEXT |
            // aux
            Self::LOP_LOADKX => OpcodeArgKind::A,

            // AB
            Self::LOP_MOVE |
            Self::LOP_GETUPVAL |
            Self::LOP_SETUPVAL |
            Self::LOP_RETURN |
            Self::LOP_NOT |
            Self::LOP_MINUS |
            Self::LOP_LENGTH |
            Self::LOP_GETVARARGS |
            Self::LOP_CAPTURE |
            // aux
            Self::LOP_NEWTABLE => OpcodeArgKind::AB,

            // AC
            Self::LOP_FASTCALL |
            // aux
            Self::LOP_SETGLOBAL |
            Self::LOP_GETGLOBAL => OpcodeArgKind::AC,

            // ABC
            Self::LOP_LOADB |
            Self::LOP_GETTABLE |
            Self::LOP_SETTABLE |
            Self::LOP_GETTABLEN |
            Self::LOP_SETTABLEN |
            Self::LOP_CALL |
            Self::LOP_ADD |
            Self::LOP_SUB |
            Self::LOP_MUL |
            Self::LOP_DIV |
            Self::LOP_MOD |
            Self::LOP_POW |
            Self::LOP_ADDK |
            Self::LOP_SUBK |
            Self::LOP_MULK |
            Self::LOP_DIVK |
            Self::LOP_MODK |
            Self::LOP_POWK |
            Self::LOP_AND |
            Self::LOP_OR |
            Self::LOP_ANDK |
            Self::LOP_ORK |
            Self::LOP_CONCAT |
            Self::LOP_FASTCALL1 |
            // aux
            Self::LOP_GETTABLEKS |
            Self::LOP_SETTABLEKS |
            Self::LOP_NAMECALL |
            Self::LOP_SETLIST |
            Self::LOP_FASTCALL2 |
            Self::LOP_FASTCALL2K => OpcodeArgKind::ABC,

            // AD
            Self::LOP_LOADN |
            Self::LOP_LOADK |
            Self::LOP_NEWCLOSURE |
            Self::LOP_JUMPIF |
            Self::LOP_JUMPIFNOT |
            Self::LOP_DUPTABLE |
            Self::LOP_FORNPREP |
            Self::LOP_FORNLOOP |
            Self::LOP_DUPCLOSURE |
            Self::LOP_FORGPREP |
            // aux
            Self::LOP_GETIMPORT |
            Self::LOP_JUMPIFEQ |
            Self::LOP_JUMPIFLE |
            Self::LOP_JUMPIFLT |
            Self::LOP_JUMPIFNOTEQ |
            Self::LOP_JUMPIFNOTLE |
            Self::LOP_JUMPIFNOTLT |
            Self::LOP_FORGLOOP |
            Self::LOP_JUMPXEQKNIL |
            Self::LOP_JUMPXEQKB |
            Self::LOP_JUMPXEQKN |
            Self::LOP_JUMPXEQKS => OpcodeArgKind::AD,

            // D
            Self::LOP_JUMP |
            Self::LOP_JUMPBACK => OpcodeArgKind::D,

            // E
            Self::LOP_JUMPX |
            Self::LOP_COVERAGE => OpcodeArgKind::E,
        }
    }
}
