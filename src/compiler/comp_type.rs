use std::{cell::OnceCell, collections::HashMap, sync::Arc};

use crate::arch::uarch;

use super::Constant;

#[derive(PartialEq, Eq, Clone)]
pub enum MaybeUnknownType<'a> {
    UnknownInteger {
        min_bits: u8,
        signed: bool,
    },
    UnknownFloat,
    AnonymousStruct {
        fields: Vec<(&'a str, MaybeUnknownType<'a>)>,
    },
    Known(CompType<'a>),
    Infer,
}

impl<'a> MaybeUnknownType<'a> {
    pub fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::UnknownInteger {
                    min_bits: b1,
                    signed: s1,
                },
                Self::UnknownInteger {
                    min_bits: b2,
                    signed: s2,
                },
            ) => b1 <= b2 && (s1 == s2 || !s2),
            (Self::UnknownInteger { min_bits, signed }, Self::Known(CompType::Number(num)))
            | (Self::Known(CompType::Number(num)), Self::UnknownInteger { min_bits, signed }) => {
                (num.is_signed_int() == *signed || !signed && !num.is_float())
                    && num.bits() >= *min_bits
            }

            (Self::UnknownFloat, Self::UnknownFloat) => true,
            (Self::Known(CompType::Number(num)), Self::UnknownFloat)
            | (Self::UnknownFloat, Self::Known(CompType::Number(num))) => num.is_float(),

            (Self::AnonymousStruct { fields: f1 }, Self::AnonymousStruct { fields: f2 }) => {
                f1.len() != f2.len()
                    && f2
                        .iter()
                        .all(|(n1, t1)| f1.iter().any(|(n2, t2)| n1 == n2 && t1 == t2))
            }
            (Self::AnonymousStruct { fields: f1 }, Self::Known(CompType::Struct(ptr)))
            | (Self::Known(CompType::Struct(ptr)), Self::AnonymousStruct { fields: f1 }) => {
                f1.len() != ptr.0.fields.len()
                    && f1.iter().all(|(n1, t1)| {
                        ptr.0
                            .fields
                            .iter()
                            .any(|(n2, t2)| n1 == n2 && t1.matches_known(&t2.field_type))
                    })
            }

            (Self::Infer, _) | (_, Self::Infer) => true,
            (Self::Known(a), Self::Known(b)) => a == b,
            _ => false,
        }
    }

    pub fn matches_known(&self, other: &CompType<'a>) -> bool {
        match (self, other) {
            (Self::UnknownInteger { min_bits, signed }, CompType::Number(num)) => {
                (num.is_signed_int() == *signed || !signed && !num.is_float())
                    && num.bits() >= *min_bits
            }
            (Self::UnknownFloat, CompType::Number(num)) => num.is_float(),
            (Self::AnonymousStruct { fields: f1 }, CompType::Struct(ptr)) => {
                f1.len() != ptr.0.fields.len()
                    && f1.iter().all(|(n1, t1)| {
                        ptr.0
                            .fields
                            .iter()
                            .any(|(n2, t2)| n1 == n2 && t1.matches_known(&t2.field_type))
                    })
            }

            (Self::Known(a), b) => a == b,
            (Self::Infer, _) => true,
            _ => false,
        }
    }

    pub fn into_known(self) -> Result<CompType<'a>, Self> {
        match self {
            Self::Known(t) => Ok(t),
            _ => Err(self),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            MaybeUnknownType::UnknownInteger { .. } => "an integer",
            MaybeUnknownType::UnknownFloat => "a float",
            MaybeUnknownType::AnonymousStruct { .. } => "an anonymous struct",
            MaybeUnknownType::Known(comp_type) => comp_type.name(),
            MaybeUnknownType::Infer => "an inferred type",
        }
    }

    pub fn extend(&mut self, other: Self) {
        match (&self, other) {
            (_, Self::Infer) | (Self::Known(_), _) => (),
            (_, b) => *self = b,
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum CompType<'a> {
    StringSlice,
    Number(NumberType),
    Boolean,
    Pointer {
        is_const: bool,
        pointer_type: Box<CompType<'a>>,
    },
    UnsizedArray {
        is_const: bool,
        item_type: Box<CompType<'a>>,
    },
    Array {
        size: uarch,
        item_type: Box<CompType<'a>>,
    },
    Struct(StructPointer<'a>),
    Enum(EnumPointer<'a>),
    Tuple(Vec<CompType<'a>>),
    Function {
        parameters: Vec<Parameter<'a>>,
        return_type: Box<CompType<'a>>,
    },
    Type,
}
impl CompType<'_> {
    pub fn name(&self) -> &'static str {
        match self {
            CompType::StringSlice => "a constant string",
            CompType::Number(number_type) => number_type.name(),
            CompType::Boolean => "a boolean",
            CompType::Pointer { is_const, .. } => {
                if *is_const {
                    "a constant pointer"
                } else {
                    "a pointer"
                }
            }
            CompType::UnsizedArray { is_const, .. } => {
                if *is_const {
                    "a constant array pointer"
                } else {
                    "an array pointer"
                }
            }
            CompType::Array { .. } => "an array",
            CompType::Struct(_) => "a struct",
            CompType::Enum(_) => "an enum",
            CompType::Tuple(_) => "a tuple",
            CompType::Function { .. } => "a function",
            CompType::Type => "a type",
        }
    }
    pub fn size(&self) -> uarch {
        let uarch_size = uarch::BITS as uarch / 8;
        match self {
            CompType::StringSlice => uarch_size * 2, // length and pointer
            CompType::Number(number_type) => number_type.bits() as uarch / 8,
            CompType::Boolean => 1,
            CompType::Pointer { .. } => uarch_size,
            CompType::UnsizedArray { .. } => uarch_size * 2, // length and pointer
            CompType::Array { size, item_type } => item_type.size() * size,
            CompType::Struct(struct_pointer) => struct_pointer.0.size(),
            CompType::Enum(enum_pointer) => enum_pointer.0.size(),
            CompType::Tuple(comp_types) => comp_types.iter().map(|c| c.size()).sum(),
            CompType::Function { .. } => uarch_size,
            CompType::Type => uarch_size,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum NumberType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}

impl NumberType {
    pub fn bits(&self) -> u8 {
        match self {
            Self::U8 | Self::I8 => 8,
            Self::U16 | Self::I16 => 16,
            Self::U32 | Self::I32 | Self::F32 => 32,
            Self::U64 | Self::I64 | Self::F64 => 64,
        }
    }
    pub fn is_signed_int(&self) -> bool {
        matches!(self, Self::I8 | Self::I16 | Self::I32 | Self::I64)
    }
    pub fn is_float(&self) -> bool {
        matches!(self, Self::F32 | Self::F64)
    }
    pub fn name(&self) -> &'static str {
        match self {
            NumberType::U8 => "a u8",
            NumberType::U16 => "a u16",
            NumberType::U32 => "a u32",
            NumberType::U64 => "a u64",
            NumberType::I8 => "a i8",
            NumberType::I16 => "a i16",
            NumberType::I32 => "a i32",
            NumberType::I64 => "a i64",
            NumberType::F32 => "a f32",
            NumberType::F64 => "a f64",
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Parameter<'a> {
    pub name: &'a str,
    pub ty: CompType<'a>,
}

pub struct StructType<'a> {
    pub name: OnceCell<&'a str>,
    pub fields: HashMap<&'a str, Field<'a>>,
    pub assoc_consts: HashMap<&'a str, Constant<'a>>,
}

impl<'a> StructType<'a> {
    pub fn size(&self) -> uarch {
        self.fields.iter().map(|(_, f)| f.field_type.size()).sum()
    }
    pub fn name(&self) -> &'a str {
        self.name.get().unwrap_or(&"{unnamed struct}")
    }
}

pub struct Field<'a> {
    pub field_type: CompType<'a>,
    pub offset: uarch,
}

#[derive(Clone)]
pub struct StructPointer<'a>(pub Arc<StructType<'a>>);
impl PartialEq for StructPointer<'_> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for StructPointer<'_> {}

pub struct EnumType<'a> {
    pub name: OnceCell<&'a str>,
    pub tag_bytes: uarch,
    pub variants: HashMap<&'a str, Variant<'a>>,
    pub assoc_consts: HashMap<&'a str, Constant<'a>>,
}

impl<'a> EnumType<'a> {
    pub fn size(&self) -> uarch {
        self.tag_bytes
            + self
                .variants
                .iter()
                .map(|(_, v)| v.inner_type.as_ref().map_or(0, |t| t.size()))
                .max()
                .unwrap_or(0)
    }
    pub fn name(&self) -> &'a str {
        self.name.get().unwrap_or(&"{unnamed struct}")
    }
}

pub struct Variant<'a> {
    pub inner_type: Option<CompType<'a>>,
    pub tag: uarch,
}

#[derive(Clone)]
pub struct EnumPointer<'a>(pub Arc<EnumType<'a>>);
impl PartialEq for EnumPointer<'_> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for EnumPointer<'_> {}
