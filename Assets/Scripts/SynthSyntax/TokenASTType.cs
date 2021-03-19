﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public enum TokenASTType
    { 
        SetValue,

        // Root AST node for function declarations. Will also have the 
        FunctionDecl,

        SetAfterAdd,
        SetAfterSub,
        SetAfterMul,
        SetAfterDiv,
        SetAfterMod,
        SetAfterBitOr,
        SetAfterBitAnd,
        SetAfterBitXor,
        SetAfterShiftL,
        SetAfterShiftR,

        Index,

        IfStatement,
        WhileStatement,
        ForStatement,
        DoWhileStatement,

        Add,
        Sub,
        Mul,
        Div,
        Mod,

        BitOr,
        BitAnd,
        BitXor,
        BitInv,
        BitShiftL,
        BitShiftR,

        Unprocessed,

        RegisterVar,

        DeclUInt,
        DeclSInt,
        DeclUInt64,
        DeclSInt64,
        DeclFloat,
        DeclFloat64,

        UIntToFloat,
        UIntToDouble,
        UIntToUInt64,
        SIntToSInt64,

        FloatToUInt,
        FloatToSInt,
        FloatToSInt64,
        FloatToUInt64,
        FloatToFloat64,

        DoubleToUInt,
        DoubleToSInt,
        DoubleToSInt64,
        DoubleToUInt64,
        DoubleToFloat,

        Cast_Int,
        Cast_UInt,
        Cast_Int8,
        Cast_UInt8,
        Cast_Int16,
        Cast_UInt16,
        Cast_Int64,
        Cast_UInt64,
        Cast_Float,
        Cast_Double,

        Compare_Eq,
        Compare_NEq,
        Compare_LessThan,
        Compare_LessThanEq,
        Compare_GreaterThan,
        Compare_GreaterThanEq,

        CallMember,
        CallGlobalFn,

        DefaultParam,

    }
}