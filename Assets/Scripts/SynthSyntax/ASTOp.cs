using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public enum ASTOp
    { 
        SetValue,

        GetGlobalVar,
        GetMemberVar,
        GetLocalVar,
        GetParamVar,
        GetFunction,
        GetRegion,
        GetThis,
        Construct,

        Destruct,

        /// <summary>
        /// The last AST generated in the context of a SynthContextBuilder 
        /// should be an EndScope, which tells the compiler to run all 
        /// destructors for local variables.
        /// </summary>
        EndScope,

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
        Negate,

        BitOr,
        BitAnd,
        BitXor,
        BitInv,
        BitShiftL,
        BitShiftR,

        Unprocessed,

        RegisterLocalVar,
        RegisterLocalVarName,
        RegisterLocalVarInit,

        // Declare type values - note this isn't the same
        // as declaring variables of these types (that would be
        // RegisterLocalVar).
        DeclBool,
        DeclUInt,
        DeclSInt,
        DeclUInt64,
        DeclSInt64,
        DeclFloat,
        DeclFloat64,
        DeclString,

        DoubleToUInt,
        DoubleToSInt,
        DoubleToSInt64,
        DoubleToUInt64,
        DoubleToFloat,

        ExplicitCast,
        ImplicitCast,

        Compare_Eq,
        Compare_NEq,
        Compare_LessThan,
        Compare_LessThanEq,
        Compare_GreaterThan,
        Compare_GreaterThanEq,

        DerefName,
        ProposeMethod,
        CallMember,
        CallGlobalFn,

        // Not currently fully supported, but partially supported so that
        // it can be parsed.
        //
        // TODO: Perform compile error checking to make sure functions with
        // return values properly return.
        ReturnValue,

        DefaultParam,

    }
}