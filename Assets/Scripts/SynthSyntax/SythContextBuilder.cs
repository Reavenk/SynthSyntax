using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SythContextBuilder : SynthObj
    {
        public struct OperatorInfo
        {
            public string tokStr;
            public TokenASTType intrinsicOperator;

            public OperatorInfo(string tokStr, TokenASTType intOp)
            {
                this.tokStr = tokStr;
                this.intrinsicOperator = intOp;
            }
        }

        public enum StackType
        { 
            Local,
            Memory
        }

        public class StackVar
        { 
            public string varName;
            public SynthVarValue variable;
            public StackType location;
            public int alignment;
        }

        public Dictionary<string, StackVar> localStackVars = new Dictionary<string, StackVar>();
        public Dictionary<string, StackVar> memoryStackVars = new Dictionary<string, StackVar>();

        public int totalLocalStack = 0;
        public int totalMemoryStack = 0;

        List<TokenAST> asts = new List<TokenAST>();

        public void ProcessFunctionTokens(SynthFuncDecl function, List<TokenTree> trees)
        { 
            foreach(TokenTree tt in trees)
            { 
                if(tt.keyword == "for")
                { 
                }

                if(tt.keyword == "if")
                { 
                }

                if(tt.keyword == "while")
                { 
                }

                if(tt.keyword == "dowhile")
                { 
                }

                if(tt.root.MatchesSymbol("=") == true)
                { 
                }

                
            }
        }

        OperatorInfo[] OperatorsAssign =
            new OperatorInfo[]{
                new OperatorInfo("=",   TokenASTType.Compare_Eq),
                new OperatorInfo("-=",  TokenASTType.Compare_NEq),
                new OperatorInfo("+=",  TokenASTType.Compare_GreaterThan),
                new OperatorInfo("*=",  TokenASTType.Compare_GreaterThanEq),
                new OperatorInfo("/=",  TokenASTType.Compare_GreaterThanEq),
                new OperatorInfo("%=",  TokenASTType.Compare_GreaterThanEq),
                new OperatorInfo("|=",  TokenASTType.Compare_GreaterThanEq),
                new OperatorInfo("&=",  TokenASTType.Compare_GreaterThanEq),
                new OperatorInfo("^=",  TokenASTType.Compare_GreaterThanEq),
                new OperatorInfo("~=",  TokenASTType.Compare_GreaterThanEq)};

        OperatorInfo[] OperatorsCmp =
            new OperatorInfo[]{
                new OperatorInfo("==",  TokenASTType.Compare_Eq),
                new OperatorInfo("!=",  TokenASTType.Compare_NEq),
                new OperatorInfo(">",   TokenASTType.Compare_GreaterThan),
                new OperatorInfo(">=",  TokenASTType.Compare_GreaterThanEq),
                new OperatorInfo("<",   TokenASTType.Compare_LessThan),
                new OperatorInfo("<=",  TokenASTType.Compare_GreaterThanEq)};

        OperatorInfo[] OperatorsBitWise =
            new OperatorInfo[]{
                new OperatorInfo("|",   TokenASTType.Compare_Eq),
                new OperatorInfo("&",   TokenASTType.Compare_NEq),
                new OperatorInfo("^",   TokenASTType.Compare_GreaterThan),
                new OperatorInfo("~",   TokenASTType.Compare_GreaterThanEq)};

        OperatorInfo[] OperatorsMath =
            new OperatorInfo[]{
                new OperatorInfo("+",   TokenASTType.Compare_Eq),
                new OperatorInfo("-",   TokenASTType.Compare_NEq),
                new OperatorInfo("=",   TokenASTType.Compare_GreaterThan),
                new OperatorInfo("*",   TokenASTType.Compare_GreaterThanEq),
                new OperatorInfo("%",   TokenASTType.Compare_GreaterThanEq)};

        

        TokenAST ProcessLogic(SynthScope scope, TokenTree tt)
        { 
            TokenAST ast = ProcessMathTree(scope, tt);
            if(ast != null)
                return ast;

            ast = ProcessComparison(scope, tt);
            if(ast != null)
                return ast;

            ast = ProcessBitOperators(scope, tt);
            if(ast != null)
                return ast;

            ast = ProcessIntrinsic(scope, tt);
            if(ast != null)
                return ast;

            throw new SynthExceptionSyntax(tt.root, "Unknown syntax.");
        }

        TokenAST ProcessAssignTree(SynthScope scope, TokenTree tt)
        {
            foreach (OperatorInfo oi in OperatorsAssign)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    TokenAST left = ProcessLogic(scope, tt.nodes[0]);
                    TokenAST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatability(ref left, ref right);
                        return new TokenAST(tt.root, oi.intrinsicOperator, null, null, false, left, right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        TokenAST ProcessMathTree(SynthScope scope, TokenTree tt)
        {
            foreach (OperatorInfo oi in OperatorsCmp)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    TokenAST left = ProcessLogic(scope, tt.nodes[0]);
                    TokenAST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatability(ref left, ref right);
                        return new TokenAST(tt.root, oi.intrinsicOperator, null, scope.GetType("bool"), false, left, right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        

        TokenAST ProcessComparison(SynthScope scope, TokenTree tt)
        {
            foreach(OperatorInfo oi in OperatorsCmp)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    TokenAST left = ProcessLogic(scope, tt.nodes[0]);
                    TokenAST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatability(ref left, ref right);
                        return new TokenAST(tt.root, oi.intrinsicOperator, null, scope.GetType("bool"), false, left, right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        

        TokenAST ProcessBitOperators(SynthScope scope, TokenTree tt)
        {
            foreach (OperatorInfo oi in OperatorsBitWise)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    TokenAST left = ProcessLogic(scope, tt.nodes[0]);
                    TokenAST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatability(ref left, ref right);
                        return new TokenAST(tt.root, oi.intrinsicOperator, null, left.evaluatingType, false, left, right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        TokenAST ProcessMathOperators(SynthScope scope, TokenTree tt)
        {
            foreach (OperatorInfo oi in OperatorsBitWise)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    TokenAST left = ProcessLogic(scope, tt.nodes[0]);
                    TokenAST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatability(ref left, ref right);
                        return new TokenAST(tt.root, oi.intrinsicOperator, null, left.evaluatingType, false, left, right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        TokenAST ProcessIntrinsic(SynthScope scope, TokenTree tt)
        { 
            TokenAST ret = null;

            switch(tt.root.type)
            { 
                case TokenType.tyDouble:
                    return new TokenAST(tt.root, TokenASTType.DeclFloat64, null, scope.GetType("double"), false);

                case TokenType.tyFloat:
                    return new TokenAST(tt.root, TokenASTType.DeclFloat, null, scope.GetType("double"), false);

                case TokenType.tyInt:
                    return new TokenAST(tt.root, TokenASTType.DeclSInt, null, scope.GetType("int"), false);

                case TokenType.tyLong:
                    return new TokenAST(tt.root, TokenASTType.DeclSInt64, null, scope.GetType("int64"), false);
            }

            if(ret == null)
                return null;

            if(tt.nodes.Count > 0)
                throw new SynthExceptionImpossible("Unknown intrinsic.");

            return ret;
        }

        public static TokenAST GenerateOperatorAST(string operatorName, Token tokOp, TokenAST left, TokenAST right)
        {
            // First check non-reversible entries.
            SynthFuncDecl sfd = left.evaluatingType.GetOperator(operatorName, right.evaluatingType, SynthScope.OperatorReversing.OnlyNonReversible);
            if (sfd != null)
                return new TokenAST(tokOp, TokenASTType.CallMember, sfd, sfd.returnType, false, left, right);

            switch (operatorName)
            {
                case ">":
                    operatorName = "<=";
                    break;

                case ">=":
                    operatorName = "<";
                    break;

                case "<":
                    operatorName = ">=";
                    break;

                case "<=":
                    operatorName = ">";
                    break;

            }

            // If we couldn't find a working operator, try looking for reversible ones.
            sfd = left.evaluatingType.GetOperator(operatorName, left.evaluatingType, SynthScope.OperatorReversing.OnlyReversible);
            if (sfd != null)
                return new TokenAST(tokOp, TokenASTType.CallMember, sfd, sfd.returnType, false, right, left);

            throw new SynthExceptionSyntax(tokOp, "Could not find operator.");
        }

        public static void EnsureIntrinsicCompatability(ref TokenAST left, ref TokenAST right)
        {
            if (left.evaluatingType.intrinsic == false || right.evaluatingType.intrinsic == false)
                throw new SynthExceptionImpossible("Checking instrinsic compatibility of non-intrinsic character.");

            HashSet<string> hs = new HashSet<string>();
            hs.Add(left.evaluatingType.typeName);
            hs.Add(right.evaluatingType.typeName);

            // If they're both the same, there will only be 1 entry, and 
            // they will be compatable with each other.
            if (hs.Count == 1)
                return;

            if (CountMatches(hs, "float", "double") == 2)
            {
                if (left.evaluatingType.typeName == "float")
                {
                    TokenAST cast = new TokenAST(left.token, TokenASTType.Cast_Double, right.evaluatingType, right.evaluatingType, false, left);
                    left = cast;
                }
                else
                {
                    TokenAST cast = new TokenAST(right.token, TokenASTType.Cast_Double, left.evaluatingType, left.evaluatingType, false, right);
                    right = cast;
                }

            }

            if (CountMatches(hs, "int", "int8", "int16", "int64", "uint", "uint8", "uint16", "uint16") > 0 && hs.Contains("float"))
            {
                if (left.evaluatingType.typeName != "float")
                {
                    TokenAST cast = new TokenAST(left.token, TokenASTType.Cast_Float, right.evaluatingType, right.evaluatingType, false, left);
                    left = cast;
                }
                else
                {
                    TokenAST cast = new TokenAST(right.token, TokenASTType.Cast_Float, left.evaluatingType, left.evaluatingType, false, right);
                    right = cast;
                }
            }

            if (CountMatches(hs, "int", "int8", "int16", "int64", "uint", "uint8", "uint16", "uint16") > 0 && hs.Contains("double"))
            {
                if (left.evaluatingType.typeName != "double")
                {
                    TokenAST cast = new TokenAST(left.token, TokenASTType.Cast_Double, right.evaluatingType, right.evaluatingType, false, left);
                    left = cast;
                }
                else
                {
                    TokenAST cast = new TokenAST(right.token, TokenASTType.Cast_Double, left.evaluatingType, left.evaluatingType, false, right);
                    right = cast;
                }
            }

            if (
                CountMatches(hs, "int8", "int", "int16", "int64") == 2 ||
                CountMatches(hs, "uint8", "uint", "uint16", "uint64") == 2)
            {
                if (GetIntrinsicByteSizeFromName(left.evaluatingType.typeName) < GetIntrinsicByteSizeFromName(right.evaluatingType.typeName))
                {
                    TokenAST cast = new TokenAST(left.token, GetCastInstrinsicType(right.evaluatingType.typeName), right.evaluatingType, right.evaluatingType, false, left);
                    left = cast;
                }
                else
                {
                    TokenAST cast = new TokenAST(right.token, GetCastInstrinsicType(left.evaluatingType.typeName), left.evaluatingType, left.evaluatingType, false, right);
                    right = cast;
                }
            }

            throw new SynthExceptionSyntax(left.token, $"Cannot perform operation between {left.evaluatingType.typeName} and {right.evaluatingType.typeName} without a cast.");
        }

        public static TokenASTType GetCastInstrinsicType(string ty)
        {
            switch (ty)
            {
                case "int8":
                    return TokenASTType.Cast_Int8;
                case "uint8":
                    return TokenASTType.Cast_UInt8;
                case "int16":
                    return TokenASTType.Cast_Int16;
                case "uint16":
                    return TokenASTType.Cast_UInt16;
                case "int":
                    return TokenASTType.Cast_Int;
                case "uint":
                    return TokenASTType.Cast_UInt;
                case "float":
                    return TokenASTType.Cast_Float;
                case "int64":
                    return TokenASTType.Cast_Int64;
                case "uint64":
                    return TokenASTType.Cast_UInt64;
                case "double":
                    return TokenASTType.Cast_Double;
            }

            throw new SynthExceptionImpossible("Could not get casting code from type name.");
        }

        public static int GetIntrinsicByteSizeFromName(string ty)
        {
            switch (ty)
            {
                case "int8":
                case "uint8":
                    return 1;

                case "int16":
                case "uint16":
                    return 2;

                case "int":
                case "uint":
                case "float":
                    return 4;

                case "int64":
                case "uint64":
                case "double":
                    return 8;
            }

            throw new SynthExceptionImpossible("Could not get byte size from type name.");
        }

        public static int CountMatches(HashSet<string> hs, params string[] set)
        {
            int ret = 0;

            foreach (string s in set)
            {
                if (hs.Contains(s) == true)
                    ++ret;
            }

            return ret;
        }

        public static TokenAST GetTypeNot(string typename, TokenAST left, TokenAST right)
        {
            if (left.evaluatingType.typeName == typename)
                return right;

            return left;
        }

        public void ProcessContextTokens(WASMBuild build, SynthFuncDecl function, List<TokenTree> trees)
        { 
        }

        public void ProcessDefaultVarTokens(SynthVarValue fnParam, List<TokenTree> trees)
        { 
        }
    }
}