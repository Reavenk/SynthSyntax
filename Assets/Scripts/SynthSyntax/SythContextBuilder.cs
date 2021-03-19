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

        public struct StackAlign
        { 
            public int locStkSize;
            public int locStkAlign;

            public int memStkSize;
            public int memStkAlign;
        }


        public int totalLocalStack = 0;
        public List<StackAlign> locStkEle = new List<StackAlign>();
        public Dictionary<string, StackVar> locStkVars = new Dictionary<string, StackVar>();

        public int totalMemoryStack = 0;
        public List<StackAlign> memStkEle = new List<StackAlign>();
        public Dictionary<string, StackVar> memStkVars = new Dictionary<string, StackVar>();

        List<TokenAST> asts = new List<TokenAST>();

        OperatorInfo[] OperatorsAssign =
            new OperatorInfo[]{
                new OperatorInfo("=",   TokenASTType.SetValue),
                new OperatorInfo("+=",  TokenASTType.SetAfterAdd),
                new OperatorInfo("-=",  TokenASTType.SetAfterSub),
                new OperatorInfo("*=",  TokenASTType.SetAfterMul),
                new OperatorInfo("/=",  TokenASTType.SetAfterDiv),
                new OperatorInfo("%=",  TokenASTType.SetAfterMod),
                new OperatorInfo("|=",  TokenASTType.SetAfterBitOr),
                new OperatorInfo("&=",  TokenASTType.SetAfterBitAnd),
                new OperatorInfo("^=",  TokenASTType.SetAfterBitXor),
                new OperatorInfo(">>=", TokenASTType.SetAfterShiftL),
                new OperatorInfo("<<=", TokenASTType.SetAfterShiftR) };

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
                new OperatorInfo("|",   TokenASTType.BitOr),
                new OperatorInfo("&",   TokenASTType.BitAnd),
                new OperatorInfo("^",   TokenASTType.BitXor),
                new OperatorInfo("~",   TokenASTType.BitInv),
                new OperatorInfo(">>",  TokenASTType.BitShiftL),
                new OperatorInfo("<<",  TokenASTType.BitShiftR)};

        OperatorInfo[] OperatorsMath =
            new OperatorInfo[]{
                new OperatorInfo("+",   TokenASTType.Add),
                new OperatorInfo("-",   TokenASTType.Sub),
                new OperatorInfo("*",   TokenASTType.Mul),
                new OperatorInfo("/",   TokenASTType.Div),
                new OperatorInfo("%",   TokenASTType.Mod)};

        

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
                        EnsureIntrinsicCompatibility(left, ref right);
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
                        EnsureIntrinsicCompatibility(left, ref right);
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
                        EnsureIntrinsicCompatibility(left, ref right);
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
                        EnsureIntrinsicCompatibility(left, ref right);
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
                        EnsureIntrinsicCompatibility(left, ref right);
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

        public TokenAST ProcessFunctionExpression(WASMBuild build, SynthFuncDecl function, TokenTree node)
        {

            if (node.keyword == "for")
            {
                if(node.nodes.Count < 2)
                    throw new System.Exception("for loop needs at least two trees, one for the structure and one for the body statements.");

                if(node.nodes[0].root.MatchesSymbol("(") == false)
                    throw new SynthExceptionImpossible("");
            }

            if (node.keyword == "if")
            {
                if(node.nodes.Count < 2)
                {
                    // Right now this may not be 100% true if an empty body is used.
                    // TODO: Check up on this.
                    throw new SynthExceptionImpossible("if statement needs at least two trees, one for the predicate and one for the body statements.");
                }

                TokenAST pred = ProcessFunctionExpression(build, function, node.nodes[0]);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(node.nodes[0].root, "If statement predicate did not evaluate to a bool.");

                TokenAST astIf = new TokenAST(node.root, TokenASTType.IfStatement, null, null, false, pred);

                for(int i = 1; i < node.nodes.Count; ++i)
                    astIf.branches.Add(ProcessFunctionExpression(build, function, node.nodes[i]));

                return astIf;
            }

            if (node.keyword == "while")
            {
                // NOTE: As far as AST generation goes, it's pretty much the same 
                // structure as an if statement. We may want to consolidate them.
                if(node.nodes.Count < 2)
                {
                    // Right now this may not be 100% true if an empty body is used.
                    // TODO: Check up on this.
                    throw new SynthExceptionImpossible("while statement needs at least two trees, one for the predicate and one for the body statements.");
                }

                TokenAST pred = ProcessFunctionExpression(build, function, node.nodes[0]);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(node.nodes[0].root, "While statement predicate did not evaludate to a bool.");

                TokenAST astWhile = new TokenAST(node.root, TokenASTType.WhileStatement, null, null, false, pred);

                for(int i = 1; i < node.nodes.Count; ++i)
                    astWhile.branches.Add(ProcessFunctionExpression(build, function, node.nodes[i]));

                return astWhile;
            }

            if (node.keyword == "dowhile")
            {
                if(node.nodes.Count < 2)
                    throw new SynthExceptionImpossible("dowhile statement needs at least two trees, one for the body statements and one for the continue predicate.");

                TokenTree ttLast = node.nodes[node.nodes.Count - 1];
                TokenAST pred = ProcessFunctionExpression(build, function, ttLast);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(ttLast.root, "dowhile statement predicate did not evaluate to a bool.");

                TokenAST astDoWhile = new TokenAST(node.root, TokenASTType.DoWhileStatement, null, null, false);

                for(int i = 0; i < node.nodes.Count - 1; ++i)
                    astDoWhile.branches.Add(ProcessFunctionExpression(build, function, node.nodes[i]));

                astDoWhile.branches.Add(pred);
                return astDoWhile;
            }

            if (node.root.MatchesSymbol("=") == true)
            {
                if(node.nodes.Count != 2)
                    throw new SynthExceptionImpossible("Equal needs exactly two trees to evaluate.");

                TokenAST left = ProcessFunctionExpression(build, function, node.nodes[0]);
                if(left.hasAddress == false)
                    throw new SynthExceptionSyntax(node.nodes[0].root, "Left side of equation not addressable.");

                TokenAST right = ProcessFunctionExpression(build, function, node.nodes[1]);
                EnsureIntrinsicCompatibility(left.evaluatingType, ref right);

                TokenAST astEq = new TokenAST(node.root, TokenASTType.SetValue, null, left.evaluatingType, false, left, right);
                return astEq;
            }

            if (node.keyword == "paren")
            {
                if (node.root.Matches(TokenType.tySymbol, "(") == false)
                    throw new SynthExceptionImpossible(""); // TODO:

                if(node.nodes.Count == 2)
                { 
                    if(node.nodes[0].root.MatchesSymbol("(") == false)
                        throw new SynthExceptionImpossible("Unknown open parenthesis symbol.");

                    int paramNum = node.nodes[0].nodes.Count;

                    List<TokenAST> astParams = new List<TokenAST>();
                    foreach(TokenTree tt in node.nodes[0].nodes)
                        astParams.Add(this.ProcessFunctionExpression(build, function, tt));

                    // Default params not supported for now
                    TokenAST caller = ProcessFunctionExpression(build, function, node.nodes[1]);
                    SynthFuncDecl fn = caller.synthObj.CastFuncDecl();
                    if(fn == null)
                        throw new SynthExceptionSyntax(node.nodes[1].root, "Could not evaluate function");

                    if(fn.paramList.Count != paramNum)
                        throw new SynthExceptionSyntax(node.nodes[1].root, $"Invalid number of parameters for {fn.functionName}, expected {fn.paramList.Count} but encountered {paramNum}.");

                    for(int i = 0; i < fn.paramList.Count; ++i)
                    { 
                        TokenAST paramAST = astParams[i];
                        if(paramAST.evaluatingType == null)
                            throw new SynthExceptionSyntax(paramAST.token, $"Parameter {i} does not evaluate to any type.");
                        else if (paramAST.evaluatingType.intrinsic == fn.paramList[i].type.intrinsic)
                        {
                            // Throws if they do not match and there isn't an appropriate cast.
                            EnsureIntrinsicCompatibility(fn.paramList[i].type, ref paramAST);
                        }
                        else if(paramAST.evaluatingType != fn.paramList[i].type)
                            throw new SynthExceptionSyntax(paramAST.token, $"Type mismatch for parameter {i}: expected type {fn.paramList[i].type.typeName} but got {paramAST.evaluatingType.typeName}.");
                        // TODO: Not supported right now, but if they're not both intrinsic, classes may
                        // have conversion operators.
                    }

                    caller.synthObj = fn;
                    caller.astType = TokenASTType.CallGlobalFn; // TODO: In the right context, figure out when to call members
                    caller.evaluatingType = fn.returnType;
                    return caller;
                }
            }

            if (node.keyword == "index")
            {
                if (node.root.Matches(TokenType.tySymbol, "[") == false)
                    throw new SynthExceptionImpossible(""); // TODO:

                TokenAST astKey = ProcessFunctionExpression(build, function, node.nodes[0]);
                TokenAST astSrc = ProcessFunctionExpression(build, function, node.nodes[1]);
                EnsureTypeIsBitCompatible(astSrc.evaluatingType, node.nodes[1].root);

                return new TokenAST(node.root, TokenASTType.Index, null, null, true, astSrc, astKey);
            }

            if (node.root.MatchesSymbol("+") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Add, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("-") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Sub, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("*") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Mul, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("/") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Div, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("%") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Mod, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("&") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.BitAnd, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("|") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.BitOr, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("^") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.BitXor, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("~") == true)
            {
                if(node.nodes.Count != 1)
                    throw new SynthExceptionSyntax(node.root, "~ operation expecting exactly 1 tree."); //Not a great error message

                TokenAST evVal = ProcessFunctionExpression(build, function, node.nodes[0]);
                EnsureTypeIsBitCompatible(evVal.evaluatingType, node.nodes[0].root);

                return new TokenAST(node.root, TokenASTType.BitInv, null, null, false, evVal);
            }

            if (node.root.MatchesSymbol("+=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, false, true);
                return new TokenAST(node.root, TokenASTType.SetAfterAdd, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("-=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, false, true);
                return new TokenAST(node.root, TokenASTType.SetAfterSub, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("*=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, false, true);
                return new TokenAST(node.root, TokenASTType.SetAfterMul, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("/=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, false, true);
                return new TokenAST(node.root, TokenASTType.SetAfterDiv, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("%=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, false, true);

                return new TokenAST(node.root, TokenASTType.SetAfterMod, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("&=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.SetAfterBitAnd, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("|=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.SetAfterBitOr, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("^=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.SetAfterBitXor, null, null, false, left, right);
            }

            return null;
        }

        public void EnsureLeftAndRightCompatibility(WASMBuild build, SynthFuncDecl fnDecl, TokenTree tt, out TokenAST left, out TokenAST right, bool allowCastingLeft, bool leftRequireAddr)
        {
            if(tt.nodes.Count != 2)
                throw new SynthExceptionSyntax(tt.root, $"Error parsing {tt.root.fragment}.");

            left = ProcessFunctionExpression(build, fnDecl, tt.nodes[0]);
            right = ProcessFunctionExpression(build, fnDecl, tt.nodes[1]);

            if(left == null || left.evaluatingType == null)
                throw new SynthExceptionSyntax(tt.root, $"Error parsing {tt.root.fragment}, left side of equation does not evaluate to a type.");

            if(right == null || right.evaluatingType == null)
                throw new SynthExceptionSyntax(tt.root, $"Error parsing {tt.root.fragment}, right side of equation does not evaluate to a type.");

            if (leftRequireAddr == true)
            {
                if (left.hasAddress == false)
                    throw new SynthExceptionSyntax(tt.root, $"Error parsing {tt.root.fragment}, left side expected to be addressable.");
            }

            try
            {
                EnsureIntrinsicCompatibility(left.evaluatingType, ref right, false);
            }
            catch(System.Exception ex)
            {
                if (leftRequireAddr == false)
                    throw ex;

                EnsureIntrinsicCompatibility(left.evaluatingType, ref right);
            }
        }

        public static void EnsureTypeIsInt(SynthType sty, Token tErrSrc)
        { 
            // They happen to be the same
            // TODO: figure out best way to throw correct error message
            EnsureTypeIsBitCompatible(sty, tErrSrc);
        }

        public static void EnsureTypeIsBitCompatible(SynthType sty, Token tErrSrc)
        { 
            switch(sty.typeName)
            { 
                case "int8":
                case "uint8":
                case "int16":
                case "uint16":
                case "int":
                case "uint":
                case "int64":
                case "uint64":
                    return;

                default:
                    throw new SynthExceptionSyntax(tErrSrc, "Bit operator not possible on value types.");
            }
        }

        public static void EnsureIntrinsicCompatibility(TokenAST left, ref TokenAST right)
        {
            EnsureIntrinsicCompatibility(left.evaluatingType, ref right);
        }

        /// <summary>
        /// Check to make sure that an AST variable of a certain intrinsic datatype is compatible
        /// with another data type.
        /// 
        /// If they are not the same, attempt to cast value to the correct type.
        /// </summary>
        /// <param name="styLeft">The type the right needs to match.</param>
        /// <param name="right">The value who's type is being matched, or casted if necessary.</param>
        public static bool EnsureIntrinsicCompatibility(SynthType styLeft, ref TokenAST right, bool throwOnFail = true)
        {
            // If they're the same, no conversion needed.
            if(styLeft == right.evaluatingType)
                return true;

            // Impossible throws will throw no matter what.
            if (styLeft.intrinsic == false || right.evaluatingType.intrinsic == false)
                    throw new SynthExceptionImpossible("Checking instrinsic compatibility of non-intrinsic character.");

            HashSet<string> hs = new HashSet<string>();
            hs.Add(styLeft.typeName);
            hs.Add(right.evaluatingType.typeName);

            // If we only have 1 entry, they have the same name and should be the same, but 
            // then how did they fail the first if-statement of the function?
            //
            // Impossible
            if (hs.Count == 1)
                throw new SynthExceptionImpossible("Compatible types were actually the same.");

            if (CountMatches(hs, "float", "double") == 2)
            {
                if (styLeft.typeName == "double")
                {
                    TokenAST cast = new TokenAST(right.token, TokenASTType.Cast_Double, styLeft, styLeft, false, right);
                    right = cast;
                    return true;
                }

            }

            if (CountMatches(hs, "int", "int8", "int16", "int64", "uint", "uint8", "uint16", "uint16") > 0 && hs.Contains("float"))
            {
                if (styLeft.typeName == "float")
                {
                    TokenAST cast = new TokenAST(right.token, TokenASTType.Cast_Float, styLeft, styLeft, false, right);
                    right = cast;
                    return true;
                }
            }

            if (CountMatches(hs, "int", "int8", "int16", "int64", "uint", "uint8", "uint16", "uint16") > 0 && hs.Contains("double"))
            {
                if (styLeft.typeName == "double")
                {
                    TokenAST cast = new TokenAST(right.token, TokenASTType.Cast_Double, styLeft, styLeft, false, right);
                    right = cast;
                    return true;
                }
            }

            if (
                CountMatches(hs, "int8", "int", "int16", "int64") == 2 ||
                CountMatches(hs, "uint8", "uint", "uint16", "uint64") == 2)
            {
                if (GetIntrinsicByteSizeFromName(styLeft.typeName) > GetIntrinsicByteSizeFromName(right.evaluatingType.typeName))
                {
                    TokenAST cast = new TokenAST(right.token, GetCastInstrinsicType(styLeft.typeName), styLeft, styLeft, false, right);
                    right = cast;
                }
            }

            if(throwOnFail == true)
                throw new SynthExceptionSyntax(right.token, $"Cannot perform operation between {styLeft.typeName} and {right.evaluatingType.typeName} without a cast.");

            return false;
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

        

        public void ProcessDefaultVarTokens(SynthVarValue fnParam, List<TokenTree> trees)
        { 
        }

        public void BuildBSFunction( SynthFuncDecl fnd, TokenAST ast, List<byte> fnbc)
        { 
        }
    }
}