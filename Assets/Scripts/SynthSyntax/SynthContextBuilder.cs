using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthContextBuilder : SynthObj
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

        public enum ValueLoc
        {
            NoValue,
            ValueOnStack,
            ValueOnMem,
            AddrParam,
            AddrOnHeap,
            AddrOnMemStack,
        }

        public struct ValueRef
        { 
            public ValueLoc valLoc;
            public int idx;
            public SynthType varType;

            public ValueRef(ValueLoc valLoc, int idx, SynthType varType)
            { 
                this.valLoc = valLoc;
                this.idx = idx;
                this.varType = varType;
            }
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

        /// <summary>
        /// The number of bytes pushed on the stack. Does not include local variables.
        /// </summary>
        public int currentStackSize = 0;


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

        /// <summary>
        /// Generates an AST based on a based TokenTree structure for the source code of a function.
        /// </summary>
        /// <param name="build">The WASM build object to cache build information and to provide and 
        /// interface to WASM compiling utilities.</param>
        /// <param name="invokingContext">The type representing the "this" of the object. This is set
        /// to null for global/static functions.</param>
        /// <param name="function">The function scope being declared. This provides the function code (being
        /// structured into an AST) with everything in its scope.</param>
        /// <param name="node">The tree of tokens to parse into an AST node.</param>
        /// <returns>The generated AST.</returns>
        /// <remarks>Currently if the function fails, either a null will be returned or an exception 
        /// will be thrown. It is expected in the future for nulls to never be thrown, and any error
        /// or null result to end with a throw.</remarks>
        public TokenAST ProcessFunctionExpression(
            WASMBuild build, 
            SynthScope invokingContext, 
            SynthFuncDecl function, 
            TokenTree node)
        {
            if(node.root.Matches(TokenType.tyBool) == true)
            {
                EnsureNoTreeChildNodes(node);
                return new TokenAST(node.root, TokenASTType.DeclBool, null, build.rootContext.GetType("bool"), false);
            }
            
            if(node.root.Matches(TokenType.tyInt) == true)
            {
                EnsureNoTreeChildNodes(node);
                // All ints are signed by default - they can be casted with the cast optimized out later.
                return new TokenAST(node.root, TokenASTType.DeclSInt, null, build.rootContext.GetType("int"), false);
            }

            if(node.root.Matches(TokenType.tyFloat) == true)
            {
                EnsureNoTreeChildNodes(node);
                return new TokenAST(node.root, TokenASTType.DeclFloat, null, build.rootContext.GetType("float"), false);
            }

            if(node.root.Matches(TokenType.tyDouble) == true)
            {
                EnsureNoTreeChildNodes(node);
                return new TokenAST(node.root, TokenASTType.DeclFloat64, null, build.rootContext.GetType("double"), false);
            }

            if(node.root.Matches(TokenType.tyString) == true)
            {
                EnsureNoTreeChildNodes(node);
                build.stringRepo.RegisterString(node.root.fragment);

                return new TokenAST(node.root, TokenASTType.DeclString, null, build.rootContext.GetType("string"), false);
            }

            if(node.root.Matches(TokenType.tyWord) == true)
            { 
                if(node.root.Matches("this") == true)
                { 
                    if(function.isStatic == true)
                        throw new SynthExceptionSyntax(node.root, "this cannot be used in a static function.");

                    SynthType_Struct rootScope = function.GetStructScope();
                    return new TokenAST(node.root, TokenASTType.GetThis, rootScope, rootScope, true);
                }

                // TODO: We need to figure out the scope so we know where to actually grab
                // it from in memory alignment when we build WASM.
                SynthVarValue svv = function.GetVar(node.root.fragment);
                if(svv != null)
                    return new TokenAST(node.root, TokenASTType.GetLocalVar, svv, svv.type, true);

                SynthCanidateFunctions canFns = function.GetCanidateFunctions(node.root.fragment);
                if(canFns == null || canFns.functions.Count > 0)
                    return new TokenAST(node.root, TokenASTType.GetFunction, canFns, null, false);

                // TODO: Getting scope
            }

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

                TokenAST pred = ProcessFunctionExpression(build, invokingContext, function, node.nodes[0]);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(node.nodes[0].root, "If statement predicate did not evaluate to a bool.");

                TokenAST astIf = new TokenAST(node.root, TokenASTType.IfStatement, null, null, false, pred);

                for(int i = 1; i < node.nodes.Count; ++i)
                    astIf.branches.Add(ProcessFunctionExpression(build, invokingContext, function, node.nodes[i]));

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

                TokenAST pred = ProcessFunctionExpression(build, invokingContext, function, node.nodes[0]);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(node.nodes[0].root, "While statement predicate did not evaludate to a bool.");

                TokenAST astWhile = new TokenAST(node.root, TokenASTType.WhileStatement, null, null, false, pred);

                for(int i = 1; i < node.nodes.Count; ++i)
                    astWhile.branches.Add(ProcessFunctionExpression(build, invokingContext, function, node.nodes[i]));

                return astWhile;
            }

            if (node.keyword == "dowhile")
            {
                if(node.nodes.Count < 2)
                    throw new SynthExceptionImpossible("dowhile statement needs at least two trees, one for the body statements and one for the continue predicate.");

                TokenTree ttLast = node.nodes[node.nodes.Count - 1];
                TokenAST pred = ProcessFunctionExpression(build, invokingContext, function, ttLast);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(ttLast.root, "dowhile statement predicate did not evaluate to a bool.");

                TokenAST astDoWhile = new TokenAST(node.root, TokenASTType.DoWhileStatement, null, null, false);

                for(int i = 0; i < node.nodes.Count - 1; ++i)
                    astDoWhile.branches.Add(ProcessFunctionExpression(build, invokingContext, function, node.nodes[i]));

                astDoWhile.branches.Add(pred);
                return astDoWhile;
            }

            if (node.root.MatchesSymbol("=") == true)
            {
                if(node.nodes.Count != 2)
                    throw new SynthExceptionImpossible("Equal needs exactly two trees to evaluate.");

                TokenAST left = ProcessFunctionExpression(build, invokingContext, function, node.nodes[0]);
                if(left.hasAddress == false)
                    throw new SynthExceptionSyntax(node.nodes[0].root, "Left side of equation not addressable.");

                TokenAST right = ProcessFunctionExpression(build, invokingContext, function, node.nodes[1]);
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
                        astParams.Add(this.ProcessFunctionExpression(build, invokingContext, function, tt));

                    // Default params not supported for now
                    TokenAST caller = ProcessFunctionExpression(build, invokingContext, function, node.nodes[1]);
                    SynthCanidateFunctions cfns = caller.synthObj.CastCanidateFunctions();
                    if(cfns == null)
                        throw new SynthExceptionSyntax(node.nodes[1].root, "Could not evaluate function");

                    // We received a list of possible function overloads, now we find the appropriate one.
                    // For now we're going to be super-naive and just go off of parameter count.
                    //
                    // TODO: Imply "this" where needed.
                    List<SynthFuncDecl> matchingFns = new List<SynthFuncDecl>();
                    foreach(SynthFuncDecl sfn in cfns.functions)
                    { 
                        if(sfn.paramList.Count != paramNum)
                            continue;

                        matchingFns.Add(sfn);
                    }

                    if (matchingFns.Count != 1)
                        throw new SynthExceptionSyntax(node.nodes[1].root, $"Could not find a proper overloaded function.");

                    // The function that has been resolved as the one for use.
                    SynthFuncDecl reslvFn = matchingFns[0];

                    for (int i = 0; i < reslvFn.paramList.Count; ++i)
                    { 
                        TokenAST paramAST = astParams[i];
                        if(paramAST.evaluatingType == null)
                            throw new SynthExceptionSyntax(paramAST.token, $"Parameter {i} does not evaluate to any type.");
                        else if (paramAST.evaluatingType.intrinsic == reslvFn.paramList[i].type.intrinsic)
                        {
                            // Throws if they do not match and there isn't an appropriate cast.
                            EnsureIntrinsicCompatibility(reslvFn.paramList[i].type, ref paramAST);
                        }
                        else if(paramAST.evaluatingType != reslvFn.paramList[i].type)
                            throw new SynthExceptionSyntax(paramAST.token, $"Type mismatch for parameter {i}: expected type {reslvFn.paramList[i].type.typeName} but got {paramAST.evaluatingType.typeName}.");
                        // TODO: Not supported right now, but if they're not both intrinsic, classes may
                        // have conversion operators.

                        caller.branches.Add(paramAST);
                    }

                    caller.synthObj = reslvFn;
                    caller.astType = TokenASTType.CallGlobalFn; // TODO: In the right context, figure out when to call members
                    caller.evaluatingType = reslvFn.returnType;
                    return caller;
                }
            }

            if (node.keyword == "index")
            {
                if (node.root.Matches(TokenType.tySymbol, "[") == false)
                    throw new SynthExceptionImpossible(""); // TODO:

                TokenAST astKey = ProcessFunctionExpression(build, invokingContext, function, node.nodes[0]);
                TokenAST astSrc = ProcessFunctionExpression(build, invokingContext, function, node.nodes[1]);
                EnsureTypeIsBitCompatible(astSrc.evaluatingType, node.nodes[1].root);

                return new TokenAST(node.root, TokenASTType.Index, null, null, true, astSrc, astKey);
            }

            if (node.root.MatchesSymbol("+") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Add, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("-") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Sub, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("*") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Mul, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("/") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Div, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("%") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, TokenASTType.Mod, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("&") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.BitAnd, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("|") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.BitOr, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("^") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.BitXor, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("~") == true)
            {
                if(node.nodes.Count != 1)
                    throw new SynthExceptionSyntax(node.root, "~ operation expecting exactly 1 tree."); //Not a great error message

                TokenAST evVal = ProcessFunctionExpression(build, invokingContext, function, node.nodes[0]);
                EnsureTypeIsBitCompatible(evVal.evaluatingType, node.nodes[0].root);

                return new TokenAST(node.root, TokenASTType.BitInv, null, evVal.evaluatingType, false, evVal);
            }

            if (node.root.MatchesSymbol("+=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, false, true);
                return new TokenAST(node.root, TokenASTType.SetAfterAdd, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("-=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, false, true);
                return new TokenAST(node.root, TokenASTType.SetAfterSub, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("*=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, false, true);
                return new TokenAST(node.root, TokenASTType.SetAfterMul, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("/=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, false, true);
                return new TokenAST(node.root, TokenASTType.SetAfterDiv, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("%=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, false, true);

                return new TokenAST(node.root, TokenASTType.SetAfterMod, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("&=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.SetAfterBitAnd, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("|=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.SetAfterBitOr, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("^=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, TokenASTType.SetAfterBitXor, null, null, false, left, right);
            }

            return null;
        }

        public void EnsureLeftAndRightCompatibility(WASMBuild build, SynthScope invokingContext, SynthFuncDecl fnDecl, TokenTree tt, out TokenAST left, out TokenAST right, bool allowCastingLeft, bool leftRequireAddr)
        {
            if(tt.nodes.Count != 2)
                throw new SynthExceptionSyntax(tt.root, $"Error parsing {tt.root.fragment}.");

            left = ProcessFunctionExpression(build, invokingContext, fnDecl, tt.nodes[0]);
            right = ProcessFunctionExpression(build, invokingContext, fnDecl, tt.nodes[1]);

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

        public void BuildBSFunction( SynthFuncDecl fnd, TokenAST ast, WASMBuild wasmBuild, SynthContextBuilder ctxBuilder, List<byte> fnbc)
        { 
            foreach(TokenAST n in ast.branches)
            { 
                this.BuildBSFunctionExpression(fnd, n, wasmBuild, ctxBuilder, fnbc);
            }
        }

        public ValueRef BuildBSFunctionExpression(SynthFuncDecl fnd, TokenAST expr, WASMBuild wasmBuild, SynthContextBuilder ctxBuilder, List<byte> fnbc)
        { 
            switch(expr.astType)
            {
                case TokenASTType.SetValue:
                    break;

                case TokenASTType.GetGlobalVar:
                    break;

                case TokenASTType.GetMemberVar:
                    break;

                case TokenASTType.GetLocalVar:
                    break;

                case TokenASTType.GetParamVar:
                    break;

                case TokenASTType.GetFunction:
                    break;

                case TokenASTType.GetRegion:
                    break;

                case TokenASTType.GetThis:
                    break;

                case TokenASTType.FunctionDecl:
                    break;

                case TokenASTType.SetAfterAdd:
                    break;

                case TokenASTType.SetAfterSub:
                    break;

                case TokenASTType.SetAfterMul:
                    break;

                case TokenASTType.SetAfterDiv:
                    break;

                case TokenASTType.SetAfterMod:
                    break;

                case TokenASTType.SetAfterBitOr:
                    break;

                case TokenASTType.SetAfterBitAnd:
                    break;

                case TokenASTType.SetAfterBitXor:
                    break;

                case TokenASTType.SetAfterShiftL:
                    break;

                case TokenASTType.SetAfterShiftR:
                    break;

                case TokenASTType.Index:
                    break;

                case TokenASTType.IfStatement:
                    break;

                case TokenASTType.WhileStatement:
                    break;

                case TokenASTType.ForStatement:
                    break;

                case TokenASTType.DoWhileStatement:
                    break;

                case TokenASTType.Add:
                    {
                        TokenAST left = expr.branches[0];
                        TokenAST right = expr.branches[1];

                        // TODO: If these are addresses, get the address
                        ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);
                        ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                        ctxBuilder.PopType(left.evaluatingType);
                        ctxBuilder.PopType(right.evaluatingType);
                        switch (left.evaluatingType.typeName)
                        { 
                            case "int8":
                            case "uint8":
                            case "int16":
                            case "uint16":
                            case "int":
                            case "uint":
                                fnbc.Add( (byte)WASM.Instruction.i32_add);
                                break;

                            case "int64":
                            case "uint64":
                                fnbc.Add((byte)WASM.Instruction.i64_add);
                                break;

                            case "float":
                                fnbc.Add((byte)WASM.Instruction.f32_add);
                                break;

                            case "float64":
                                fnbc.Add((byte)WASM.Instruction.f64_add);
                                break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown add type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, left.evaluatingType);
                    }

                case TokenASTType.Sub:
                    {
                        TokenAST left = expr.branches[0];
                        TokenAST right = expr.branches[1];

                        // TODO: If these are addresses, get the address
                        ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);


                        ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                        ctxBuilder.PopType(left.evaluatingType);
                        ctxBuilder.PopType(right.evaluatingType);
                        switch (left.evaluatingType.typeName)
                        {
                            case "int8":
                            case "uint8":
                            case "int16":
                            case "uint16":
                            case "int":
                            case "uint":
                                fnbc.Add((byte)WASM.Instruction.i32_sub);
                                break;

                            case "int64":
                            case "uint64":
                                fnbc.Add((byte)WASM.Instruction.i64_sub);
                                break;

                            case "float":
                                fnbc.Add((byte)WASM.Instruction.f32_sub);
                                break;

                            case "float64":
                                fnbc.Add((byte)WASM.Instruction.f64_sub);
                                break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown add type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, left.evaluatingType);
                    }

                case TokenASTType.Mul:
                    {
                        TokenAST left = expr.branches[0];
                        TokenAST right = expr.branches[1];

                        // TODO: If these are addresses, get the address
                        ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);


                        ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                        ctxBuilder.PopType(left.evaluatingType);
                        ctxBuilder.PopType(right.evaluatingType);
                        switch (left.evaluatingType.typeName)
                        {
                            case "int8":
                            case "uint8":
                            case "int16":
                            case "uint16":
                            case "int":
                            case "uint":
                                fnbc.Add((byte)WASM.Instruction.i32_mul);
                                break;

                            case "int64":
                            case "uint64":
                                fnbc.Add((byte)WASM.Instruction.i64_mul);
                                break;

                            case "float":
                                fnbc.Add((byte)WASM.Instruction.f32_mul);
                                break;

                            case "float64":
                                fnbc.Add((byte)WASM.Instruction.f64_mul);
                                break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown add type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, left.evaluatingType);
                    }
                    break;

                case TokenASTType.Div:
                    break;

                case TokenASTType.Mod:
                    break;

                case TokenASTType.BitOr:
                    break;

                case TokenASTType.BitAnd:
                    break;

                case TokenASTType.BitXor:
                    break;

                case TokenASTType.BitInv:
                    break;

                case TokenASTType.BitShiftL:
                    break;

                case TokenASTType.BitShiftR:
                    break;

                case TokenASTType.Unprocessed:
                    break;

                case TokenASTType.RegisterVar:
                    break;

                case TokenASTType.DeclBool:
                    {
                        fnbc.Add((byte)WASM.Instruction.i32_const);
                        if(expr.token.fragment == "false" || expr.token.fragment == "0")
                            fnbc.Add(0);
                        else
                            fnbc.Add(1);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef( ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclUInt:
                    {
                        fnbc.Add((byte)WASM.Instruction.i32_const);
                        uint val = uint.Parse(expr.token.fragment);
                        // i32.const values are uninterpreted ints, which are
                        // signed
                        fnbc.AddRange(WASM.BinParse.EncodeSignedLEB((int)val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclSInt:
                    { 
                        fnbc.Add((byte)WASM.Instruction.i32_const);
                        int val = int.Parse(expr.token.fragment);
                        ctxBuilder.PushType(expr.evaluatingType);
                        fnbc.AddRange(WASM.BinParse.EncodeSignedLEB(val));
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclUInt64:
                    { 
                        fnbc.Add((byte)WASM.Instruction.i64_const);
                        ulong val = ulong.Parse(expr.token.fragment);
                        fnbc.AddRange(WASM.BinParse.EncodeSignedLEB((long)val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclSInt64:
                    { 
                        fnbc.Add((byte)WASM.Instruction.i64_const);
                        long val = long.Parse(expr.token.fragment);
                        fnbc.AddRange(WASM.BinParse.EncodeSignedLEB(val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclFloat:
                    { 
                        fnbc.Add((byte)WASM.Instruction.f32_const);
                        float val = float.Parse(expr.token.fragment);
                        fnbc.AddRange(System.BitConverter.GetBytes(val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclFloat64:
                    { 
                        fnbc.Add((byte)WASM.Instruction.f64_const);
                        double val = double.Parse(expr.token.fragment);
                        fnbc.AddRange(System.BitConverter.GetBytes(val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclString:
                    { 
                        // TODO:
                    }
                    break;

                case TokenASTType.UIntToFloat:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        fnbc.Add((byte)WASM.Instruction.f32_convert_i32_u);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.UIntToDouble:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        fnbc.Add((byte)WASM.Instruction.f64_convert_i32_u);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.UIntToUInt64:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        fnbc.Add((byte)WASM.Instruction.i64_extend_i32_u);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.SIntToSInt64:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        fnbc.Add((byte)WASM.Instruction.i64_extend_i32_s);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.FloatToUInt:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        ctxBuilder.PopType(expr.branches[0].evaluatingType);
                        ctxBuilder.PushType(expr.evaluatingType);
                        fnbc.Add((byte)WASM.Instruction.i32_trunc_f32_u);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, expr.evaluatingType);
                    }

                case TokenASTType.FloatToSInt:
                    break;

                case TokenASTType.FloatToSInt64:
                    break;

                case TokenASTType.FloatToUInt64:
                    break;

                case TokenASTType.FloatToFloat64:
                    break;

                case TokenASTType.DoubleToUInt:
                    break;

                case TokenASTType.DoubleToSInt:
                    break;

                case TokenASTType.DoubleToSInt64:
                    break;

                case TokenASTType.DoubleToUInt64:
                    break;

                case TokenASTType.DoubleToFloat:
                    break;

                case TokenASTType.Cast_Int:
                    break;

                case TokenASTType.Cast_UInt:
                    break;

                case TokenASTType.Cast_Int8:
                    break;

                case TokenASTType.Cast_UInt8:
                    break;

                case TokenASTType.Cast_Int16:
                    break;

                case TokenASTType.Cast_UInt16:
                    break;

                case TokenASTType.Cast_Int64:
                    break;

                case TokenASTType.Cast_UInt64:
                    break;

                case TokenASTType.Cast_Float:
                    break;

                case TokenASTType.Cast_Double:
                    break;

                case TokenASTType.Compare_Eq:
                    break;

                case TokenASTType.Compare_NEq:
                    break;

                case TokenASTType.Compare_LessThan:
                    break;

                case TokenASTType.Compare_LessThanEq:
                    break;

                case TokenASTType.Compare_GreaterThan:
                    break;

                case TokenASTType.Compare_GreaterThanEq:
                    break;

                case TokenASTType.CallMember:
                    break;

                case TokenASTType.CallGlobalFn:
                    { 
                        for(int i = 0; i < expr.branches.Count; ++i)
                        {
                            BuildBSFunctionExpression(fnd, expr.branches[i], wasmBuild, ctxBuilder, fnbc);
                        }

                        if(expr.synthObj == null)
                            throw new SynthExceptionImpossible("Missing function for global AST processing.");

                        SynthFuncDecl fn = expr.synthObj.CastFuncDecl();
                        if(fn == null)
                            throw new SynthExceptionImpossible("Missing function for global AST processing.");

                        if(fn.isStatic == false)
                            throw new SynthExceptionImpossible("Attempting to call static function which is not recorded as static.");

                        uint ? fnIdx = wasmBuild.GetFunctionIndex(fn);

                        if(fnIdx.HasValue == false)
                            throw new SynthExceptionImpossible("");

                        fnbc.Add((byte)WASM.Instruction.call);
                        fnbc.AddRange(WASM.BinParse.EncodeUnsignedLEB(fnIdx.Value));

                        // TODO: Figure out where return value (if any) is and
                        // place it in the return value.
                        if(fn.returnType == null)
                            return new ValueRef(ValueLoc.NoValue, 0, null);
                        else if(fn.returnType.intrinsic == true)
                            return new ValueRef( ValueLoc.ValueOnStack, 0, fn.returnType);
                        else
                            return new ValueRef(ValueLoc.ValueOnMem, 0, fn.returnType);
                    }

                case TokenASTType.DefaultParam:
                    break;
            }

            throw new SynthExceptionImpossible($"Unhandled AST type {expr.astType}.");
        }

        public void EnsureNoTreeChildNodes(TokenTree tt)
        { 
            if(tt.nodes.Count != 0)
                throw new SynthExceptionImpossible("Tree encountered unexpected children.");
        }

        public void PushType(SynthType type)
        { 
            // TODO:
        }

        public void PopType(SynthType type)
        { 
            //TODO:
        }
    }
}