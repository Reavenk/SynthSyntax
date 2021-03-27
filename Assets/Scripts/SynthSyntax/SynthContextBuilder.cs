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
            AddrLocal,
            AddrParam,
            AddrOnHeap,
            AddrOnMemStack,
        }

        public class ValueRef : SynthObj
        { 
            public ValueLoc valLoc;

            /// <summary>
            /// The local index on the stack - 
            /// or if the variable doesn't exist on the stack, the pointer
            /// to it.
            /// </summary>
            public int idx;

            /// <summary>
            /// The number of bytes on the memory -
            /// or if the variable doesn't exist on the stack, the memory.
            /// </summary>
            public int byteAlign;

            /// <summary>
            /// The global scope version of idx;
            /// </summary>
            public int fnIdx;

            /// <summary>
            /// The global scope version of fnByteAlign.
            /// </summary>
            public int fnByteAlign;

            /// <summary>
            /// The pointer indirection amount. A value of 0 is the actual variable
            /// value. A value of 1 is a pointer. A value of 2 is a pointer to a 
            /// pointer, etc.
            /// </summary>
            public int pointerAmt = 0;

            /// <summary>
            /// How to interpret the ValueRef.
            /// </summary>
            public SynthType varType;

            public ValueRef(ValueLoc valLoc, int idx, int byteAlign, SynthType varType, int pointerAmt = 0)
            { 
                this.valLoc = valLoc;
                this.idx = idx;
                this.byteAlign = byteAlign;
                this.varType = varType;
                this.pointerAmt = pointerAmt;
            }
        }

        /// <summary>
        /// The number of bytes pushed on the stack. Does not include local variables.
        /// </summary>
        public int currentStackSize = 0;

        // TODO: Because of technical reason of the language beeing able to freely take the
        // address of any parameter in a generic way, parameters directly on the local stack
        // are probably not going to be supported, and everything is going to be on the memory stack.
        public int totalLocalStackBytes = 0;
        public int totalLocalStack = 0;
        public List<ValueRef> locStkEle = new List<ValueRef>();
        public Dictionary<string, ValueRef> locStkVars = new Dictionary<string, ValueRef>();

        public int totalMemoryStackBytes = 0;
        public int totalMemoryStack = 0;
        public List<ValueRef> memStkEle = new List<ValueRef>();
        public Dictionary<string, ValueRef> memStkVars = new Dictionary<string, ValueRef>();

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

        public readonly SynthContextBuilder parent;

        public SynthContextBuilder(SynthContextBuilder parent)
        { 
            this.parent = parent;
        }


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

        void AddLocalVariable(Token declTok, SynthType type, string varName)
        {
            // TODO: Validate varName isn't a reserved word

            if( 
                this.locStkVars.ContainsKey(varName) == true || 
                this.memStkVars.ContainsKey(varName) == true)
            { 
                throw new SynthExceptionSyntax(declTok, "Variable name already exists.");
            }

            if (type.intrinsic == true)
            {
                // If we're dealing with an intrinsic value type, they're either 
                // 4 or 8 bytes, and can fit on the actual WASM stack.
                ValueRef vr = new ValueRef(ValueLoc.AddrLocal, locStkEle.Count, this.totalLocalStack, type);
                this.locStkEle.Add(vr);
                this.locStkVars.Add(varName, vr);
                this.totalLocalStack += type.GetByteSize();
            }
            else
            { 
                // Everything that's complexgets put on the secondary stack in the WASM mem
                // section. The biggest issue we're trying to solve with the secondary stack is
                // arbitrary byte alignment. A traditional WASM stack can't run through variable
                // bytes, and only allows alignments that are a multiple of 4.
                ValueRef vr = new ValueRef(ValueLoc.ValueOnMem, memStkEle.Count, this.totalMemoryStack, type);
                this.memStkEle.Add(vr);
                this.memStkVars.Add(varName, vr);
                this.totalMemoryStack += type.GetByteSize();
            }
        }

        public ValueRef GetLocalVariable(string name, bool recursive = true)
        { 
            ValueRef vrRet;
            if(this.locStkVars.TryGetValue(name, out vrRet) == true)
                return vrRet;

            if(this.memStkVars.TryGetValue(name, out vrRet) == true)
                return vrRet;

            if(recursive == true && this.parent != null)
                return this.parent.GetLocalVariable(name, true);

            return null;
        }

        public void CompileAlignment()
        { 
            int locMax = 0;
            int memMax = 0;

            if(this.parent != null)
            { 
                locMax = this.parent.totalLocalStack;
                memMax = this.parent.totalMemoryStackBytes;
            }


            for(int i = 0; i < this.locStkEle.Count; ++i)
            {
                ValueRef vr = this.locStkEle[i];
                vr.fnIdx += locMax;
            }

            for(int i = 0; i < this.memStkEle.Count; ++i)
            { 
                ValueRef vr = this.memStkEle[i];
                vr.fnByteAlign += memMax;
            }
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
                        return new TokenAST(tt.root, this, oi.intrinsicOperator, null, null, false, left, right);
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
                        return new TokenAST(tt.root, this, oi.intrinsicOperator, null, scope.GetType("bool"), false, left, right);
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
                        return new TokenAST(tt.root, this, oi.intrinsicOperator, null, scope.GetType("bool"), false, left, right);
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
                        return new TokenAST(tt.root, this, oi.intrinsicOperator, null, left.evaluatingType, false, left, right);
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
                        return new TokenAST(tt.root, this, oi.intrinsicOperator, null, left.evaluatingType, false, left, right);
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
                    return new TokenAST(tt.root, this, TokenASTType.DeclFloat64, null, scope.GetType("double"), false);

                case TokenType.tyFloat:
                    return new TokenAST(tt.root, this, TokenASTType.DeclFloat, null, scope.GetType("double"), false);

                case TokenType.tyInt:
                    return new TokenAST(tt.root, this, TokenASTType.DeclSInt, null, scope.GetType("int"), false);

                case TokenType.tyLong:
                    return new TokenAST(tt.root, this, TokenASTType.DeclSInt64, null, scope.GetType("int64"), false);
            }

            if(ret == null)
                return null;

            if(tt.nodes.Count > 0)
                throw new SynthExceptionImpossible("Unknown intrinsic.");

            return ret;
        }

        public TokenAST GenerateOperatorAST(string operatorName, Token tokOp, TokenAST left, TokenAST right)
        {
            // First check non-reversible entries.
            SynthFuncDecl sfd = left.evaluatingType.GetOperator(operatorName, right.evaluatingType, SynthScope.OperatorReversing.OnlyNonReversible);
            if (sfd != null)
                return new TokenAST(tokOp, this, TokenASTType.CallMember, sfd, sfd.returnType, false, left, right);

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
                return new TokenAST(tokOp, this, TokenASTType.CallMember, sfd, sfd.returnType, false, right, left);

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
            List<SynthContextBuilder> regCtxBuilders,
            WASMBuild build, 
            SynthScope invokingContext, 
            SynthFuncDecl function, 
            TokenTree node)
        {
            if(node.root.Matches(TokenType.tyBool) == true)
            {
                EnsureNoTreeChildNodes(node);
                return new TokenAST(node.root, this, TokenASTType.DeclBool, null, build.rootContext.GetType("bool"), false);
            }
            
            if(node.root.Matches(TokenType.tyInt) == true)
            {
                EnsureNoTreeChildNodes(node);
                // All ints are signed by default - they can be casted with the cast optimized out later.
                return new TokenAST(node.root, this, TokenASTType.DeclSInt, null, build.rootContext.GetType("int"), false);
            }

            if(node.root.Matches(TokenType.tyFloat) == true)
            {
                EnsureNoTreeChildNodes(node);
                return new TokenAST(node.root, this, TokenASTType.DeclFloat, null, build.rootContext.GetType("float"), false);
            }

            if(node.root.Matches(TokenType.tyDouble) == true)
            {
                EnsureNoTreeChildNodes(node);
                return new TokenAST(node.root, this, TokenASTType.DeclFloat64, null, build.rootContext.GetType("double"), false);
            }

            if(node.root.Matches(TokenType.tyString) == true)
            {
                EnsureNoTreeChildNodes(node);
                build.stringRepo.RegisterString(node.root.fragment);

                return new TokenAST(node.root, this, TokenASTType.DeclString, null, build.rootContext.GetType("string"), false);
            }

            if(node.root.Matches(TokenType.tyWord) == true)
            { 
                if(node.root.Matches("this") == true)
                { 
                    if(function.isStatic == true)
                        throw new SynthExceptionSyntax(node.root, "this cannot be used in a static function.");

                    SynthType_Struct rootScope = function.GetStructScope();
                    return new TokenAST(node.root, this, TokenASTType.GetThis, rootScope, rootScope, true);
                }

                // If it matches a known type, we've detected the token is attempting
                // to declare a variable in local scope.
                SynthType sty = function.GetType(node.root.fragment);
                if(sty != null)
                { 
                    if(node.nodes.Count < 2)
                        throw new SynthExceptionImpossible("Local variable declaration missing variable name.");

                    if(node.nodes[0].keyword != "varname") // TODO: Use const string
                        throw new SynthExceptionImpossible("Pipeline for defining local variable names is corrupt.");

                    this.AddLocalVariable(node.root, sty, node.nodes[0].root.fragment);

                    TokenAST astDeclVar = new TokenAST(node.root, this, TokenASTType.RegisterLocalVar, null, sty, true);
                    TokenAST astVarName = new TokenAST(node.nodes[0].root, this, TokenASTType.RegisterLocalVarName, null, null, false);
                    
                    astDeclVar.branches.Add(astVarName);
                    if(node.nodes.Count > 1)
                    {
                        TokenAST astVarInit = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[1]);
                        astDeclVar.branches.Add(astVarInit);

                        if(node.nodes.Count > 2)
                            throw new SynthExceptionImpossible("Variable declaration has invalid number of tree children.");
                    }
                    return astDeclVar;
                }

                // Compiler constant macros
                if(node.root.fragment == "__LINE__")
                {
                    return new TokenAST(
                        new Token(
                            node.root.line, 
                            node.root.line.ToString(), 
                            TokenType.tyInt), 
                        this, 
                        TokenASTType.DeclSInt, 
                        null, 
                        build.rootContext.GetType("int"), 
                        false);
                }

                ValueRef localVR = this.GetLocalVariable(node.root.fragment);
                if(localVR != null)
                    return new TokenAST(node.root, this, TokenASTType.GetLocalVar, localVR, localVR.varType, true);

                SynthVarValue svv = function.GetVar(node.root.fragment);
                if(svv != null)
                {
                    // TODO: Check if we're in a function context.
                    return new TokenAST(node.root, this, TokenASTType.GetMemberVar, svv, svv.type, true);
                }

                SynthCanidateFunctions canFns = function.GetCanidateFunctions(node.root.fragment);
                if(canFns == null || canFns.functions.Count > 0)
                    return new TokenAST(node.root, this, TokenASTType.GetFunction, canFns, null, false);

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

                TokenAST pred = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(node.nodes[0].root, "If statement predicate did not evaluate to a bool.");

                TokenAST astIf = new TokenAST(node.root, this, TokenASTType.IfStatement, null, null, false, pred);

                for(int i = 1; i < node.nodes.Count; ++i)
                    astIf.branches.Add(ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[i]));

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

                TokenAST pred = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(node.nodes[0].root, "While statement predicate did not evaludate to a bool.");

                TokenAST astWhile = new TokenAST(node.root, this, TokenASTType.WhileStatement, null, null, false, pred);

                for(int i = 1; i < node.nodes.Count; ++i)
                    astWhile.branches.Add(ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[i]));

                return astWhile;
            }

            if (node.keyword == "dowhile")
            {
                if(node.nodes.Count < 2)
                    throw new SynthExceptionImpossible("dowhile statement needs at least two trees, one for the body statements and one for the continue predicate.");

                TokenTree ttLast = node.nodes[node.nodes.Count - 1];
                TokenAST pred = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, ttLast);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(ttLast.root, "dowhile statement predicate did not evaluate to a bool.");

                TokenAST astDoWhile = new TokenAST(node.root, this, TokenASTType.DoWhileStatement, null, null, false);

                for(int i = 0; i < node.nodes.Count - 1; ++i)
                    astDoWhile.branches.Add(ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[i]));

                astDoWhile.branches.Add(pred);
                return astDoWhile;
            }

            if (node.root.MatchesSymbol("=") == true)
            {
                if(node.nodes.Count != 2)
                    throw new SynthExceptionImpossible("Equal needs exactly two trees to evaluate.");

                TokenAST left = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                if(left.hasAddress == false)
                    throw new SynthExceptionSyntax(node.nodes[0].root, "Left side of equation not addressable.");

                TokenAST right = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[1]);
                EnsureIntrinsicCompatibility(left.evaluatingType, ref right);

                TokenAST astEq = new TokenAST(node.root, this, TokenASTType.SetValue, null, left.evaluatingType, false, left, right);
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
                    {
                        TokenAST astParamVal = this.ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, tt);
                        astParams.Add(astParamVal);
                    }

                    // Default params not supported for now
                    TokenAST caller = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[1]);
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

                TokenAST astKey = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                TokenAST astSrc = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[1]);
                EnsureTypeIsBitCompatible(astSrc.evaluatingType, node.nodes[1].root);

                return new TokenAST(node.root, this, TokenASTType.Index, null, null, true, astSrc, astKey);
            }

            if (node.root.MatchesSymbol("+") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, this, TokenASTType.Add, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("-") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, this, TokenASTType.Sub, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("*") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, this, TokenASTType.Mul, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("/") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, this, TokenASTType.Div, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("%") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                return new TokenAST(node.root, this, TokenASTType.Mod, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("&") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, this, TokenASTType.BitAnd, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("|") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, this, TokenASTType.BitOr, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("^") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, this, TokenASTType.BitXor, null, left.evaluatingType, false, left, right);
            }

            if( node.root.MatchesSymbol(">>") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, this, TokenASTType.BitShiftR, null, left.evaluatingType, false, left, right);
            }

            if(node.root.MatchesSymbol("<<") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, this, TokenASTType.BitShiftL, null, left.evaluatingType, false, left, right);
            }

            if (node.root.MatchesSymbol("~") == true)
            {
                if(node.nodes.Count != 1)
                    throw new SynthExceptionSyntax(node.root, "~ operation expecting exactly 1 tree."); //Not a great error message

                TokenAST evVal = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                EnsureTypeIsBitCompatible(evVal.evaluatingType, node.nodes[0].root);

                return new TokenAST(node.root, this, TokenASTType.BitInv, null, evVal.evaluatingType, false, evVal);
            }

            if (node.root.MatchesSymbol("+=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                return new TokenAST(node.root, this, TokenASTType.SetAfterAdd, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("-=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                return new TokenAST(node.root, this, TokenASTType.SetAfterSub, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("*=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                return new TokenAST(node.root, this, TokenASTType.SetAfterMul, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("/=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                return new TokenAST(node.root, this, TokenASTType.SetAfterDiv, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("%=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);

                return new TokenAST(node.root, this, TokenASTType.SetAfterMod, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("&=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, this, TokenASTType.SetAfterBitAnd, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("|=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, this, TokenASTType.SetAfterBitOr, null, null, false, left, right);
            }

            if (node.root.MatchesSymbol("^=") == true)
            {
                TokenAST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new TokenAST(node.root, this, TokenASTType.SetAfterBitXor, null, null, false, left, right);
            }

            return null;
        }

        public void EnsureLeftAndRightCompatibility(
            List<SynthContextBuilder> regCtxBuilders, 
            WASMBuild build, 
            SynthScope invokingContext, 
            SynthFuncDecl fnDecl, 
            TokenTree tt, 
            out TokenAST left, 
            out TokenAST right, 
            bool allowCastingLeft, 
            bool leftRequireAddr)
        {
            if(tt.nodes.Count != 2)
                throw new SynthExceptionSyntax(tt.root, $"Error parsing {tt.root.fragment}.");

            left = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, fnDecl, tt.nodes[0]);
            right = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, fnDecl, tt.nodes[1]);

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

        public void EnsureIntrinsicCompatibility(TokenAST left, ref TokenAST right)
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
        public bool EnsureIntrinsicCompatibility(SynthType styLeft, ref TokenAST right, bool throwOnFail = true)
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
                    TokenAST cast = new TokenAST(right.token, this, TokenASTType.Cast_Double, styLeft, styLeft, false, right);
                    right = cast;
                    return true;
                }

            }

            if (CountMatches(hs, "int", "int8", "int16", "int64", "uint", "uint8", "uint16", "uint16") > 0 && hs.Contains("float"))
            {
                if (styLeft.typeName == "float")
                {
                    TokenAST cast = new TokenAST(right.token, this, TokenASTType.Cast_Float, styLeft, styLeft, false, right);
                    right = cast;
                    return true;
                }
            }

            if (CountMatches(hs, "int", "int8", "int16", "int64", "uint", "uint8", "uint16", "uint16") > 0 && hs.Contains("double"))
            {
                if (styLeft.typeName == "double")
                {
                    TokenAST cast = new TokenAST(right.token, this, TokenASTType.Cast_Double, styLeft, styLeft, false, right);
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
                    TokenAST cast = new TokenAST(right.token, this, GetCastInstrinsicType(styLeft.typeName), styLeft, styLeft, false, right);
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

        static HashSet<string> match32 = new HashSet<string> { "int8", "uint8", "int16", "uint16", "int", "uint" };
        static HashSet<string> match64 = new HashSet<string> { "int64", "uint64" };
        public ValueRef BuildBSFunctionExpression(SynthFuncDecl fnd, TokenAST expr, WASMBuild wasmBuild, SynthContextBuilder ctxBuilder, List<byte> fnbc)
        { 
            switch(expr.astType)
            {
                case TokenASTType.SetValue:
                    { 
                        if(expr.branches.Count != 2)
                            throw new SynthExceptionCompile("Setting a value expected two parameters, a target and a destination.");

                        ValueRef vrLeft = this.BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);

                        if(vrLeft.valLoc == ValueLoc.AddrLocal)
                        { 
                            ValueRef vrRight = this.BuildBSFunctionExpression(fnd, expr.branches[1], wasmBuild, ctxBuilder, fnbc);

                            if(vrRight.valLoc == ValueLoc.AddrLocal)
                            {
                                fnbc.Add((byte)WASM.Instruction.local_get);
                                fnbc.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)vrRight.idx));
                            }
                            else if(vrRight.valLoc == ValueLoc.ValueOnStack)
                            { } // Already on stack
                            else
                                throw new SynthExceptionCompile($"Setting from RValue source {vrRight.valLoc} currently not supported.");

                            
                            fnbc.Add((byte)WASM.Instruction.local_set);
                            fnbc.AddRange(WASM.BinParse.EncodeSignedLEB((uint)vrLeft.idx));

                            return new ValueRef(ValueLoc.NoValue, -1, -1, vrLeft.varType);
                        }
                        else
                            throw new SynthExceptionCompile("Setting non-local variables currently not supported.");
                    }

                case TokenASTType.GetGlobalVar:
                    break;

                case TokenASTType.GetMemberVar:
                    break;

                case TokenASTType.GetLocalVar:
                    { 
                        ValueRef vr = this.GetLocalVariable(expr.token.fragment);

                        if(vr == null)
                            throw new SynthExceptionImpossible("Expected local variable wasn't found.");

                        if(vr.varType.intrinsic == false)
                            throw new SynthExceptionCompile("Non intrinsic local variables not supported yet.");

                        if (vr.valLoc == ValueLoc.ValueOnMem || vr.valLoc == ValueLoc.AddrOnMemStack || vr.valLoc == ValueLoc.AddrOnHeap)
                            throw new SynthExceptionCompile("Mem stack and heap variables not supported yet.");
                        
                        fnbc.Add((byte)WASM.Instruction.local_get);
                        fnbc.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)vr.idx));

                        return vr;
                    }

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
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
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
                                throw new SynthExceptionImpossible($"AST Unknown sub type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
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
                                throw new SynthExceptionImpossible($"AST Unknown mul type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
                    }

                case TokenASTType.Div:
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
                            case "int":
                            case "int16":
                                fnbc.Add((byte)WASM.Instruction.i32_div_s);
                                break;

                            case "uint8":
                            case "uint16":
                            case "uint":
                                fnbc.Add((byte)WASM.Instruction.i32_div_u);
                                break;

                            case "int64":
                                fnbc.Add((byte)WASM.Instruction.i64_div_s);
                                break;

                            case "uint64":
                                fnbc.Add((byte)WASM.Instruction.i64_div_u);
                                break;

                            case "float":
                                fnbc.Add((byte)WASM.Instruction.f32_div);
                                break;

                            case "float64":
                                fnbc.Add((byte)WASM.Instruction.f64_div);
                                break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown div type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
                    }

                case TokenASTType.Mod:
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
                            case "int":
                            case "int16":
                                fnbc.Add((byte)WASM.Instruction.i32_rem_s);
                                break;

                            case "uint8":
                            case "uint16":
                            case "uint":
                                fnbc.Add((byte)WASM.Instruction.i32_rem_u);
                                break;

                            case "int64":
                                fnbc.Add((byte)WASM.Instruction.i64_rem_s);
                                break;

                            case "uint64":
                                fnbc.Add((byte)WASM.Instruction.i64_rem_u);
                                break;

                            case "float":
                                throw new SynthExceptionSyntax(expr.token, "Floating point modulus not currently supported.");
                                //fnbc.Add((byte)WASM.Instruction.f32_div);
                                //break;

                            case "float64":
                                throw new SynthExceptionSyntax(expr.token, "Double floating point modulus not currently supported.");
                                //fnbc.Add((byte)WASM.Instruction.f64_);
                                //break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown div type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
                    }

                case TokenASTType.BitOr:
                case TokenASTType.BitAnd:
                case TokenASTType.BitXor:
                    {
                        TokenAST left = expr.branches[0];
                        TokenAST right = expr.branches[1];

                        WASM.Instruction instr32;
                        WASM.Instruction instr64;
                        switch(expr.astType)
                        { 
                            case TokenASTType.BitOr:
                                instr32 = WASM.Instruction.i32_or;
                                instr64 = WASM.Instruction.i64_or;
                                break;

                            default:
                            case TokenASTType.BitAnd:
                                instr32 = WASM.Instruction.i32_and;
                                instr64 = WASM.Instruction.i64_and;
                                break;

                            case TokenASTType.BitXor:
                                instr32 = WASM.Instruction.i32_xor;
                                instr64 = WASM.Instruction.i64_xor;
                                break;
                        }

                        HashSet<string> match32 = new HashSet<string>{ "int8", "uint8", "int16", "uint16", "int", "uint"};
                        HashSet<string> match64 = new HashSet<string>{ "int64", "uint64" };
                        
                        // It's uncertain how the autocasting should work
                        if(
                            match32.Contains(left.evaluatingType.typeName) == true && 
                            match32.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);
                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                            fnbc.Add((byte)instr32);
                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int"));
                        }
                        else if(
                            match64.Contains(left.evaluatingType.typeName) == true &&
                            match64.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);
                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                            fnbc.Add((byte)instr64);
                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int64"));
                        }
                        else
                        {
                            throw new SynthExceptionSyntax(
                                expr.token, 
                                $"Cannot combine types {left.evaluatingType} and {right.evaluatingType} for a bit or operation.");
                        }
                    }

                case TokenASTType.BitInv:
                    {
                        if(expr.branches.Count != 1)
                            throw new SynthExceptionImpossible("Bit inv has an unexpected node count.");

                        TokenAST astVal = expr.branches[0];

                        // https://github.com/WebAssembly/design/issues/701
                        if (match32.Contains(astVal.evaluatingType.typeName) == true)
                        {
                            ValueRef vrVal = BuildBSFunctionExpression(fnd, astVal, wasmBuild, ctxBuilder, fnbc);

                            fnbc.Add((byte)WASM.Instruction.i32_const);
                            fnbc.AddRange(WASM.BinParse.EncodeSignedLEB(-1));
                            fnbc.Add((byte)WASM.Instruction.i32_xor);
                        }
                        else if(match64.Contains(astVal.evaluatingType.typeName) == true)
                        {
                            ValueRef vrVal = BuildBSFunctionExpression(fnd, astVal, wasmBuild, ctxBuilder, fnbc);

                            fnbc.Add((byte)WASM.Instruction.i64_const);
                            fnbc.AddRange(WASM.BinParse.EncodeSignedLEB((long)-1));
                            fnbc.Add((byte)WASM.Instruction.i64_xor);
                        }
                        else
                        {
                            throw new SynthExceptionSyntax(
                                expr.token, 
                                $"Cannot invert bits of type {astVal.evaluatingType.typeName}.");
                        }

                        return new ValueRef(ValueLoc.ValueOnStack, -1, -1, astVal.evaluatingType);
                    }

                case TokenASTType.BitShiftL:
                    {
                        TokenAST left = expr.branches[0];
                        TokenAST right = expr.branches[1];

                        // It's uncertain how the autocasting should work
                        if (
                            match32.Contains(left.evaluatingType.typeName) == true &&
                            match32.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);
                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                            fnbc.Add((byte)WASM.Instruction.i32_shl);
                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int"));
                        }
                        else if (
                            match64.Contains(left.evaluatingType.typeName) == true &&
                            match64.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);
                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                            fnbc.Add((byte)WASM.Instruction.i64_shl);
                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int64"));
                        }
                        else
                        {
                            throw new SynthExceptionSyntax(
                                expr.token,
                                $"Cannot combine types {left.evaluatingType} and {right.evaluatingType} for a bit shift operation.");
                        }
                    }

                case TokenASTType.BitShiftR:
                    {
                        TokenAST left = expr.branches[0];
                        TokenAST right = expr.branches[1];

                        HashSet<string> match32 = new HashSet<string> { "int8", "uint8", "int16", "uint16", "int", "uint" };
                        HashSet<string> match64 = new HashSet<string> { "int64", "uint64" };

                        // It's uncertain how the autocasting should work
                        if (
                            match32.Contains(left.evaluatingType.typeName) == true &&
                            match32.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);
                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                            // TODO: More work needs to be done to figure out casting of lower unsigned values
                            switch(right.evaluatingType.typeName)
                            { 
                                case "int8":
                                case "int16":
                                case "int":
                                    fnbc.Add((byte)WASM.Instruction.i32_shr_s);
                                    break;

                                case "uint8":
                                case "uint16":
                                case "uint":
                                    fnbc.Add((byte)WASM.Instruction.i32_shr_u);
                                    break;
                            }

                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int"));
                        }
                        else if (
                            match64.Contains(left.evaluatingType.typeName) == true &&
                            match64.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnbc);
                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnbc);

                            if(right.evaluatingType.typeName == "int64")
                                fnbc.Add((byte)WASM.Instruction.i64_shr_s);
                            else if(right.evaluatingType.typeName == "uint64")
                                fnbc.Add((byte)WASM.Instruction.i64_shr_u);

                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int64"));
                        }
                        else
                        {
                            throw new SynthExceptionSyntax(
                                expr.token,
                                $"Cannot combine types {left.evaluatingType} and {right.evaluatingType} for a bit shift operation.");
                        }
                    }

                case TokenASTType.Unprocessed: 
                    break;

                case TokenASTType.RegisterLocalVar:
                    // The language doesn't return any kind of value for registering a local.
                    {
                        ValueRef vrReg = this.GetLocalVariable(expr.branches[0].token.fragment);

                        // The first branch is the variable name, the second is 
                        // an initialization
                        if (expr.branches.Count > 1) 
                        {

                            // This will put/calculate the evaluated intrinsic value on the stack.
                            ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[1], wasmBuild, ctxBuilder, fnbc);
                            if(vr.varType.intrinsic == false)
                                throw new SynthExceptionCompile("Initializing non-intrinsic variables on the stack is currently not supported.");

                            fnbc.Add((byte)WASM.Instruction.local_set);
                            fnbc.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)vrReg.idx));
                        }
                        return new ValueRef(ValueLoc.NoValue, 0, 0, vrReg.varType);
                    }

                case TokenASTType.DeclBool:
                    {
                        fnbc.Add((byte)WASM.Instruction.i32_const);
                        if(expr.token.fragment == "false" || expr.token.fragment == "0")
                            fnbc.Add(0);
                        else
                            fnbc.Add(1);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef( ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclUInt:
                    {
                        fnbc.Add((byte)WASM.Instruction.i32_const);
                        uint val = uint.Parse(expr.token.fragment);
                        // i32.const values are uninterpreted ints, which are
                        // signed
                        fnbc.AddRange(WASM.BinParse.EncodeSignedLEB((int)val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclSInt:
                    { 
                        fnbc.Add((byte)WASM.Instruction.i32_const);
                        int val = int.Parse(expr.token.fragment);
                        ctxBuilder.PushType(expr.evaluatingType);
                        fnbc.AddRange(WASM.BinParse.EncodeSignedLEB(val));
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclUInt64:
                    { 
                        fnbc.Add((byte)WASM.Instruction.i64_const);
                        ulong val = ulong.Parse(expr.token.fragment);
                        fnbc.AddRange(WASM.BinParse.EncodeSignedLEB((long)val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclSInt64:
                    { 
                        fnbc.Add((byte)WASM.Instruction.i64_const);
                        long val = long.Parse(expr.token.fragment);
                        fnbc.AddRange(WASM.BinParse.EncodeSignedLEB(val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclFloat:
                    { 
                        fnbc.Add((byte)WASM.Instruction.f32_const);
                        float val = float.Parse(expr.token.fragment);
                        fnbc.AddRange(System.BitConverter.GetBytes(val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.DeclFloat64:
                    { 
                        fnbc.Add((byte)WASM.Instruction.f64_const);
                        double val = double.Parse(expr.token.fragment);
                        fnbc.AddRange(System.BitConverter.GetBytes(val));
                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
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
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.UIntToDouble:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        fnbc.Add((byte)WASM.Instruction.f64_convert_i32_u);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.UIntToUInt64:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        fnbc.Add((byte)WASM.Instruction.i64_extend_i32_u);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.SIntToSInt64:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        fnbc.Add((byte)WASM.Instruction.i64_extend_i32_s);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case TokenASTType.FloatToUInt:
                    {
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnbc);
                        ctxBuilder.PopType(expr.branches[0].evaluatingType);
                        ctxBuilder.PushType(expr.evaluatingType);
                        fnbc.Add((byte)WASM.Instruction.i32_trunc_f32_u);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
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
                            return new ValueRef(ValueLoc.NoValue, 0, 0, null);
                        else if(fn.returnType.intrinsic == true)
                            return new ValueRef( ValueLoc.ValueOnStack, 0, 0, fn.returnType);
                        else
                            return new ValueRef(ValueLoc.ValueOnMem, 0, 0, fn.returnType);
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