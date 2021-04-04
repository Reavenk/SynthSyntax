using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynNestingBuilder : SynObj
    {
        public struct OperatorInfo
        {
            public string tokStr;
            public ASTOp intrinsicOperator;

            public OperatorInfo(string tokStr, ASTOp intOp)
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

        public enum SignMode
        { 
            Signed,
            Unsigned,
            Agnostic
        }

        /// <summary>
        /// The number of bytes pushed on the stack. Does not include local variables.
        /// </summary>
        public int currentStackSize = 0;

        // TODO: Because of technical reason of the language beeing able to freely take the
        // address of any parameter in a generic way, parameters directly on the local stack
        // are probably not going to be supported, and everything is going to be on the memory stack.
        public int totalLocalStack = 0;
        public List<ValueRef> locStkEle = new List<ValueRef>();
        public Dictionary<string, ValueRef> locStkVars = new Dictionary<string, ValueRef>();

        public int totalMemoryStackBytes = 0;
        public List<ValueRef> memStkEle = new List<ValueRef>();
        public Dictionary<string, ValueRef> memStkVars = new Dictionary<string, ValueRef>();


        List<ValueRef> allLocalVars = new List<ValueRef>();

        List<AST> asts = new List<AST>();

        OperatorInfo[] OperatorsAssign =
            new OperatorInfo[]{
                new OperatorInfo("=",   ASTOp.SetValue),
                new OperatorInfo("+=",  ASTOp.SetAfterAdd),
                new OperatorInfo("-=",  ASTOp.SetAfterSub),
                new OperatorInfo("*=",  ASTOp.SetAfterMul),
                new OperatorInfo("/=",  ASTOp.SetAfterDiv),
                new OperatorInfo("%=",  ASTOp.SetAfterMod),
                new OperatorInfo("|=",  ASTOp.SetAfterBitOr),
                new OperatorInfo("&=",  ASTOp.SetAfterBitAnd),
                new OperatorInfo("^=",  ASTOp.SetAfterBitXor),
                new OperatorInfo(">>=", ASTOp.SetAfterShiftL),
                new OperatorInfo("<<=", ASTOp.SetAfterShiftR) };

        OperatorInfo[] OperatorsCmp =
            new OperatorInfo[]{
                new OperatorInfo("==",  ASTOp.Compare_Eq),
                new OperatorInfo("!=",  ASTOp.Compare_NEq),
                new OperatorInfo(">",   ASTOp.Compare_GreaterThan),
                new OperatorInfo(">=",  ASTOp.Compare_GreaterThanEq),
                new OperatorInfo("<",   ASTOp.Compare_LessThan),
                new OperatorInfo("<=",  ASTOp.Compare_GreaterThanEq)};

        OperatorInfo[] OperatorsBitWise =
            new OperatorInfo[]{
                new OperatorInfo("|",   ASTOp.BitOr),
                new OperatorInfo("&",   ASTOp.BitAnd),
                new OperatorInfo("^",   ASTOp.BitXor),
                new OperatorInfo("~",   ASTOp.BitInv),
                new OperatorInfo(">>",  ASTOp.BitShiftL),
                new OperatorInfo("<<",  ASTOp.BitShiftR)};

        OperatorInfo[] OperatorsMath =
            new OperatorInfo[]{
                new OperatorInfo("+",   ASTOp.Add),
                new OperatorInfo("-",   ASTOp.Sub),
                new OperatorInfo("*",   ASTOp.Mul),
                new OperatorInfo("/",   ASTOp.Div),
                new OperatorInfo("%",   ASTOp.Mod)};

        public readonly SynNestingBuilder parent;

        public SynNestingBuilder(SynNestingBuilder parent)
        { 
            this.parent = parent;
        }


        AST ProcessLogic(SynScope scope, TokenTree tt)
        { 
            AST ast = ProcessMathTree(scope, tt);
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

        void AddLocalVariable(Token declTok, SynType type, string varName, int ptrDepth)
        {
            // TODO: Validate varName isn't a reserved word

            if( 
                this.locStkVars.ContainsKey(varName) == true || 
                this.memStkVars.ContainsKey(varName) == true)
            { 
                throw new SynthExceptionSyntax(declTok, "Variable name already exists.");
            }

            // Everything that's complex gets put on the secondary stack in the WASM mem
            // section. The biggest issue we're trying to solve with the secondary stack is
            // arbitrary byte alignment. A traditional WASM stack can't run through variable
            // bytes, and only allows alignments that are a multiple of 4.
            ValueRef vr = new ValueRef(ValueLoc.ValueOnMemStack, memStkEle.Count, this.totalMemoryStackBytes, type, ptrDepth);
            this.memStkEle.Add(vr);
            this.memStkVars.Add(varName, vr);
            this.totalMemoryStackBytes += vr.GetByteSize();

            this.allLocalVars.Add(vr);
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

        /// <summary>
        /// Perform CompileAlignment, but based the alignment off 
        /// the parent scope.
        /// </summary>
        public void CompileAlignment()
        { 
            int locMax = 0;
            int memMax = 0;

            if(this.parent != null)
            { 
                // This is more of a graceful handler for a situation that
                // should never happen.
                //
                // The first scope's CompileAlignment (that has not parent) 
                // should use the SyhtFuncDecl overload.
                locMax = this.parent.totalLocalStack;
                memMax = this.parent.totalMemoryStackBytes;

            }
            this.CompileAlignment(locMax, memMax);
        }

        /// <summary>
        /// Perform CompileAlignment for the base scope, which bases
        /// alignment off the function parameters.
        /// </summary>
        /// <param name="fnBase"></param>
        public void CompileAlignment(SynFuncDecl fnBase)
        {
            this.CompileAlignment(
                fnBase.parameterSet.TotalLocalIndices, 
                fnBase.parameterSet.TotalMemStackByte);
        }

        private void CompileAlignment(int locBase, int memBase)
        {

            for (int i = 0; i < this.locStkEle.Count; ++i)
            {
                ValueRef vr = this.locStkEle[i];
                vr.fnIdx = locBase + vr.idx;
            }


            for (int i = 0; i < this.memStkEle.Count; ++i)
            {
                ValueRef vr = this.memStkEle[i];
                vr.fnByteAlign = memBase + vr.byteAlign;
            }

            this.totalLocalStack += locBase;
            this.totalMemoryStackBytes += memBase;
        }

        AST ProcessMathTree(SynScope scope, TokenTree tt)
        {
            // TODO: Is this a duplicate of ProcessMathOperators()?

            foreach (OperatorInfo oi in OperatorsCmp)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    AST left = ProcessLogic(scope, tt.nodes[0]);
                    AST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatibility(ref left, ref right, true);

                        return new AST(
                            tt.root, 
                            this, 
                            oi.intrinsicOperator, 
                            null, 
                            scope.GetType("bool"), 
                            false,
                            AST.CombineManifests(left, right), 
                            0,
                            left, 
                            right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        

        AST ProcessComparison(SynScope scope, TokenTree tt)
        {
            foreach(OperatorInfo oi in OperatorsCmp)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    AST left = ProcessLogic(scope, tt.nodes[0]);
                    AST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatibility(ref left, ref right, true);

                        return new AST(
                            tt.root, 
                            this, 
                            oi.intrinsicOperator, 
                            null, 
                            scope.GetType("bool"), 
                            false,
                            AST.CombineManifests(left, right), 
                            0,
                            left, 
                            right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        

        AST ProcessBitOperators(SynScope scope, TokenTree tt)
        {
            foreach (OperatorInfo oi in OperatorsBitWise)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    AST left = ProcessLogic(scope, tt.nodes[0]);
                    AST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatibility(ref left, ref right, true);

                        return new AST(
                            tt.root, 
                            this, 
                            oi.intrinsicOperator, 
                            null, 
                            left.evaluatingType, 
                            false,
                            AST.CombineManifests(left, right), 
                            0,
                            left, 
                            right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        AST ProcessMathOperators(SynScope scope, TokenTree tt)
        {
            foreach (OperatorInfo oi in OperatorsBitWise)
            {
                if (tt.root.MatchesSymbol(oi.tokStr) == true)
                {
                    AST left = ProcessLogic(scope, tt.nodes[0]);
                    AST right = ProcessLogic(scope, tt.nodes[1]);

                    if (
                        left.evaluatingType.intrinsic == true &&
                        right.evaluatingType.intrinsic == true)
                    {
                        EnsureIntrinsicCompatibility(ref left, ref right, true);

                        return new AST(
                            tt.root, 
                            this, 
                            oi.intrinsicOperator, 
                            null, left.evaluatingType, 
                            false,
                            AST.CombineManifests(left, right), 
                            0,
                            left, 
                            right);
                    }
                    else
                        return GenerateOperatorAST(oi.tokStr, tt.root, left, right);
                }
            }

            return null;
        }

        /// <summary>
        /// Given a Token number type, create the AST conversion of it.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name="tt"></param>
        /// <returns></returns>
        AST ProcessIntrinsic(SynScope scope, TokenTree tt)
        {
            // TODO: Is this function redundant compared to ProcessFunctionExpression()?
            // Check if this should be removed.

            AST ret = null;

            switch(tt.root.type)
            { 
                case TokenType.tyDouble:
                    return new AST(tt.root, this, ASTOp.DeclFloat64, null, scope.GetType("double"), false, AST.DataManifest.ValueConst, 0);

                case TokenType.tyFloat:
                    return new AST(tt.root, this, ASTOp.DeclFloat, null, scope.GetType("double"), false, AST.DataManifest.ValueConst, 0);

                case TokenType.tyInt:
                    return new AST(tt.root, this, ASTOp.DeclSInt, null, scope.GetType("int"), false, AST.DataManifest.ValueConst, 0);

                case TokenType.tyLong:
                    return new AST(tt.root, this, ASTOp.DeclSInt64, null, scope.GetType("int64"), false, AST.DataManifest.ValueConst, 0);
            }

            if(ret == null)
                return null;

            if(tt.nodes.Count > 0)
                throw new SynthExceptionImpossible("Unknown intrinsic.");

            return ret;
        }

        public AST GenerateOperatorAST(string operatorName, Token tokOp, AST left, AST right)
        {
            // First check non-reversible entries.
            SynFuncDecl sfd = 
                left.evaluatingType.GetOperator(
                    operatorName, 
                    right.evaluatingType, 
                    SynScope.OperatorReversing.OnlyNonReversible);

            if (sfd != null)
            {
                return new AST(
                    tokOp, 
                    this, 
                    ASTOp.CallMember, 
                    sfd, 
                    sfd.returnType, 
                    false,
                    AST.CombineManifests(left, right), 
                    0,
                    left, 
                    right);
            }

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
            sfd = left.evaluatingType.GetOperator(operatorName, left.evaluatingType, SynScope.OperatorReversing.OnlyReversible);
            if (sfd != null)
            {
                return new AST(
                    tokOp, 
                    this, 
                    ASTOp.CallMember, 
                    sfd, 
                    sfd.returnType, 
                    false, 
                    AST.DataManifest.Procedural, 
                    0,
                    right, 
                    left);
            }

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
        public AST ProcessFunctionExpression(
            List<SynNestingBuilder> regCtxBuilders,
            WASMBuild build, 
            SynScope invokingContext, 
            SynFuncDecl function, 
            TokenTree node)
        {
            // We may formalize this later instead of piggybacking off the fact
            // that the function has fallback access to the base scope where
            // instrinsics are defined.
            SynScope intrinsicSource = function;

            if(node.root.Matches(TokenType.tyBool) == true)
            {
                EnsureNoTreeChildNodes(node);
                return new AST(node.root, this, ASTOp.DeclBool, null, build.rootContext.GetType("bool"), false, AST.DataManifest.ValueConst, 0);
            }
            
            if(node.root.Matches(TokenType.tyInt) == true)
            {
                EnsureNoTreeChildNodes(node);
                // All ints are signed by default - they can be casted with the cast optimized out later.
                return new AST(node.root, this, ASTOp.DeclSInt, null, build.rootContext.GetType("int"), false, AST.DataManifest.ValueConst, 0);
            }

            if(node.root.Matches(TokenType.tyFloat) == true)
            {
                EnsureNoTreeChildNodes(node);
                return new AST(node.root, this, ASTOp.DeclFloat, null, build.rootContext.GetType("float"), false, AST.DataManifest.ValueConst,0 );
            }

            if(node.root.Matches(TokenType.tyDouble) == true)
            {
                EnsureNoTreeChildNodes(node);

                return new AST(
                    node.root, 
                    this, 
                    ASTOp.DeclFloat64, 
                    null, 
                    build.rootContext.GetType("double"), 
                    false, 
                    AST.DataManifest.ValueConst,
                    0);
            }

            if(node.root.Matches(TokenType.tyString) == true)
            {
                EnsureNoTreeChildNodes(node);
                build.stringRepo.RegisterString(node.root.fragment);

                // Placeholder string, declaring strings currently not supported.
                return new AST(
                    node.root, 
                    this, 
                    ASTOp.DeclString, 
                    null, 
                    build.rootContext.GetType("string"), 
                    false, 
                    AST.DataManifest.ValueConst,
                    0);
            }

            if(node.root.MatchesSymbol(".") == true)
            { 
                if(node.nodes.Count != 2)
                    throw new SynthExceptionImpossible(". operator expected two nodes.");

                AST astSrc = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);

                if(node.nodes[1].root.Matches(TokenType.tyWord) == false || node.nodes[1].nodes.Count != 0)
                    throw new SynthExceptionSyntax(node.nodes[1].root, "Invalid dereference type.");

                SynVarValue svv = astSrc.evaluatingType.GetVar(node.nodes[1].root.fragment);

                if(svv != null)
                {
                    // TODO: Do we support const member variables?
                    AST astDeref = 
                        new AST(
                            node.nodes[1].root, 
                            this, 
                            ASTOp.DerefName, 
                            svv, 
                            svv.type, 
                            false, 
                            AST.DataManifest.NoData,
                            0);

                    return new AST(
                        node.root, 
                        this, 
                        ASTOp.GetMemberVar, 
                        svv, 
                        svv.type, 
                        true, 
                        AST.DataManifest.Procedural,
                        0,
                        astSrc,
                        astDeref);
                }

                // If a variable dereference wasn't found, the only other possibility is a 
                // method call
                if(node.nodes[1].root.Matches(TokenType.tyWord) && node.nodes[1].nodes.Count == 0)
                {
                    SynCanidateFuncs scf = astSrc.evaluatingType.GetCanidateFunctions(node.nodes[1].root.fragment);
                    AST astPropose = new AST(node.nodes[1].root, this, ASTOp.ProposeMethod, scf, null, false, AST.DataManifest.NoData, 0, astSrc);
                    return astPropose;
                }

                throw new SynthExceptionSyntax(node.root, "Unhandled dereference.");
            }

            if(node.root.Matches(TokenType.tyWord) == true)
            { 
                if(node.keyword == "cast")
                { 
                    // TODO: Later on for more complex types, we may need to 
                    // use class casting constructors.

                    if(node.nodes.Count != 1)
                        throw new SynthExceptionImpossible("Invalid AST branches for casting.");

                    string typeCast = node.root.fragment;
                    SynType styCast = intrinsicSource.GetType(typeCast);

                    if(styCast == null)
                        throw new SynthExceptionSyntax(node.root, "Could not find existed casted type.");

                    AST astCast = 
                        ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);

                    // Having an AST type for each instrinsic cast seems a little bloated. 
                    // There may be a more elegant way to do all this.
                    switch (typeCast)
                    { 
                        case "int8":
                        case "uint8":
                        case "int16":
                        case "uint16":
                        case "int":
                        case "uint":
                        case "int64":
                        case "float":
                        case "float64":
                            return new AST(node.root, this, ASTOp.ExplicitCast, null, styCast, false, AST.CastManifest(astCast.manifest), 0, astCast);

                        case "bool":
                        default:
                            throw new SynthExceptionCompile($"Cannot support casting of type {typeCast}.");
                    }
                }

                if(node.root.Matches("this") == true)
                { 
                    if(function.isStatic == true)
                        throw new SynthExceptionSyntax(node.root, "this cannot be used in a static function.");

                    SynStruct rootScope = function.GetStructScope();
                    return new AST(node.root, this, ASTOp.GetThis, rootScope, rootScope, true, AST.DataManifest.Procedural, 0);
                }

                if(node.root.Matches("return") == true)
                { 
                    if(node.nodes.Count != 1)
                        throw new SynthExceptionImpossible("return keyword expected exactly one AST expression node.");

                    AST astRet = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                    return new AST(node.root, this, ASTOp.ReturnValue, null, astRet.evaluatingType, astRet.hasAddress, AST.DataManifest.Procedural, 0, astRet);
                }

                // If it matches a known type, we've detected the token is attempting
                // to declare a variable in local scope.
                SynType sty = function.GetType(node.root.fragment);
                if(sty != null)
                { 
                    // Constructor is already handled elsewhere
                    if(node.nodes.Count < 1)
                        throw new SynthExceptionImpossible("Local variable declaration missing variable name.");

                    if(node.nodes[0].keyword != "varname") // TODO: Use const string
                        throw new SynthExceptionImpossible("Pipeline for defining local variable names is corrupt.");

                    int ptrDepth = 0;
                    foreach(Token t in node.toksToProcess)
                    { 
                        if(t.MatchesSymbol("@") == true)
                            ++ptrDepth;
                    }

                    this.AddLocalVariable(node.root, sty, node.nodes[0].root.fragment, ptrDepth);

                    AST astDeclVar = 
                        new AST(node.root, this, ASTOp.RegisterLocalVar, null, sty, true, AST.DataManifest.NoData, 0);

                    AST astVarName = 
                        new AST(node.nodes[0].root, this, ASTOp.RegisterLocalVarName, null, null, false, AST.DataManifest.NoData, 0);

                    //int pointerLvl = 0;
                    //foreach(Token t_ttp in node.toksToProcess)
                    //{ 
                    //    if(t_ttp.MatchesSymbol("@") == true)
                    //        ++pointerLvl;
                    //}
                    
                    astDeclVar.branches.Add(astVarName);
                    if(node.nodes.Count > 1)
                    {
                        AST astVarInit = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[1]);
                        astDeclVar.branches.Add(astVarInit);

                        if(node.nodes.Count > 2)
                            throw new SynthExceptionImpossible("Variable declaration has invalid number of tree children.");
                    }
                    return astDeclVar;
                }

                // Compiler constant macros
                if(node.root.fragment == "__LINE__")
                {
                    return new AST(
                        new Token(
                            node.root.line, 
                            node.root.line.ToString(), 
                            TokenType.tyInt), 
                        this, 
                        ASTOp.DeclSInt, 
                        null, 
                        build.rootContext.GetType("int"), 
                        false,
                        AST.DataManifest.ValueConst,
                        0);
                }

                ValueRef localVR = this.GetLocalVariable(node.root.fragment);
                if(localVR != null)
                    return new AST(node.root, this, ASTOp.GetLocalVar, localVR, localVR.varType, true, AST.DataManifest.Procedural, localVR.pointerDepth);

                SynVarValue svv = function.GetVar(node.root.fragment);
                if(svv != null)
                {
                    if(svv.varLoc == SynVarValue.VarLocation.Member)
                    {
                        AST astSrc = new AST(node.root, this, ASTOp.GetThis, function.GetStructScope(), function.GetStructScope(), false, AST.DataManifest.Procedural, 0);
                        AST astDeref = new AST(node.root, this, ASTOp.DerefName, null, svv.type, false, AST.DataManifest.Procedural, 0);
                        return new AST(node.root, this, ASTOp.GetMemberVar, svv, svv.type, true, AST.DataManifest.Procedural, 0, astSrc, astDeref);
                    }
                    else if(svv.varLoc == SynVarValue.VarLocation.Parameter)
                    {
                        return new AST(node.root, this, ASTOp.GetParamVar, svv, svv.type, true, AST.DataManifest.Procedural, 0);
                    }
                    else if(svv.varLoc == SynVarValue.VarLocation.Local)
                    {
                        return new AST(node.root, this, ASTOp.GetLocalVar, svv, svv.type, true, AST.DataManifest.Procedural, 0);
                    }
                    else if(svv.varLoc == SynVarValue.VarLocation.Static)
                    {
                        return new AST(node.root, this, ASTOp.GetGlobalVar, svv, svv.type, true, AST.DataManifest.Procedural, 0);
                    }
                    //TokenAST astDeref =
                    //new TokenAST(
                    //    node.nodes[1].root,
                    //    this,
                    //    TokenASTType.DerefName,
                    //    svv,
                    //    svv.type,
                    //    false,
                    //    TokenAST.DataManifest.NoData);
                    //
                    //TokenAST astDeref =
                    //    new TokenAST(
                    //        node.nodes[1].root,
                    //        this,
                    //        TokenASTType.DerefName,
                    //        svv,
                    //        svv.type,
                    //        false,
                    //        TokenAST.DataManifest.NoData);
                    //
                    //if(svv.
                    // TODO: Check if we're in a function context.
                    
                }

                SynCanidateFuncs canFns = function.GetCanidateFunctions(node.root.fragment);
                if(canFns == null || canFns.functions.Count > 0)
                    return new AST(node.root, this, ASTOp.GetFunction, canFns, null, false, AST.DataManifest.NoData, 0);

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

                AST pred = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(node.nodes[0].root, "If statement predicate did not evaluate to a bool.");

                AST astIf = new AST(node.root, this, ASTOp.IfStatement, null, null, false, AST.DataManifest.NoData, 0, pred);

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

                AST pred = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(node.nodes[0].root, "While statement predicate did not evaludate to a bool.");

                AST astWhile = new AST(node.root, this, ASTOp.WhileStatement, null, null, false, AST.DataManifest.NoData, 0, pred);

                for(int i = 1; i < node.nodes.Count; ++i)
                    astWhile.branches.Add(ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[i]));

                return astWhile;
            }

            if (node.keyword == "dowhile")
            {
                if(node.nodes.Count < 2)
                    throw new SynthExceptionImpossible("dowhile statement needs at least two trees, one for the body statements and one for the continue predicate.");

                TokenTree ttLast = node.nodes[node.nodes.Count - 1];
                AST pred = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, ttLast);
                if(pred.evaluatingType == null || pred.evaluatingType.typeName != "bool")
                    throw new SynthExceptionSyntax(ttLast.root, "dowhile statement predicate did not evaluate to a bool.");

                AST astDoWhile = new AST(node.root, this, ASTOp.DoWhileStatement, null, null, false, AST.DataManifest.NoData, 0);

                for(int i = 0; i < node.nodes.Count - 1; ++i)
                    astDoWhile.branches.Add(ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[i]));

                astDoWhile.branches.Add(pred);
                return astDoWhile;
            }

            if (node.root.MatchesSymbol("=") == true)
            {
                if(node.nodes.Count != 2)
                    throw new SynthExceptionImpossible("Equal needs exactly two trees to evaluate.");

                AST left = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                if(left.hasAddress == false)
                    throw new SynthExceptionSyntax(node.nodes[0].root, "Left side of equation not addressable.");

                AST right = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[1]);
                EnsureIntrinsicCompatibility(ref left, ref right, false);

                AST astEq = 
                    new AST(
                        node.root, 
                        this, 
                        ASTOp.SetValue, 
                        null, 
                        left.evaluatingType, 
                        false, 
                        AST.DataManifest.NoData, 
                        0,
                        left, 
                        right);

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

                    List<AST> astParams = new List<AST>();
                    foreach(TokenTree tt in node.nodes[0].nodes)
                    {
                        AST astParamVal = this.ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, tt);
                        astParams.Add(astParamVal);
                    }

                    // Get the function
                    // The top IF branch gets a direct function, whichch the lower ELSE can also do but
                    // we need it because of constructors - since they have a distinct signature BUT
                    // also clash with typenames.
                    AST caller = null;
                    if (node.nodes[1].root.Matches(TokenType.tyWord) && node.nodes[1].nodes.Count == 0)
                    {
                        string fragName = node.nodes[1].root.fragment;

                        // Similar (duplicate) of what's found in this.ProcessFunctionExpression - but done
                        // under a different context.
                        SynCanidateFuncs canFns = function.GetCanidateFunctions(fragName);
                        if (canFns == null || canFns.functions.Count > 0)
                            caller = new AST(node.root, this, ASTOp.GetFunction, canFns, null, false, AST.DataManifest.NoData, 0);
                        else
                        { 
                            // If it's a type, get a function from the type with the typenames -
                            // which are the constructors.
                            SynType styFrag = function.GetType(fragName);
                            if(styFrag != null)
                            { 
                                SynCanidateFuncs consFns = styFrag.GetCanidateFunctions(fragName);
                                if(consFns == null || consFns.functions.Count > 0)
                                    caller = new AST(node.root, this, ASTOp.Construct, consFns, styFrag, false, AST.DataManifest.NoData, 0);
                            }
                        }
                    }
                    else
                    {
                        // Default params not supported for now
                        caller = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[1]);
                    }

                    SynCanidateFuncs cfns = caller.synthObj.CastCanidateFunctions();
                    if(cfns == null)
                        throw new SynthExceptionSyntax(node.nodes[1].root, "Could not evaluate function");

                    // We received a list of possible function overloads, now we find the appropriate one.
                    // For now we're going to be super-naive and just go off of parameter count.
                    List<SynFuncDecl> matchingFns = new List<SynFuncDecl>();
                    foreach(SynFuncDecl sfn in cfns.functions)
                    { 
                        if(sfn.GetInputParameters() != paramNum)
                            continue;

                        matchingFns.Add(sfn);
                    }

                    if (matchingFns.Count != 1)
                        throw new SynthExceptionSyntax(node.nodes[1].root, $"Could not find a proper overloaded function.");

                    // The function that has been resolved as the one for use.
                    SynFuncDecl reslvFn = matchingFns[0];

                    int startParamIdx = reslvFn.isStatic ? 0 : 1;
                    for (int i = startParamIdx, j = 0; i < reslvFn.parameterSet.Count; ++i, ++j)
                    { 
                        AST paramAST = astParams[j];
                        if(paramAST.evaluatingType == null)
                            throw new SynthExceptionSyntax(paramAST.token, $"Parameter {i} does not evaluate to any type.");
                        else if (paramAST.evaluatingType.intrinsic == reslvFn.parameterSet.Get(i).type.intrinsic)
                        {
                            // Throws if they do not match and there isn't an appropriate cast.
                            //
                            // NOTE: Not really sure what the manifest should be for the left here (for function
                            // parameters).
                            EnsureIntrinsicCompatibility(reslvFn.parameterSet.Get(i).type, ref paramAST, true);
                        }
                        else if(paramAST.evaluatingType != reslvFn.parameterSet.Get(i).type)
                            throw new SynthExceptionSyntax(paramAST.token, $"Type mismatch for parameter {i}: expected type {reslvFn.parameterSet.Get(i).type.typeName} but got {paramAST.evaluatingType.typeName}.");
                        // TODO: Not supported right now, but if they're not both intrinsic, classes may
                        // have conversion operators.

                        caller.branches.Add(paramAST);
                    }

                    caller.synthObj = reslvFn;

                    if(reslvFn.isStatic == true)
                        caller.astType = ASTOp.CallGlobalFn; 
                    else if(reslvFn.isConstructor == true)
                        caller.astType = ASTOp.Construct;
                    else
                    { 
                        // If it's not calling a global function or constructor,
                        // then it's calling a member function. We'll validate the
                        // local struct context later when we build the WASM.
                        caller.astType = ASTOp.CallMember;
                    }

                    caller.evaluatingType = reslvFn.returnType;

                    if(caller.evaluatingType == null)
                        caller.manifest = AST.DataManifest.NoData;
                    else
                        caller.manifest = AST.DataManifest.Procedural;

                    return caller;
                }
            }

            if (node.keyword == "index")
            {
                if (node.root.Matches(TokenType.tySymbol, "[") == false)
                    throw new SynthExceptionImpossible(""); // TODO:

                AST astKey = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                AST astSrc = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[1]);
                EnsureTypeIsBitCompatible(astSrc.evaluatingType, node.nodes[1].root);

                return new AST(node.root, this, ASTOp.Index, null, null, true, AST.DataManifest.Procedural, 0, astSrc, astKey);
            }

            if (node.root.MatchesSymbol("+") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);

                return new AST(node.root, this, ASTOp.Add, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if (node.root.MatchesSymbol("-") == true)
            {
                if(node.nodes.Count == 1)
                {
                    AST astExpr = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);

                    switch(astExpr.astType)
                    {
                        case ASTOp.DeclFloat:
                            {
                                float f = float.Parse(astExpr.token.fragment);
                                astExpr.token.fragment = (-f).ToString();
                                return astExpr;
                            }

                        case ASTOp.DeclFloat64:
                            {
                                double d = double.Parse(astExpr.token.fragment);
                                astExpr.token.fragment = (-d).ToString();
                                return astExpr;
                            }

                        case ASTOp.DeclSInt:
                        case ASTOp.DeclSInt64:
                            { 
                                long l = long.Parse(astExpr.token.fragment);
                                astExpr.token.fragment = (-l).ToString();
                                return astExpr;
                            }
                    }

                    return new AST(
                        node.root, 
                        this, 
                        ASTOp.Negate, 
                        null, 
                        astExpr.evaluatingType, 
                        false, 
                        astExpr.manifest, 
                        0,
                        astExpr);
                }
                else
                {
                    AST left, right;
                    EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                    return new AST(node.root, this, ASTOp.Sub, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
                }
            }

            // The deref handler code is going to be handled closer to where "@" is handled.
            if (node.root.MatchesSymbol("*") == true && string.IsNullOrEmpty(node.keyword) == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                return new AST(node.root, this, ASTOp.Mul, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if (node.root.MatchesSymbol("/") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                return new AST(node.root, this, ASTOp.Div, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if (node.root.MatchesSymbol("%") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                return new AST(node.root, this, ASTOp.Mod, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if (node.root.MatchesSymbol("&") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new AST(node.root, this, ASTOp.BitAnd, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if (node.root.MatchesSymbol("|") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new AST(node.root, this, ASTOp.BitOr, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if (node.root.MatchesSymbol("^") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new AST(node.root, this, ASTOp.BitXor, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if( node.root.MatchesSymbol(">>") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new AST(node.root, this, ASTOp.BitShiftR, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if(node.root.MatchesSymbol("<<") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, true, false);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new AST(node.root, this, ASTOp.BitShiftL, null, left.evaluatingType, false, AST.CombineManifests(left, right), 0, left, right);
            }

            if (node.root.MatchesSymbol("~") == true)
            {
                if(node.nodes.Count != 1)
                    throw new SynthExceptionSyntax(node.root, "~ operation expecting exactly 1 tree."); //Not a great error message

                AST evVal = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                EnsureTypeIsBitCompatible(evVal.evaluatingType, node.nodes[0].root);

                AST.DataManifest dm = evVal.manifest;
                if(dm != AST.DataManifest.ValueConst)
                    dm = AST.DataManifest.Procedural;

                return new AST(node.root, this, ASTOp.BitInv, null, evVal.evaluatingType, false, dm, 0, evVal);
            }

            if (node.root.MatchesSymbol("+=") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                return new AST(node.root, this, ASTOp.SetAfterAdd, null, null, false, AST.DataManifest.NoData, 0, left, right);
            }

            if (node.root.MatchesSymbol("-=") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                return new AST(node.root, this, ASTOp.SetAfterSub, null, null, false, AST.DataManifest.NoData, 0, left, right);
            }

            if (node.root.MatchesSymbol("*=") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                return new AST(node.root, this, ASTOp.SetAfterMul, null, null, false, AST.DataManifest.NoData, 0, left, right);
            }

            if (node.root.MatchesSymbol("/=") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                return new AST(node.root, this, ASTOp.SetAfterDiv, null, null, false, AST.DataManifest.NoData, 0, left, right);
            }

            if (node.root.MatchesSymbol("%=") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);

                return new AST(node.root, this, ASTOp.SetAfterMod, null, null, false, AST.DataManifest.NoData, 0, left, right);
            }

            if (node.root.MatchesSymbol("&=") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new AST(node.root, this, ASTOp.SetAfterBitAnd, null, null, false, AST.DataManifest.NoData, 0, left, right);
            }

            if (node.root.MatchesSymbol("|=") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new AST(node.root, this, ASTOp.SetAfterBitOr, null, null, false, AST.DataManifest.NoData, 0, left, right);
            }

            if (node.root.MatchesSymbol("^=") == true)
            {
                AST left, right;
                EnsureLeftAndRightCompatibility(regCtxBuilders, build, invokingContext, function, node, out left, out right, false, true);
                EnsureTypeIsBitCompatible(left.evaluatingType, node.root);
                EnsureTypeIsBitCompatible(right.evaluatingType, node.root);

                return new AST(node.root, this, ASTOp.SetAfterBitXor, null, null, false, AST.DataManifest.NoData, 0, left, right);
            }

            if(node.root.MatchesSymbol("@") == true)
            {
                if(node.nodes.Count != 1)
                    throw new SynthExceptionImpossible("Attempting to load address of AST node with incorrect children expressions.");

                if(node.nodes[0].root.Matches("@") == true)
                    throw new SynthExceptionSyntax(node.nodes[0].root, "Cannot obtain an address of an address; addresses are non-addressable values.");


                AST inner = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);

                // TODO: Negate things that are dereferenced and addressed execcesively.
                return new AST(node.root, this, ASTOp.LoadPtrAddr, null, inner.evaluatingType, false, AST.DataManifest.Procedural, 0, inner);
            }

            if(node.root.MatchesSymbol("*") == true && node.keyword == "deref")
            { 
                if(node.nodes.Count != 1)
                    throw new SynthExceptionImpossible("Attempting to dereference address of AST node with incorrect children expressions.");

                AST inner = ProcessFunctionExpression(regCtxBuilders, build, invokingContext, function, node.nodes[0]);
                if(inner.evaluatingType == null)
                    throw new SynthExceptionSyntax(node.root, "Attempting to derefence unknown value type.");

                if(inner.ptrDepth == 0)
                    throw new SynthExceptionSyntax(node.root, "Attempting to derefence a non-pointer");

                // TODO: Negate things that are dereferenced and addressed execcesively.
                return new AST(node.root, this, ASTOp.DerefPtrAddr, null, inner.evaluatingType, false, AST.DataManifest.Procedural, 0, inner);
            }

            return null;
        }

        public void EnsureLeftAndRightCompatibility(
            List<SynNestingBuilder> regCtxBuilders, 
            WASMBuild build, 
            SynScope invokingContext, 
            SynFuncDecl fnDecl, 
            TokenTree tt, 
            out AST left, 
            out AST right, 
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


            if (leftRequireAddr == true && left.hasAddress == false)
                throw new SynthExceptionSyntax(tt.root, $"Error parsing {tt.root.fragment}, left side expected to be addressable.");

            // This doesn't handle intrinsics, although more probably needs to be decided about the language.
            if(left.evaluatingType == right.evaluatingType)
                return;

            SynType castedTy = GetSignificantIntrinsicType(left.evaluatingType, right.evaluatingType, left.token);

            EnsureIntrinsicCompatibility(ref left, ref right, allowCastingLeft);

            // Check again, in case the left side required a cast to make the expression
            // compatible.
            if (leftRequireAddr == true && left.hasAddress == false)
                throw new SynthExceptionSyntax(tt.root, $"Error parsing {tt.root.fragment}, left side expected to be addressable.");
        }

        public static void EnsureTypeIsInt(SynType sty, Token tErrSrc)
        { 
            // They happen to be the same
            // TODO: figure out best way to throw correct error message
            EnsureTypeIsBitCompatible(sty, tErrSrc);
        }

        public static void EnsureTypeIsBitCompatible(SynType sty, Token tErrSrc)
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

        public static SynType GetSignificantIntrinsicType(SynType left, SynType right, Token tok)
        { 
            if(left.intrinsic == false || right.intrinsic == false)
                throw new SynthExceptionImpossible("Attempting to get significant intrinsic type with a non-intrinsic.");

            if(left == right)
                return left;

            int leftGrade = _GetIntrinsicSignificanceLevel(left.typeName);
            int rightGrade = _GetIntrinsicSignificanceLevel(right.typeName);

            if(leftGrade > rightGrade)
                return left;

            if(rightGrade > leftGrade)
                return right;

            // TODO: There's actually more we have to do in regards to error checking.
            // The biggest issue is with implicitly casting a signed int type to an 
            // unsigned int type, even if it's a larger bit width.
            // 
            // TODO: If we're mixing integers of the same bitwidth and different sign,
            // we can actually keep the expressions compatible if we force an implicit
            // cast to a signed type of a larger bit width. This falls apart though
            // when dealing with uin64 vs int64.

            throw new SynthExceptionSyntax(tok, $"Cannot perform implicity cast between {left.typeName} and {right.typeName}. An explicit cast is required.");
        }

        public static SignMode _GetIntrinsicSignMode(string type)
        { 
            switch(type)
            { 
                case "bool":
                case "float":
                case "float64":
                    return SignMode.Agnostic;

                case "int8":
                case "int16":
                case "int":
                case "int64":
                    return SignMode.Signed;

                case "uint8":
                case "uint16":
                case "uint":
                case "uint64":
                    return SignMode.Unsigned;
            }
            throw new SynthExceptionImpossible($"Attempting to get sign mode of unknown intrinsic type {type}.");
        }

        private static int _GetIntrinsicSignificanceLevel(string type)
        { 
            switch(type)
            { 
                case "bool":
                    return 0;
                case "uint8":
                case "int8":
                    return 1;
                case "uint16":
                case "int16":
                    return 2;
                case "uint":
                case "int":
                    return 3;
                case "uint64":
                case "int64":
                    return 4;
                case "float":
                    return 5;
                case "float64":
                    return 6;
            }
            throw new SynthExceptionImpossible($"Attempting to grade significance level of unknown intrinsic type {type}.");
        }

        /// <summary>
        /// Check to make sure that an AST variable of a certain intrinsic datatype is compatible
        /// with another data type.
        /// 
        /// If they are not the same, attempt to cast value to the correct type.
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="canCastLeft"></param>
        public void EnsureIntrinsicCompatibility(ref AST left, ref AST right, bool canCastLeft)
        {
            // If they're the same, no conversion needed.
            if(left.evaluatingType == right.evaluatingType)
                return;

            // Impossible throws will throw no matter what.
            if (left.evaluatingType.intrinsic == false || right.evaluatingType.intrinsic == false)
                    throw new SynthExceptionImpossible("Checking instrinsic compatibility of non-intrinsic character.");

            SynType stySig = GetSignificantIntrinsicType(left.evaluatingType, right.evaluatingType, left.token);

            if(left.evaluatingType != stySig)
            {
                // If we can't perform an implicit cast on the left, perform one on the right,
                // and we'll evaluate if it's a valid cast a little later. Some types of downgrading
                // casts are allowed, mainly integer downgrades if we're downgrading a value that's
                // declared as a const in the SynthSyntax script.
                if (canCastLeft == false)
                {
                    AST cast = new AST(right.token, this, ASTOp.ImplicitCast, stySig, stySig, false, AST.CastManifest(right.manifest), 0, right);
                    right = cast;
                    return;
                }
                else
                {
                    AST cast = new AST(left.token, this, ASTOp.ImplicitCast, stySig, stySig, false, AST.CastManifest(left.manifest), 0, left);
                    left = cast;
                }
            }

            if(right.evaluatingType != stySig)
            {
                AST cast = new AST( right.token, this, ASTOp.ImplicitCast, stySig, stySig, false, AST.CastManifest(right.manifest), 0, right);
                right = cast;
            }
        }

        public void EnsureIntrinsicCompatibility(SynType leftTy, ref AST right, bool allowReverseForce)
        { 
            if(right.evaluatingType == leftTy)
                return;

            if (leftTy.intrinsic == false || right.evaluatingType.intrinsic == false)
                throw new SynthExceptionImpossible("Checking instrinsic compatibility of non-intrinsic character.");

            SynType stySig = GetSignificantIntrinsicType(leftTy, right.evaluatingType, right.token);
            if(stySig != leftTy)
            {
                if(allowReverseForce == false)
                    throw new SynthExceptionSyntax(right.token, "Cannot maintain type compatability without explicitly casting the right value.");
            }

            if (right.evaluatingType != leftTy)
            {
                AST cast = new AST(right.token, this, ASTOp.ImplicitCast, leftTy, leftTy, false, AST.CastManifest(right.manifest), 0, right);
                right = cast;
            }
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

        public static AST GetTypeNot(string typename, AST left, AST right)
        {
            if (left.evaluatingType.typeName == typename)
                return right;

            return left;
        }

        

        public void ProcessDefaultVarTokens(SynVarValue fnParam, List<TokenTree> trees)
        { 
        }

        public void BuildBSFunction( SynFuncDecl fnd, AST ast, WASMBuild wasmBuild, SynNestingBuilder ctxBuilder, WASMByteBuilder fnBuild)
        { 

            foreach(AST n in ast.branches)
            { 
                this.BuildBSFunctionExpression(fnd, n, wasmBuild, ctxBuilder, fnBuild);
            }
        }

        static HashSet<string> match32 = new HashSet<string> { "int8", "uint8", "int16", "uint16", "int", "uint" };
        static HashSet<string> match64 = new HashSet<string> { "int64", "uint64" };
        public ValueRef BuildBSFunctionExpression(SynFuncDecl fnd, AST expr, WASMBuild wasmBuild, SynNestingBuilder ctxBuilder, WASMByteBuilder fnBuild)
        { 
            switch(expr.astType)
            {
                case ASTOp.SetValue:
                    { 
                        if(expr.branches.Count != 2)
                            throw new SynthExceptionCompile("Setting a value expected two parameters, a target and a destination.");

                        ValueRef vrLeft = this.BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnBuild);

                        if (vrLeft.valLoc == ValueLoc.LocalIdx)
                        { 
                            ValueRef vrRight = this.BuildBSFunctionExpression(fnd, expr.branches[1], wasmBuild, ctxBuilder, fnBuild);
                            vrRight.PutInstrinsicValueOnStack(fnBuild); // Currently only handling intrinsic values

                            fnBuild.Add_LocalSet((uint)vrLeft.idx);
                            return new ValueRef(ValueLoc.NoValue, -1, -1, vrLeft.varType);
                        }
                        else if(vrLeft.valLoc == ValueLoc.PointerOnStack)
                        {
                            ValueRef vrRight = this.BuildBSFunctionExpression(fnd, expr.branches[1], wasmBuild, ctxBuilder, fnBuild);
                            vrRight.PutInstrinsicValueOnStack(fnBuild); // Currently only handling intrinsic values

                            // Evaluating the left puts the pointer on the stack
                            // Then we put the value on the stack
                            // and that's the format all the WASM *.store* expect, so we just need
                            // to add the correct store instruction afterwards.

                            switch(vrRight.varType.typeName)
                            {
                            case "bool":
                            case "int8":
                            case "uint8":
                                fnBuild.Add_I32Store8();
                                break;

                            case "int16":
                            case "uint16":
                                fnBuild.Add_I32Store16();
                                break;

                            case "int":
                            case "uint":
                                fnBuild.Add_I32Store();
                                break;

                            case "int64":
                            case "uint64":
                                fnBuild.Add_I64Store();
                                break;

                            case "float":
                                fnBuild.Add_F32Store();
                                break;

                            case "float64":
                                fnBuild.Add_F64Store();
                                break;

                            default:
                                throw new SynthExceptionImpossible("Attempting to set member of unknown type.");
                            }

                            return new ValueRef(ValueLoc.NoValue, -1, -1, vrLeft.varType);
                        }
                        else
                            throw new SynthExceptionCompile($"Setting non-local variables at {vrLeft.valLoc} currently not supported.");
                    }

                case ASTOp.GetGlobalVar:
                    break;

                case ASTOp.GetMemberVar:
                    { 
                        if(expr.branches.Count != 2)
                            throw new SynthExceptionImpossible("Getting member with an unexpected number of branches.");

                        // Get the base object (pointer to it)
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnBuild);

                        if(expr.branches[1].token.Matches(TokenType.tyWord) == false)
                            throw new SynthExceptionSyntax(expr.branches[0].token, "Member name has unexpected syntax.");

                        // Get the member variable (pointer to it)
                        if(vr.valLoc != ValueLoc.ValueOnMemStack)
                        { 
                            SynVarValue svv = vr.varType.GetVar(expr.branches[1].token.fragment, false);
                            if(svv == null)
                                throw new SynthExceptionSyntax(expr.branches[0].token, "Dereferenced member doesn't exist.");

                            if(svv.alignmentOffset != 0)
                            {
                                fnBuild.Add_I32Const((uint)svv.alignmentOffset);
                                fnBuild.AddInstr(WASM.Instruction.i32_add);
                            }
                            return new ValueRef(ValueLoc.PointerOnStack, -1, -1, svv.type, 1);
                        }

                        throw new SynthExceptionSyntax(expr.token, "Member is not dereferenceable.");
                    }

                case ASTOp.GetLocalVar:
                    { 
                        ValueRef vr = this.GetLocalVariable(expr.token.fragment);

                        if(vr == null)
                            throw new SynthExceptionImpossible("Expected local variable wasn't found.");
                        
                        if(vr.valLoc == ValueLoc.ValueOnMemStack)
                            return vr.PutLocalVarAddressOnStack(fnBuild);

                        if (vr.varType.intrinsic == false)
                            throw new SynthExceptionCompile("Non intrinsic local variables not supported yet.");

                        if (vr.valLoc != ValueLoc.LocalIdx)
                            throw new SynthExceptionCompile("Mem stack and heap variables not supported yet.");

                        //fnBuild.Add_LocalGet((uint)vr.idx);
                        return vr;
                    }

                case ASTOp.GetParamVar:
                    { 
                        SynVarValue svvParam = fnd.GetParameter(expr.token.fragment);
                        if(svvParam == null)
                            throw new SynthExceptionImpossible("Could not find expected parameter.");

                        ValueRef vr = fnd.parameterSet.GetRef(svvParam);
                        if(vr == null)
                            throw new SynthExceptionImpossible("Could not resolve reference to parameter.");

                        if(vr.valLoc != ValueLoc.LocalIdx)
                            throw new SynthExceptionCompile($"Unable to get parameter from {vr.valLoc}.");

                        vr.PutInstrinsicValueOnStack(fnBuild);
                        return new ValueRef(ValueLoc.ValueOnStack, -1, -1, vr.varType);
                    }

                case ASTOp.LoadPtrAddr:
                    {
                        if(expr.branches.Count != 1)
                            throw new SynthExceptionCompile("LoadPtrAddr expected 1 branch to process.");

                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnBuild);
                        vr.PutLocalVarAddressOnStack(fnBuild);

                        return new ValueRef(ValueLoc.PointerOnStack, -1, -1, vr.varType, vr.pointerDepth + 1);
                    }

                case ASTOp.DerefPtrAddr:
                    { 
                        if(expr.branches.Count != 1)
                            throw new SynthExceptionCompile("DerefPtrAddr expected 1 branch to process.");

                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnBuild);

                        int depth = vr.pointerDepth - 1;
                        if(depth == 0)
                        { 
                            if(vr.varType.intrinsic == true)
                            { 
                                vr.PutInstrinsicValueOnStack(fnBuild);
                                return new ValueRef(ValueLoc.ValueOnStack, -1, -1, vr.varType, 0);
                            }
                        }
                        vr.PutLocalVarAddressOnStack(fnBuild);
                        return new ValueRef(ValueLoc.PointerOnStack, -1, -1, vr.varType, depth);
                    }

                case ASTOp.GetFunction:
                    break;

                case ASTOp.GetRegion:
                    break;

                case ASTOp.GetThis:
                    {
                        SynType styThis = fnd.GetStructScope();
                        if(styThis == null)
                            throw new SynthExceptionSyntax(expr.token, "Illegal scope for use of 'this' keyword.");

                        if(fnd.isStatic == true)
                            throw new SynthExceptionSyntax(expr.token, "Cannot get 'this' in global function.");

                        if(fnd.parameterSet.HasThis() == false)
                            throw new SynthExceptionImpossible("Keyword 'this' unavailable.");

                        fnBuild.Add_LocalGet(0);

                        return new ValueRef(ValueLoc.PointerOnStack, -1, -1, styThis, 1);
                    }

                case ASTOp.FunctionDecl:
                    break;

                case ASTOp.SetAfterAdd:
                    break;

                case ASTOp.SetAfterSub:
                    break;

                case ASTOp.SetAfterMul:
                    break;

                case ASTOp.SetAfterDiv:
                    break;

                case ASTOp.SetAfterMod:
                    break;

                case ASTOp.SetAfterBitOr:
                    break;

                case ASTOp.SetAfterBitAnd:
                    break;

                case ASTOp.SetAfterBitXor:
                    break;

                case ASTOp.SetAfterShiftL:
                    break;

                case ASTOp.SetAfterShiftR:
                    break;

                case ASTOp.Index:
                    break;

                case ASTOp.IfStatement:
                    break;

                case ASTOp.WhileStatement:
                    break;

                case ASTOp.ForStatement:
                    break;

                case ASTOp.DoWhileStatement:
                    break;

                case ASTOp.Add:
                    {
                        AST left = expr.branches[0];
                        AST right = expr.branches[1];

                        // TODO: If these are addresses, get the address
                        ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                        vrLeft.PutInstrinsicValueOnStack(fnBuild);

                        ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                        vrRight.PutInstrinsicValueOnStack(fnBuild);

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
                                fnBuild.AddInstr(WASM.Instruction.i32_add);
                                break;

                            case "int64":
                            case "uint64":
                                fnBuild.AddInstr(WASM.Instruction.i64_add);
                                break;

                            case "float":
                                fnBuild.AddInstr(WASM.Instruction.f32_add);
                                break;

                            case "float64":
                                fnBuild.AddInstr(WASM.Instruction.f64_add);
                                break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown add type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
                    }

                case ASTOp.Sub:
                    {
                        AST left = expr.branches[0];
                        AST right = expr.branches[1];

                        ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                        vrLeft.PutInstrinsicValueOnStack(fnBuild);

                        ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                        vrRight.PutInstrinsicValueOnStack(fnBuild);

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
                                fnBuild.AddInstr(WASM.Instruction.i32_sub);
                                break;

                            case "int64":
                            case "uint64":
                                fnBuild.AddInstr(WASM.Instruction.i64_sub);
                                break;

                            case "float":
                                fnBuild.AddInstr(WASM.Instruction.f32_sub);
                                break;

                            case "float64":
                                fnBuild.AddInstr(WASM.Instruction.f64_sub);
                                break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown sub type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
                    }

                case ASTOp.Mul:
                    {
                        AST left = expr.branches[0];
                        AST right = expr.branches[1];

                        ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                        vrLeft.PutInstrinsicValueOnStack(fnBuild);

                        ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                        vrRight.PutInstrinsicValueOnStack(fnBuild);

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
                                fnBuild.AddInstr(WASM.Instruction.i32_mul);
                                break;

                            case "int64":
                            case "uint64":
                                fnBuild.AddInstr(WASM.Instruction.i64_mul);
                                break;

                            case "float":
                                fnBuild.AddInstr(WASM.Instruction.f32_mul);
                                break;

                            case "float64":
                                fnBuild.AddInstr(WASM.Instruction.f64_mul);
                                break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown mul type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
                    }

                case ASTOp.Div:
                    {
                        AST left = expr.branches[0];
                        AST right = expr.branches[1];

                        ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                        vrLeft.PutInstrinsicValueOnStack(fnBuild);

                        ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                        vrRight.PutInstrinsicValueOnStack(fnBuild);

                        ctxBuilder.PopType(left.evaluatingType);
                        ctxBuilder.PopType(right.evaluatingType);
                        switch (left.evaluatingType.typeName)
                        {
                            case "int8":
                            case "int":
                            case "int16":
                                fnBuild.AddInstr(WASM.Instruction.i32_div_s);
                                break;

                            case "uint8":
                            case "uint16":
                            case "uint":
                                fnBuild.AddInstr(WASM.Instruction.i32_div_u);
                                break;

                            case "int64":
                                fnBuild.AddInstr(WASM.Instruction.i64_div_s);
                                break;

                            case "uint64":
                                fnBuild.AddInstr(WASM.Instruction.i64_div_u);
                                break;

                            case "float":
                                fnBuild.AddInstr(WASM.Instruction.f32_div);
                                break;

                            case "float64":
                                fnBuild.AddInstr(WASM.Instruction.f64_div);
                                break;

                            default:
                                throw new SynthExceptionImpossible($"AST Unknown div type {left.evaluatingType.typeName}");
                        }
                        ctxBuilder.PushType(left.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, left.evaluatingType);
                    }

                case ASTOp.Mod:
                    {
                        AST left = expr.branches[0];
                        AST right = expr.branches[1];

                        ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                        vrLeft.PutInstrinsicValueOnStack(fnBuild);

                        ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                        vrRight.PutInstrinsicValueOnStack(fnBuild);

                        ctxBuilder.PopType(left.evaluatingType);
                        ctxBuilder.PopType(right.evaluatingType);
                        switch (left.evaluatingType.typeName)
                        {
                            case "int8":
                            case "int":
                            case "int16":
                                fnBuild.AddInstr(WASM.Instruction.i32_rem_s);
                                break;

                            case "uint8":
                            case "uint16":
                            case "uint":
                                fnBuild.AddInstr(WASM.Instruction.i32_rem_u);
                                break;

                            case "int64":
                                fnBuild.AddInstr(WASM.Instruction.i64_rem_s);
                                break;

                            case "uint64":
                                fnBuild.AddInstr(WASM.Instruction.i64_rem_u);
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

                case ASTOp.BitOr:
                case ASTOp.BitAnd:
                case ASTOp.BitXor:
                    {
                        AST left = expr.branches[0];
                        AST right = expr.branches[1];

                        WASM.Instruction instr32;
                        WASM.Instruction instr64;
                        switch(expr.astType)
                        { 
                            case ASTOp.BitOr:
                                instr32 = WASM.Instruction.i32_or;
                                instr64 = WASM.Instruction.i64_or;
                                break;

                            default:
                            case ASTOp.BitAnd:
                                instr32 = WASM.Instruction.i32_and;
                                instr64 = WASM.Instruction.i64_and;
                                break;

                            case ASTOp.BitXor:
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
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                            vrLeft.PutInstrinsicValueOnStack(fnBuild);

                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                            vrRight.PutInstrinsicValueOnStack(fnBuild);

                            fnBuild.AddInstr(instr32);
                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int"));
                        }
                        else if(
                            match64.Contains(left.evaluatingType.typeName) == true &&
                            match64.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                            vrLeft.PutInstrinsicValueOnStack(fnBuild);

                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                            vrRight.PutInstrinsicValueOnStack(fnBuild);

                            fnBuild.AddInstr(instr64);
                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int64"));
                        }
                        else
                        {
                            throw new SynthExceptionSyntax(
                                expr.token, 
                                $"Cannot combine types {left.evaluatingType} and {right.evaluatingType} for a bit or operation.");
                        }
                    }

                case ASTOp.BitInv:
                    {
                        if(expr.branches.Count != 1)
                            throw new SynthExceptionImpossible("Bit inv has an unexpected node count.");

                        AST astVal = expr.branches[0];

                        // https://github.com/WebAssembly/design/issues/701
                        if (match32.Contains(astVal.evaluatingType.typeName) == true)
                        {
                            ValueRef vrVal = BuildBSFunctionExpression(fnd, astVal, wasmBuild, ctxBuilder, fnBuild);
                            vrVal.PutInstrinsicValueOnStack(fnBuild);

                            fnBuild.Add_I32Const(-1);
                            fnBuild.AddInstr(WASM.Instruction.i32_xor);
                        }
                        else if(match64.Contains(astVal.evaluatingType.typeName) == true)
                        {
                            ValueRef vrVal = BuildBSFunctionExpression(fnd, astVal, wasmBuild, ctxBuilder, fnBuild);
                            vrVal.PutInstrinsicValueOnStack(fnBuild);

                            fnBuild.Add_I64Const(-1);
                            fnBuild.AddInstr(WASM.Instruction.i64_xor);
                        }
                        else
                        {
                            throw new SynthExceptionSyntax(
                                expr.token, 
                                $"Cannot invert bits of type {astVal.evaluatingType.typeName}.");
                        }

                        return new ValueRef(ValueLoc.ValueOnStack, -1, -1, astVal.evaluatingType);
                    }

                case ASTOp.BitShiftL:
                    {
                        AST left = expr.branches[0];
                        AST right = expr.branches[1];

                        // It's uncertain how the autocasting should work
                        if (
                            match32.Contains(left.evaluatingType.typeName) == true &&
                            match32.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                            vrLeft.PutInstrinsicValueOnStack(fnBuild);

                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                            vrRight.PutInstrinsicValueOnStack(fnBuild);

                            fnBuild.AddInstr(WASM.Instruction.i32_shl);
                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int"));
                        }
                        else if (
                            match64.Contains(left.evaluatingType.typeName) == true &&
                            match64.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                            vrLeft.PutInstrinsicValueOnStack(fnBuild);

                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                            vrRight.PutInstrinsicValueOnStack(fnBuild);

                            fnBuild.AddInstr(WASM.Instruction.i64_shl);
                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int64"));
                        }
                        else
                        {
                            throw new SynthExceptionSyntax(
                                expr.token,
                                $"Cannot combine types {left.evaluatingType} and {right.evaluatingType} for a bit shift operation.");
                        }
                    }

                case ASTOp.BitShiftR:
                    {
                        AST left = expr.branches[0];
                        AST right = expr.branches[1];

                        HashSet<string> match32 = new HashSet<string> { "int8", "uint8", "int16", "uint16", "int", "uint" };
                        HashSet<string> match64 = new HashSet<string> { "int64", "uint64" };

                        // It's uncertain how the autocasting should work
                        if (
                            match32.Contains(left.evaluatingType.typeName) == true &&
                            match32.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                            vrLeft.PutInstrinsicValueOnStack(fnBuild);

                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                            vrRight.PutInstrinsicValueOnStack(fnBuild);

                            // TODO: More work needs to be done to figure out casting of lower unsigned values
                            switch(right.evaluatingType.typeName)
                            { 
                                case "int8":
                                case "int16":
                                case "int":
                                    fnBuild.AddInstr(WASM.Instruction.i32_shr_s);
                                    break;

                                case "uint8":
                                case "uint16":
                                case "uint":
                                    fnBuild.AddInstr(WASM.Instruction.i32_shr_u);
                                    break;
                            }

                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int"));
                        }
                        else if (
                            match64.Contains(left.evaluatingType.typeName) == true &&
                            match64.Contains(right.evaluatingType.typeName) == true)
                        {
                            ValueRef vrLeft = BuildBSFunctionExpression(fnd, left, wasmBuild, ctxBuilder, fnBuild);
                            vrLeft.PutInstrinsicValueOnStack(fnBuild);

                            ValueRef vrRight = BuildBSFunctionExpression(fnd, right, wasmBuild, ctxBuilder, fnBuild);
                            vrRight.PutInstrinsicValueOnStack(fnBuild);

                            if(right.evaluatingType.typeName == "int64")
                                fnBuild.AddInstr(WASM.Instruction.i64_shr_s);
                            else if(right.evaluatingType.typeName == "uint64")
                                fnBuild.AddInstr(WASM.Instruction.i64_shr_u);

                            return new ValueRef(ValueLoc.ValueOnStack, -1, -1, fnd.GetType("int64"));
                        }
                        else
                        {
                            throw new SynthExceptionSyntax(
                                expr.token,
                                $"Cannot combine types {left.evaluatingType} and {right.evaluatingType} for a bit shift operation.");
                        }
                    }

                case ASTOp.Unprocessed: 
                    break;

                case ASTOp.RegisterLocalVar:
                    // The language doesn't return any kind of value for registering a local.
                    {
                        // The variable being registered
                        ValueRef vrReg = this.GetLocalVariable(expr.branches[0].token.fragment);

                        // The first branch is the variable name, the second is 
                        // an initialization
                        if (expr.branches.Count > 1) 
                        {
                            if(expr.branches[1].astType == ASTOp.Construct)
                            {
                                if(vrReg.varType.intrinsic == true)
                                    throw new SynthExceptionSyntax(expr.branches[1].token, "Cannot construct intrinsic type.");

                                vrReg.PutLocalVarAddressOnStack(fnBuild);

                                SynFuncDecl sfdConstr = expr.branches[1].synthObj.CastFuncDecl();
                                BuildBSFunctionInvoke(sfdConstr, fnd, expr.branches[1], wasmBuild, ctxBuilder, fnBuild);
                            }
                            else
                            {
                                if(
                                    vrReg.valLoc == ValueLoc.NoValue || 
                                    vrReg.valLoc == ValueLoc.ValueOnStack)
                                {
                                    throw new SynthExceptionImpossible($"Cannot initialize local variable at location {vrReg.valLoc}.");
                                }
                                else if (
                                    vrReg.valLoc == ValueLoc.PointerOnStack ||
                                    vrReg.valLoc == ValueLoc.ValueOnHeap ||
                                    vrReg.valLoc == ValueLoc.ValueOnMemStack )
                                {
                                    // The top item is a pointer                                    
                                    vrReg.PutLocalVarAddressOnStack(fnBuild);

                                    // This will put/calculate the evaluated intrinsic value on the stack.
                                    ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[1], wasmBuild, ctxBuilder, fnBuild);
                                    if (vr.varType.intrinsic == false)
                                        throw new SynthExceptionCompile("Initializing non-intrinsic variables on the stack is currently not supported.");
                                    vr.PutInstrinsicValueOnStack(fnBuild);

                                    vrReg.StoreIntrinsicValue(fnBuild);
                                }
                                else if( vrReg.valLoc == ValueLoc.LocalIdx)
                                {
                                    // This will put/calculate the evaluated intrinsic value on the stack.
                                    ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[1], wasmBuild, ctxBuilder, fnBuild);
                                    if (vr.varType.intrinsic == false)
                                        throw new SynthExceptionCompile("Initializing non-intrinsic variables on the stack is currently not supported.");

                                    vr.PutInstrinsicValueOnStack(fnBuild);

                                    fnBuild.Add_LocalSet((uint)vrReg.idx);
                                }
                                else
                                    throw new SynthExceptionImpossible("Could not set value when initializing local variable.");
                            }
                        }
                        else if(vrReg.varType.intrinsic == false)
                        { 
                            SynFuncDecl sfDefcon = vrReg.varType.GetDefaultConstructor();

                            if(sfDefcon != null)
                            {
                                WASMBuild.FunctionInfo fi = wasmBuild.functionLookup[sfDefcon];

                                fnBuild.Add_I32Const(vrReg.fnByteAlign);
                                fnBuild.Add_I32Load();
                                fnBuild.Add_Call(fi.functionIndex);
                            }
                        }
                        return new ValueRef(ValueLoc.NoValue, 0, 0, vrReg.varType);
                    }

                case ASTOp.DeclBool:
                    {
                        if(expr.token.fragment == "false" || expr.token.fragment == "0")
                            fnBuild.Add_I32Const(0);
                        else
                            fnBuild.Add_I32Const(1);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef( ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case ASTOp.DeclUInt:
                    {
                        // i32.const values are uninterpreted ints, which are
                        // signed
                        uint val = uint.Parse(expr.token.fragment);
                        fnBuild.Add_I32Const((int)val);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case ASTOp.DeclSInt:
                    { 
                        int val = int.Parse(expr.token.fragment);
                        fnBuild.Add_I32Const(val);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case ASTOp.DeclUInt64:
                    { 
                        ulong val = ulong.Parse(expr.token.fragment);
                        fnBuild.Add_I64Const(val);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case ASTOp.DeclSInt64:
                    { 
                        long val = long.Parse(expr.token.fragment);
                        fnBuild.Add_I64Const(val);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case ASTOp.DeclFloat:
                    { 
                        float val = float.Parse(expr.token.fragment);
                        fnBuild.Add_F32Const(val);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case ASTOp.DeclFloat64:
                    {
                        double val = double.Parse(expr.token.fragment);
                        fnBuild.Add_F64Const(val);

                        ctxBuilder.PushType(expr.evaluatingType);
                        return new ValueRef(ValueLoc.ValueOnStack, 0, 0, expr.evaluatingType);
                    }

                case ASTOp.DeclString:
                    { 
                        // TODO:
                    }
                    break;

                case ASTOp.ExplicitCast:
                    {
                        if (expr.branches.Count != 1)
                            throw new SynthExceptionImpossible("Attempting to cast with unexpected number of branches.");

                        // All explicit intrinsic casts should be supported, so no validation should be necessary.

                        SynType styCastTo = expr.evaluatingType;
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnBuild);
                        vr.PutInstrinsicValueOnStack(fnBuild);
                        CastIntrinsicOnStack(vr.varType, styCastTo, fnBuild);
                        return new ValueRef(ValueLoc.ValueOnStack, -1, -1, styCastTo);
                    }

                case ASTOp.ImplicitCast:
                    {
                        if (expr.branches.Count != 1)
                            throw new SynthExceptionImpossible("Attempting to cast with unexpected number of branches.");

                        SynType styCastTo = expr.evaluatingType;
                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnBuild);
                        vr.PutInstrinsicValueOnStack(fnBuild);

                        // TODO: Validate the cast is proper.
                        // Certain casting are not allowed, or only allowed if expr.branches[0].manifest
                        // is a const.

                        CastIntrinsicOnStack(vr.varType, styCastTo, fnBuild);
                        return new ValueRef(ValueLoc.ValueOnStack, -1, -1, styCastTo);
                    }

                case ASTOp.Compare_Eq:
                    break;

                case ASTOp.Compare_NEq:
                    break;

                case ASTOp.Compare_LessThan:
                    break;

                case ASTOp.Compare_LessThanEq:
                    break;

                case ASTOp.Compare_GreaterThan:
                    break;

                case ASTOp.Compare_GreaterThanEq:
                    break;

                case ASTOp.CallMember:
                    { 
                        SynFuncDecl fnInvoke = expr.synthObj.CastFuncDecl();
                        if(fnInvoke == null)
                            throw new SynthExceptionImpossible("Missing function for method AST processing.");

                        if(fnInvoke.isStatic == true)
                            throw new SynthExceptionImpossible("Attempting to call struct method with a method that is a static function.");

                        if(expr.branches.Count == 0)
                            throw new SynthExceptionImpossible("Attemptiong to call a struct method without an invoking object AST node.");

                        // Call member is expected to contain the invoking object as fnBuild[0], and to put the
                        // pointer on stack, so we just need to build the binary for a normal function call.
                        return BuildBSFunctionInvoke(fnInvoke, fnd, expr, wasmBuild, ctxBuilder, fnBuild);
                    }

                case ASTOp.CallGlobalFn:
                    { 
                        if(expr.synthObj == null)
                            throw new SynthExceptionImpossible("Missing function for global AST processing.");

                        SynFuncDecl fnInvoke = expr.synthObj.CastFuncDecl();
                        if(fnInvoke == null)
                            throw new SynthExceptionImpossible("Missing function for global AST processing.");

                        if(fnInvoke.isStatic == false)
                            throw new SynthExceptionImpossible("Attempting to call static function which is not recorded as static.");

                        return BuildBSFunctionInvoke(fnInvoke, fnd, expr, wasmBuild, ctxBuilder, fnBuild);
                    }

                case ASTOp.Negate:
                    { 
                        if(expr.branches.Count != 1)
                            throw new SynthExceptionImpossible("Negate encountered with unexpected AST branches.");

                        ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[0], wasmBuild, ctxBuilder, fnBuild);
                        vr.PutInstrinsicValueOnStack(fnBuild);

                        switch (vr.varType.typeName)
                        { 
                            case "int8":
                            case "uint8":
                            case "int16":
                            case "uin16":
                            case "int":
                            case "uint":
                                fnBuild.Add_I32Const(-1);
                                fnBuild.AddInstr(WASM.Instruction.i32_mul);
                                return new ValueRef(ValueLoc.ValueOnStack, -1, -1, vr.varType);

                            case "int64":
                            case "uint64":
                                fnBuild.Add_I64Const(-1);
                                fnBuild.AddInstr(WASM.Instruction.i64_mul);
                                return new ValueRef(ValueLoc.ValueOnStack, -1, -1, vr.varType);

                            case "float":
                                fnBuild.Add_F32Const(-1.0f);
                                fnBuild.AddInstr(WASM.Instruction.f32_mul);
                                return new ValueRef(ValueLoc.ValueOnStack, -1, -1, vr.varType);

                            case "float64":
                                fnBuild.Add_F64Const(-1.0);
                                fnBuild.AddInstr(WASM.Instruction.f64_mul);
                                return new ValueRef(ValueLoc.ValueOnStack, -1, -1, vr.varType);

                            default:
                                throw new SynthExceptionSyntax(expr.token, "Negating unsupported type.");
                        }
                    }

                case ASTOp.DefaultParam:
                    break;

                case ASTOp.Destruct:
                    break;

                case ASTOp.EndScope:
                    { 
                        SynNestingBuilder ctxNest = expr.synthObj.CastNest();
                        if(ctxNest == null)
                            throw new SynthExceptionImpossible("Attempting to close the scope of a null nest.");

                        for(int i = ctxNest.allLocalVars.Count - 1; i >= 0; --i)
                        { 
                            ValueRef vrToDestr = ctxNest.allLocalVars[i];
                            if(vrToDestr.varType.intrinsic == true)
                                continue;

                            SynFuncDecl fnDestr = vrToDestr.varType.GetDestructor();
                            if(fnDestr == null)
                                continue;

                            vrToDestr.PutLocalVarAddressOnStack(fnBuild);
                            this.BuildBSFunctionDirectInvoke(fnDestr, fnd, wasmBuild, fnBuild);
                        }
                        return new ValueRef(ValueLoc.NoValue, -1, -1, null);
                    }

                case ASTOp.ReturnValue:
                    {
                        if(expr.branches.Count != 1)
                            throw new SynthExceptionImpossible("Return value missing the value node.");

                        ValueRef vrRet = BuildBSFunctionExpression( fnd, expr.branches[0], wasmBuild, ctxBuilder, fnBuild);

                        if(vrRet.valLoc != ValueLoc.ValueOnStack)
                            throw new SynthExceptionCompile($"Cannot perform return location where return value ends up at location {vrRet.valLoc}");

                        if(vrRet.varType == null || vrRet.varType.intrinsic == false)
                            throw new SynthExceptionCompile("Attempting to return non-intrinsic value.");

                        vrRet.PutInstrinsicValueOnStack(fnBuild);
                        return vrRet;
                    }
            }

            throw new SynthExceptionImpossible($"Unhandled AST type {expr.astType}.");
        }

        /// <summary>
        /// When an intrinsic value is on the stack, emit the proper WASM assembly to
        /// convert the value to another intrinsic type.
        /// </summary>
        /// <param name="styOrig">The original type on the stack.</param>
        /// <param name="styCastTo">The type to convert the value to.</param>
        /// <param name="build">The WASM program to emit the casting instructions to.</param>
        public void CastIntrinsicOnStack(SynType styOrig, SynType styCastTo, WASMByteBuilder build)
        { 
            CastIntrinsicOnStack(styOrig.typeName, styCastTo.typeName, build);
        }

        /// <summary>
        /// When an intrinsic value is on the stack, emit the proper WASM assembly to
        /// convert the value to another intrinsic type.
        /// </summary>
        /// <param name="styOrig">The name of the original type on the stack.</param>
        /// <param name="styCastTo">The name of the type to convert the value to.</param>
        /// <param name="build">The WASM program to emit the casting instructions to.</param>
        public void CastIntrinsicOnStack(string styOrig, string styCastTo, WASMByteBuilder build)
        { 
            if(styOrig == styCastTo)
                return;

            // More-or-less, this is creating a grid of all intrinsic types and 
            //addressing each pair combination.

            switch(styOrig)
            { 
                case "bool":
                case "int8":
                case "int16":
                case "int":
                    switch(styCastTo)
                    { 
                        case "uint8":
                        case "uint16":
                        case "uint":
                        case "bool":
                        case "int8":
                        case "int16":
                        case "int":
                            // NOTE: Arguably we could & mask the higher bits to truncate properly
                            return;
                        case "int64":
                            build.AddInstr(WASM.Instruction.i64_extend_i32_s);
                            return;
                        case "uint64":
                            build.AddInstr(WASM.Instruction.i64_extend_i32_u);
                            return;
                        case "float":
                            build.AddInstr(WASM.Instruction.f32_convert_i32_s);
                            return;
                        case "float64":
                            build.AddInstr(WASM.Instruction.f64_convert_i32_s);
                            break;
                    }
                    break;

                case "uint8":
                case "uint16":
                case "uint":
                    switch(styCastTo)
                    {
                        case "uint8":
                        case "uint16":
                        case "uint":
                        case "bool":
                        case "int8":
                        case "int16":
                        case "int":
                            // NOTE: Arguably we could & mask the higher bits to truncate properly
                            return;
                        case "int64":
                            build.AddInstr(WASM.Instruction.i64_extend_i32_s);
                            return;
                        case "uint64":
                            build.AddInstr(WASM.Instruction.i64_extend_i32_u);
                            return;
                        case "float":
                            build.AddInstr(WASM.Instruction.f32_convert_i32_u);
                            return;
                        case "float64":
                            build.AddInstr(WASM.Instruction.f64_convert_i32_u);
                            return;
                    }
                    break;

                case "int64":
                    switch (styCastTo)
                    {
                        case "bool":
                        case "int8":
                        case "int16":
                        case "int":
                        case "uint8":
                        case "uint16":
                        case "uint":
                            build.AddInstr(WASM.Instruction.i32_wrap_i64);
                            // NOTE: Arguably we could & mask the higher bits to truncate 
                            // properly for the datatypes that have a bitwidth less than 32.
                            return;
                        case "uint64":
                            return;
                        case "float":
                            build.AddInstr(WASM.Instruction.f32_convert_i64_s);
                            return;
                        case "float64":
                            build.AddInstr(WASM.Instruction.f64_convert_i64_s);
                            return;
                    }
                    break;

                case "uint64":
                    switch(styCastTo)
                    { 
                        case "bool":
                        case "int8":
                        case "int16":
                        case "int":
                        case "uint8":
                        case "uint16":
                        case "uint":
                            build.AddInstr(WASM.Instruction.i32_wrap_i64);
                            // NOTE: Arguably we could & mask the higher bits to truncate 
                            // properly for the datatypes that have a bitwidth less than 32.
                            return;
                        case "int64":
                            return;
                        case "float":
                            build.AddInstr(WASM.Instruction.f32_convert_i64_u);
                            return;
                        case "float64":
                            build.AddInstr(WASM.Instruction.f64_convert_i64_u);
                            return;
                    }
                    break;

                case "float":
                    switch(styCastTo)
                    { 
                        case "bool":
                        case "int8":
                        case "int16":
                        case "int":
                            build.AddInstr(WASM.Instruction.i32_trunc_f32_s);
                            // NOTE: Arguably we could & mask the higher bits to truncate 
                            // properly for the datatypes that have a bitwidth less than 32.
                            return;
                        case "uint8":
                        case "uint16":
                        case "uint":
                            build.AddInstr(WASM.Instruction.i32_trunc_f32_u);
                            // NOTE: Arguably we could & mask the higher bits to truncate 
                            // properly for the datatypes that have a bitwidth less than 32.
                            return;
                        case "int64":
                            build.AddInstr(WASM.Instruction.i64_trunc_f32_s);
                            return;
                        case "uint64":
                            build.AddInstr(WASM.Instruction.i64_trunc_f32_u);
                            return;
                        case "float64":
                            build.AddInstr(WASM.Instruction.f64_promote_f32);
                            return;
                    }
                    break;

                case "double":
                    switch(styCastTo)
                    { 
                        case "bool":
                        case "int8":
                        case "int16":
                        case "int":
                            build.AddInstr(WASM.Instruction.i32_trunc_f64_s);
                            // NOTE: Arguably we could & mask the higher bits to truncate 
                            // properly for the datatypes that have a bitwidth less than 32.
                            return;
                        case "uint8":
                        case "uint16":
                        case "uint":
                            build.AddInstr(WASM.Instruction.i32_trunc_f64_u);
                            // NOTE: Arguably we could & mask the higher bits to truncate 
                            // properly for the datatypes that have a bitwidth less than 32.
                            return;
                        case "int64":
                            build.AddInstr(WASM.Instruction.i64_trunc_f64_s);
                            return;
                        case "uint64":
                            build.AddInstr(WASM.Instruction.i64_trunc_f64_u);
                            return;
                        case "float":
                            build.AddInstr(WASM.Instruction.f32_demote_f64);
                            return;
                    }
                    break;
            }

            throw new SynthExceptionImpossible($"Unable to cast intrinsic type from {styOrig} to {styCastTo}.");
        }

        /// <summary>
        /// Evaluate a function's parameters, place them on the stack, and invoke
        /// the function.
        /// </summary>
        /// <param name="fnInvoke">The function to invoke.</param>
        /// <param name="fnd">The function scope that the invoked function is being called in.</param>
        /// <param name="expr">Function call expression (used to evaluate the ASTs for the parameters)</param>
        /// <param name="wasmBuild">The wasm compiler utility.</param>
        /// <param name="ctxBuilder">The nesting context the function is being invoked in.</param>
        /// <param name="fnBuild">The WASM binary being generated.</param>
        /// <returns>Information on the location of the return value.</returns>
        public ValueRef BuildBSFunctionInvoke(
            SynFuncDecl fnInvoke, 
            SynFuncDecl fnd, 
            AST expr, 
            WASMBuild wasmBuild, 
            SynNestingBuilder ctxBuilder, 
            WASMByteBuilder fnBuild)
        { 
            for (int i = 0; i < expr.branches.Count; ++i)
            {
                ValueRef vr = BuildBSFunctionExpression(fnd, expr.branches[i], wasmBuild, ctxBuilder, fnBuild);

                if (vr.varType.intrinsic == true)
                    vr.PutInstrinsicValueOnStack(fnBuild);
            }

            return BuildBSFunctionDirectInvoke(fnInvoke, fnd, wasmBuild, fnBuild);
        }

        /// <summary>
        /// Directly invoke a function without concern for the function parameter.
        /// 
        /// This should be called for a function without any parameters, or for a function
        /// whos parameters are already set up.
        /// </summary>
        /// <param name="fnCallContext">The function that the invoked function was called in.</param>
        /// <param name="fnInvoke">The function to invoke.</param>
        /// <param name="wasmBuild">The wasm compiler utility.</param>
        /// <param name="fnBuild">The wasm binary being generated.</param>
        /// <returns>Information on the location of the return value.</returns>
        public ValueRef BuildBSFunctionDirectInvoke(
            SynFuncDecl fnInvoke,
            SynFuncDecl fnCallContext,
            WASMBuild wasmBuild,
            WASMByteBuilder fnBuild)
        {
            uint? fnIdx = wasmBuild.GetFunctionIndex(fnInvoke);

            if (fnIdx.HasValue == false)
                throw new SynthExceptionImpossible("Cound not find registered index of function");

            // If the current function calling the invokee needs the mem stack, and the 
            // invoked function needs the memstack, offset the mem stack pointer. Or else
            // do a little optimization and ignore the offset.
            int invokeMemParam = fnInvoke.parameterSet.TotalMemStackByte;
            int invokeMemStack = fnInvoke.memoryStackSize;
            //
            int ctxMemParam = fnCallContext.parameterSet.TotalMemStackByte;
            int ctxMemStack = fnCallContext.memoryStackSize;
            //
            bool requireMemStackShift = 
                (invokeMemParam + invokeMemStack > 0) && 
                (ctxMemParam + ctxMemStack > 0);

            if(requireMemStackShift == true)
            { 
                // Add the offset of how much memory the invoking function
                // requires so the invoked function doens't overwrite it.
                fnBuild.AddLEB128(0); // Store Dst
                {
                    fnBuild.AddLEB128(0);   // Load Src
                    fnBuild.Add_I32Load();  // Deref

                    // Offset
                    fnBuild.AddLEB128((uint)ctxMemParam + ctxMemStack);
                    fnBuild.AddInstr(WASM.Instruction.i32_add);
                }
                fnBuild.Add_I32Store();
            }

            fnBuild.AddInstr(WASM.Instruction.call);
            fnBuild.AddLEB128(fnIdx.Value);

            if(requireMemStackShift == true)
            { 
                // Restore the offset/shift.
                // It's the same process as above, only we're subtracting.
                fnBuild.AddLEB128(0); // Store Dst
                { 
                    fnBuild.AddLEB128(0);
                    fnBuild.Add_I32Load();
                    fnBuild.AddLEB128((uint)ctxMemParam + ctxMemStack);
                    fnBuild.AddInstr(WASM.Instruction.i32_sub);

                }
                fnBuild.Add_I32Store();
            }

            // TODO: Figure out where return value (if any) is and
            // place it in the return value.
                if (fnInvoke.returnType == null)
                return new ValueRef(ValueLoc.NoValue, 0, 0, null);
            else if (fnInvoke.returnType.intrinsic == true)
                return new ValueRef(ValueLoc.ValueOnStack, 0, 0, fnInvoke.returnType);
            else
                return new ValueRef(ValueLoc.ValueOnMemStack, 0, 0, fnInvoke.returnType);
        }

        public static void EnsureNoTreeChildNodes(TokenTree tt)
        { 
            if(tt.nodes.Count != 0)
                throw new SynthExceptionImpossible("Tree encountered unexpected children.");
        }

        public void PushType(SynType type)
        { 
            // TODO:
        }

        public void PopType(SynType type)
        { 
            //TODO:
        }

        public override SynNestingBuilder CastNest() 
        { 
            return this; 
        }
    }
}