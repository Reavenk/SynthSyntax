using System.Collections.Generic;

namespace PxPre.SynthSyn
{
    /// <summary>
    /// SynthSyntax Function declaration and definition.
    /// </summary>
    public class SynthFuncDecl : SynthScope
    {
        [System.Flags]
        public enum ParseType
        { 
            Static          = 1 << 0,
            Member          = 1 << 1,
            Constructor     = 1 << 2,
            Entry           = 1 << 3,
            Externable      = 1 << 4,

            // If a region function, it must either succeed or throw.
            Region          = 1 << 5,

            RootContext     = Static | Entry | Externable,
            FunctionContext = Static,
            StructContext   = Static | Member | Constructor
        }

        public enum CallType
        { 
            // Entry functions are exported WASM functions
            // that are static and do no have a return value.
            // 
            // The expected entry names and their use depends 
            // on what SynthSyntax is used for.
            Entry,

            /// <summary>
            /// Extern functions are imported WASM functions. 
            /// All externed functions are static.
            /// </summary>
            Extern,

            /// <summary>
            /// The function is a static function
            /// </summary>
            Static,

            /// <summary>
            /// The function is a struct member.
            /// </summary>
            Member,

            /// <summary>
            /// The function is a struct constructor.
            /// </summary>
            Constructor,

            /// <summary>
            /// The function is a struct destructor.
            /// </summary>
            Destructor
        }

        /// <summary>
        /// The name of the function.
        /// </summary>
        public string functionName;

        /// <summary>
        /// The name of the return type. This will be defined from parsing
        /// the source code before - and depending on the phase of compilation,
        /// the actual return type may not even be processed yet.
        /// </summary>
        public string returnTyName;

        /// <summary>
        /// A reference to the processed return type - the type (in scope) whos
        /// typename matches returnTyName.
        /// </summary>
        public SynthType returnType;

        public SynthFuncParamSet parameterSet = new SynthFuncParamSet();

        public List<SynthRegion> regionList = new List<SynthRegion>();
        public Dictionary<string, SynthRegion> regionLookup = new Dictionary<string, SynthRegion>();

        /// <summary>
        /// Cache if the function is an operator.
        /// </summary>
        public bool isOperator = false;

        /// <summary>
        /// Only relevant if the function is an operator. If so, the function can also be invoked
        /// if called in reverse order.
        /// </summary>
        public bool isReversible = false;

        public CallType callType;

        /// <summary>
        /// Used in BreakApartParsedTokens to specify the cached location where 
        /// parenthesis were found to start.
        /// </summary>
        public int parenStart = -1;

        public int paramByteSize = -1;

        public List<List<Token>> executingLines = new List<List<Token>>();

        // TODO: Reorganize correctly (refactor)
        // Cached WASM types on the stack. This should map 1-to-1 with the stack variable listing
        public List<WASM.Bin.TypeID> localTypes = new List<WASM.Bin.TypeID>();

        // TODO: Reorganize correctly (refactor)
        // The AST for the function
        public TokenAST ast;

        // TODO: Reorganize correctly (refactor)
        // The WASM bytecode for the function.
        public byte [] fnBin = null;

        /// <summary>
        /// If true, the function is a global/static. Else, it's a member variable.
        /// </summary>
        public bool isStatic
        {
            get =>
                this.callType == CallType.Static ||
                this.callType == CallType.Entry || 
                this.callType == CallType.Extern;
        }

        /// <summary>
        /// Cache if the function is an entry.
        /// </summary>
        public bool isEntry
        {
            get => this.callType == CallType.Entry;
        }

        /// <summary>
        /// True if the function is a constructor.
        /// </summary>
        public bool isConstructor
        {
            get => this.callType == CallType.Constructor;
        }

        public bool isDestructor
        {
            get => this.callType == CallType.Destructor;
        }

        public bool isMethod
        { 
            get => this.callType == CallType.Member;
        }

        public bool isStructInvoked
        { 
            get => 
                this.callType == CallType.Member ||
                this.callType == CallType.Constructor ||
                this.callType == CallType.Destructor;
        }

        public bool isExtern
        { 
            get => this.callType == CallType.Extern;
        }

        public SynthFuncDecl(SynthScope parentScope)
            : base(parentScope)
        { 
        }

        public static SynthFuncDecl Parse(SynthScope parentScope, List<Token> tokens, string structName, bool isStatic, ParseType parseTypes)
        {
            bool isExtern = false;
            bool committed = false;
            bool isEntry = false;

            int idx = 0;

            //          Get keyword modifiers
            //
            //////////////////////////////////////////////////
            while (true)
            {
                if((parseTypes & ParseType.Static) != 0)
                {
                    if(tokens[idx].Matches( TokenType.tyWord, "static") == true)
                    {
                        isStatic = true;
                        ++idx;
                        continue;
                    }
                }

                if((parseTypes & ParseType.Externable) != 0)
                {
                    if (tokens[idx].Matches(TokenType.tyWord, "extern") == true)
                    {
                        isExtern = true;
                        committed = true;
                        ++idx;
                        continue;
                    }
                }

                break;
            }

            // Explicit check for destructor
            if((parseTypes & ParseType.Member) != 0 &&
                string.IsNullOrEmpty(structName) == false &&
                tokens[idx].MatchesSymbol("~") && 
                tokens[idx + 1].Matches(TokenType.tyWord) &&
                tokens[idx + 1].fragment == structName)
            {
                if (isStatic == true)
                    throw new SynthExceptionSyntax(tokens[0], "Static destructors not allowed.");

                if (isExtern == true)
                    throw new SynthExceptionSyntax(tokens[0], "Externed destructors are not allowed.");

                // We already know this is the same as structName, but we'll extract it
                // and get the name from the fragment for the sake of rigor.
                string destructorName = tokens[idx].fragment + tokens[idx + 1].fragment;

                // Finding a destructor is a commited parse.
                if (tokens[idx + 2].MatchesSymbol("(") == false)
                    throw new SynthExceptionSyntax(tokens[idx + 2], "Incorrect syntax for destructor.");

                idx += 2;
                int dstrIdx = idx;
                Parser.MovePastScopeTSemi(ref dstrIdx, tokens);

                SynthFuncDecl retDstr = new SynthFuncDecl(parentScope);
                retDstr.returnTyName = "";
                retDstr.functionName = destructorName;
                retDstr.callType = CallType.Destructor;
                retDstr.parenStart = idx;

                retDstr.declPhrase = tokens.GetRange(0, dstrIdx);
                tokens.RemoveRange(0, dstrIdx);

                retDstr.BreakApartParsedTokens();
                return retDstr;
            }

            // Explicit check for constructor
            if (
                (parseTypes & ParseType.Member) != 0 &&
                string.IsNullOrEmpty(structName) == false && 
                tokens[idx].Matches(TokenType.tyWord) && 
                tokens[idx].fragment == structName)
            { 
                // constructor

                if(isStatic == true)
                    throw new SynthExceptionSyntax(tokens[0], "Static constructor not allowed.");

                if(isExtern == true)
                    throw new SynthExceptionSyntax(tokens[0], "Externed constructors are not allowed.");

                // We already know this is the same as structName, but we'll extract it
                // and get the name from the fragment for the sake of rigor.
                string constructorName = tokens[idx].fragment;

                
                if (tokens[idx + 1].MatchesSymbol("(") == true)
                {
                    ++idx;
                    int constrIdx = idx;
                    Parser.MovePastScopeTSemi(ref constrIdx, tokens);

                    SynthFuncDecl retConstr     = new SynthFuncDecl(parentScope);
                    retConstr.returnTyName      = "";
                    retConstr.functionName      = constructorName;
                    retConstr.callType          = CallType.Constructor;
                    retConstr.parenStart        = idx;

                    retConstr.declPhrase = tokens.GetRange(0, constrIdx);
                    tokens.RemoveRange(0, constrIdx);

                    retConstr.BreakApartParsedTokens();
                    return retConstr;
                }
            }

            string typeName = string.Empty;
            bool hasType = true;

            if((parseTypes & ParseType.Region) != 0)
                hasType = false;
            if( (parseTypes & ParseType.Entry) != 0  &&
                tokens[idx].Matches(TokenType.tyWord, "entry") == true)
            {
                isEntry = true;
                hasType = false;
                ++idx;
            }

            // Region don't have return types
            if(hasType == true)
            {
                if (tokens[idx].Matches(TokenType.tyWord) == false)
                    return null;

                if (_ParseReservedKeywords_NotFunctions.Contains(tokens[idx].fragment) == true)
                    return null;

                typeName = tokens[idx].fragment;
                ++idx;
            }

            if(tokens[idx].Matches(TokenType.tyWord) == false)
            { 
                return null;
            }

            string fnName = tokens[idx].fragment;
            ++idx;

            bool isReversible = false;
            bool isOperator = false;
            if(
                (parseTypes & ParseType.Member) != 0 &&
                string.IsNullOrEmpty(structName) == false && 
                fnName == "operator")
            {
                if(isExtern == true)
                    throw new SynthExceptionSyntax(tokens[0], "Operators cannot be exported.");

                if (isStatic == true)
                    throw new SynthExceptionSyntax(tokens[0], "Operators cannot be static.");

                if (tokens[idx].Matches(TokenType.tySymbol) == false)
                    throw new SynthExceptionSyntax(tokens[0], "Unknown operator type {tokens[idx].fragment.");

                isOperator = true;

                switch(tokens[idx].fragment)
                { 
                    case "+":
                    case "-":
                    case "/":
                    case "*":
                    case "+=":
                    case "-=":
                    case "/=":
                    case "*=":
                    case "==":
                    case "!=":
                    case "<":
                    case "<=":
                    case ">":
                    case ">=":
                    case ">>=":
                    case "<<=":
                        fnName += tokens[idx].fragment;
                        ++idx;
                        break;
                    default:
                        throw new System.Exception($"Unknown operator type {tokens[1].fragment} on line {tokens[1].line}.");
                }

                if(tokens[idx].Matches(TokenType.tyWord, "rev") == true)
                { 
                    ++idx;
                    isReversible = true;
                }
            }

            if(tokens[idx].Matches(TokenType.tySymbol, "(") == false)
            {
                if(isOperator == true)
                    throw new SynthExceptionSyntax(tokens[idx], $"Operator declared incorrectly.");
                return null;
            }

            int parenSpot = idx;
            Parser.MovePastScopeTSemi(ref idx, tokens);

            if(typeName == "void")
                typeName = string.Empty;

            SynthFuncDecl ret = new SynthFuncDecl(parentScope);
            ret.returnTyName    = typeName;
            ret.functionName    = fnName;
            ret.isReversible    = isReversible;
            ret.isOperator      = isOperator;
            ret.parenStart      = parenSpot;

            if(isEntry == true)
                ret.callType = CallType.Entry;
            else if(isExtern == true)
                ret.callType = CallType.Extern;
            else if(isStatic == true)
                ret.callType = CallType.Static;
            else
            {
                // Constructor and destructor declarations are handled elsewhere,
                // so this is the only thing left be process of elimination.
                ret.callType = CallType.Member;
            }


            ret.declPhrase = tokens.GetRange(0, idx);
            tokens.RemoveRange(0, idx);

            ret.BreakApartParsedTokens();

            return ret;
        }

        private static HashSet<string> _ParseReservedKeywords_NotFunctions = 
            new HashSet<string>{ "if", "return", "region", "for", "while", "rev", "operator" };

        public override void BreakApartParsedTokens()
        {
            // Trim end semicolons
            while (this.declPhrase[this.declPhrase.Count - 1].Matches(";") == true)
                this.declPhrase.RemoveAt(this.declPhrase.Count - 1);

            //
            //      VERIFICATIONS &  EXTRACTING PARAM SECTIONS 
            //      AND FUNCTION BODY SECTION
            //
            //////////////////////////////////////////////////

            // Ensure opening
            if(this.declPhrase[this.parenStart].MatchesSymbol("(") == false)
                throw new SynthExceptionImpossible("Open parenthesis is not found at cached location.");

            int startParams = this.parenStart;
            int endParen = startParams;
            Parser.MovePastScope(ref endParen, this.declPhrase, ")", new HashSet<string>());

            
            int startFn = endParen;
            int endFn = startFn;
            if(this.isExtern == true)
            {
                if (this.declPhrase.Count != endParen && this.declPhrase[endParen].MatchesSymbol(";") == false)
                    throw new SynthExceptionSyntax(this.declPhrase[endParen], $"Externed functions cannot have a body. They are expected to end with a semicolon.");
            }
            else
            {
                if(this.declPhrase[startFn].MatchesSymbol("{") == false)
                    throw new System.Exception(); // TODO: Error msg

                Parser.MovePastCurlScope(ref endFn, declPhrase, true);

                if(endFn != this.declPhrase.Count)
                    throw new System.Exception(""); // TODO: error msg
            }

            //
            //      FINAL SECTION EXTRACTIONS
            //
            //////////////////////////////////////////////////

            // Given ty fnName( p1, p2, p3, ...){fnbody}
            // extract the p1, p2, p3 part.
            List<Token> paramsPhrase = this.declPhrase.GetRange(startParams + 1, endParen - startParams - 2);

            // Given ty fnName( p1, p2, p3, ...){fnbody}
            // extract the fnbody part.
            List<Token> fnBodPhrase = null;
            
            if(isExtern == false)
            {
                // TODO: Log and debug stuff
                fnBodPhrase = this.declPhrase.GetRange(startFn + 1, endFn - startFn - 2);
            }

            //
            //      FORMALIZE DECLARACTIONS
            //
            //////////////////////////////////////////////////

            if(this.isStatic == false)
            { 
                SynthType styThis = this.GetStructScope();
                this.parameterSet.AddThisParam(styThis);
            }

            while(paramsPhrase.Count > 0) 
            { 
                SynthVarValue varParam = SynthVarValue.ParseParameter(paramsPhrase);
                if (varParam == null)
                    throw new System.Exception($"Unexpected parameter declaration on line {paramsPhrase[0].line}.");

                this.parameterSet.AddParameter(varParam);

                if(paramsPhrase.Count > 0)
                {
                    if(paramsPhrase[0].Matches(TokenType.tySymbol, ",") == false)
                        throw new System.Exception($"Unexpected token in function parameters on line {paramsPhrase[0].line}, expected comma.");

                    paramsPhrase.RemoveAt(0); // TODO: Force check that there's stuff after the comma?
                }
            }

            //
            //      BREAK BODY
            //
            //////////////////////////////////////////////////

            if(fnBodPhrase != null)
            {
                SynthLog.Log("Parsing function body.");

                while (fnBodPhrase.Count > 0)
                { 
                    if(Parser.EatLeadingSemicolons(fnBodPhrase) == true)
                        continue;

                    SynthType_Struct sst = SynthType_Struct.Parse(this, fnBodPhrase);
                    if(sst != null)
                    { 
                        this.AddType(sst);
                        sst.BreakApartParsedTokens();
                        continue;
                    }

                    SynthFuncDecl sfn = SynthFuncDecl.Parse(this, fnBodPhrase, "", false, ParseType.FunctionContext);
                    if(sfn != null)
                    { 
                        this.AddFunction(sfn);
                        sfn.BreakApartParsedTokens();
                        continue;
                    }

                    int idx = 0;
                    Parser.MovePastScopeTSemi(ref idx, fnBodPhrase);

                    if(idx == 0)
                        throw new System.Exception("");

                    List<Token> execPhrase = fnBodPhrase.GetRange(0, idx);
                    fnBodPhrase.RemoveRange(0, idx);

                    SynthLog.Log("Extracted function phrase:");
                    SynthLog.LogFragments( execPhrase );

                    this.executingLines.Add(execPhrase);
                }
            }

            SynthLog.Log($"Finished BreakApartParsedTokens for {this.functionName}.");
        }

        public override void Validate_AfterTypeAlignment(int logIndent)
        {
            SynthLog.Log("");
            SynthLog.LogIndent(logIndent, $"Started {this.functionName}.Validate_AfterTypeAlignment()");


            //      VERIFY RETURN VALUE
            //////////////////////////////////////////////////
            SynthLog.LogIndent(logIndent + 1, "Checking return value type:");
            if (string.IsNullOrEmpty(this.returnTyName) == false)
            { 
                SynthType ret = this.GetType(this.returnTyName);
                if(ret == null)
                {
                    throw new SynthExceptionSyntax(
                        this.lineNumber, 
                        $"Function {this.functionName} has unknown return type {this.returnTyName}.");

                }
                
                this.returnType = ret;
                SynthLog.LogIndent(logIndent + 2, $"Verified return value {this.returnTyName} with {this.returnType.typeName}.");

            }
            else
                SynthLog.LogIndent(logIndent + 2, "No return value.");

            //      VERIFY PARAMETER TYPES
            //////////////////////////////////////////////////
            SynthLog.LogIndent(logIndent + 1, "Checking parameter types:");

            this.parameterSet._Validate(this);

            if(this.parameterSet.Count == 0)
            {
                SynthLog.LogIndent(logIndent + 2, "No parameters.");
            }
            else
            {
                for (int i = 0; i < this.parameterSet.Count; ++i)
                { 
                    SynthVarValue svvParam = this.parameterSet.Get(i);

                    SynthType p = this.GetType(svvParam.typeName);
                    if(p == null)
                    {
                        throw new SynthExceptionSyntax(
                            this.lineNumber, 
                            $"Function {this.functionName} has unknown type for parameter {i}, {svvParam.varName}.");
                    }

                    svvParam.type = p;
                    SynthLog.LogIndent(logIndent + 2, $"Verified parameter {i}, {svvParam.varName}, {p.typeName} with {svvParam.type.typeName}.");
                }
            }

            // It's arguable this should be in here since this is a validation
            // function, and not a processing function.
            this.parameterSet.PostTypeAlignment(this);

            base.Validate_AfterTypeAlignment(logIndent + 1);

            SynthLog.LogIndent(logIndent, $"Ended {this.functionName}.Validate_AfterTypeAlignment()");
        }

        public override SynthFuncDecl CastFuncDecl()
        {
            return this;
        }

        public override SynthCanidateFunctions CastCanidateFunctions() 
        { 
            SynthCanidateFunctions retSelf = new SynthCanidateFunctions(this.GetStructScope());
            retSelf.functions.Add(this);
            return retSelf;
        }

        public override void GatherFunctionRegistration(WASMBuild builds)
        { 
            builds.RegisterFunction(this);

            base.GatherFunctionRegistration(builds);
        }

        public SynthVarValue GetParameter(string name)
        {
            return this.parameterSet.Get(name);
        }

        public override SynthVarValue GetVar(string name, bool recursion = true)
        { 
            SynthVarValue svvParam = GetParameter(name);
            if(svvParam != null)
                return svvParam;

            return base.GetVar(name, recursion);
        }

        /// <summary>
        /// The term 'input' parameter refers to parameters that are explicitly 
        /// given values in the source code. This is pretty much every parameter
        /// except the 'this' variable (if the function even has one).
        /// </summary>
        /// <returns></returns>
        public int GetInputParameters()
        { 
            if(this.isStatic == true)
                return this.parameterSet.Count;

            // Remove the first entry, which is assumed to be the 'this' parameter.
            return this.parameterSet.Count - 1;
        }
    }
}
