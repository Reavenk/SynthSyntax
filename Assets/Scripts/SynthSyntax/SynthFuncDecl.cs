using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
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

        public string functionName;

        public string returnTyName;
        public SynthType returnType;

        public List<SynthVarValue> paramList = new List<SynthVarValue>();
        public Dictionary<string, SynthVarValue> paramLookup = new Dictionary<string, SynthVarValue>();

        public List<SynthRegion> regionList = new List<SynthRegion>();
        public Dictionary<string, SynthRegion> regionLookup = new Dictionary<string, SynthRegion>();

        /// <summary>
        /// Cache if the function is an entry.
        /// </summary>
        public bool isEntry = false;

        /// <summary>
        /// True if the function is a constructor.
        /// </summary>
        public bool isConstructor = false;

        /// <summary>
        /// Cache if the function is an operator.
        /// </summary>
        public bool isOperator = false;

        /// <summary>
        /// Only relevant if the function is an operator. If so, the function can also be invoked
        /// if called in reverse order.
        /// </summary>
        public bool isReversible = false;

        /// <summary>
        /// If true, the function is a global/static. Else, it's a member variable.
        /// </summary>
        public bool isStatic = false;

        /// <summary>
        /// Used in BreakApartParsedTokens to specify the cached location where 
        /// parenthesis were found to start.
        /// </summary>
        public int parenStart = -1;

        public int paramByteSize = -1;

        public List<List<Token>> executingLines = new List<List<Token>>();

        public bool isExtern = false;

        public SythContextBuilder build = new SythContextBuilder();

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

            // Explicit check for constructor
            if(
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
                    retConstr.isConstructor     = true;
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
            ret.isStatic        = isStatic;
            ret.isExtern        = isExtern;
            ret.isEntry         = isEntry;

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
                this.declPhrase.GetRange(startFn + 1, endFn - startFn - 2);

            //
            //      FORMALIZE DECLARACTIONS
            //
            //////////////////////////////////////////////////

            // paramList
            // paramLookup

            while(paramsPhrase.Count > 0) 
            { 
                SynthVarValue varParam = SynthVarValue.ParseParameter(paramsPhrase);

                if(varParam != null)
                {
                    this.paramList.Add(varParam);
                    this.paramLookup.Add(varParam.varName, varParam);
                }
                else
                    throw new System.Exception($"Unexpected parameter declaration on line {paramsPhrase[0].line}.");

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
            if(this.paramList.Count == 0)
            {
                SynthLog.LogIndent(logIndent + 2, "No parameters.");
            }
            else
            {
                for (int i = 0; i < this.paramList.Count; ++i)
                { 
                    SynthVarValue svvParam = this.paramList[i];

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

            // Alignment of parameters
            this.paramByteSize = 0;
            foreach(SynthVarValue svv in this.paramList)
            { 
                svv.alignmentOffset = this.paramByteSize;
                this.paramByteSize += svv.GetByteSize();
            }

            // Validation of default parameters.
            bool startedDefaultSection = false;
            foreach(SynthVarValue svv in this.paramList)
            { 
                if(svv.declPhrase.Count < 2)
                    throw new SynthExceptionImpossible($"Parameter {svv.varName} for function {this.functionName} found with less than 2 tokens.");

                if(svv.declPhrase.Count > 2)
                {
                    // TODO: Hardcoded 2
                     if(svv.declPhrase[2].Matches( TokenType.tySymbol, "=") == false)
                        throw new SynthExceptionSyntax(svv.declPhrase[2], "Unexpected addition to parameter declaration on line.");



                    startedDefaultSection = true;
                }
                else if(startedDefaultSection == true)
                { 
                    throw new SynthExceptionSyntax(svv.declPhrase[1], "All parameters after the first default parameter must also have default parameters.");
                }
            }

            base.Validate_AfterTypeAlignment(logIndent + 1);

            SynthLog.LogIndent(logIndent, $"Ended {this.functionName}.Validate_AfterTypeAlignment()");
        }

        public override SynthFuncDecl CastFuncDecl()
        {
            return this;
        }

        public SythContextBuilder Build(WASMBuild build, SythContextBuilder parentContext)
        {
            SythContextBuilder builder = new SythContextBuilder();

            List<TokenTree> treeLines = new List<TokenTree>();
            for(int i = 0; i < executingLines.Count; ++i)
            { 
                List<Token> execLine = executingLines[i];
                TokenTree rootLineNode = TokenTree.EatTokensIntoTree(execLine);
                treeLines.Add(rootLineNode);
            }

            builder.ProcessContextTokens(build, this, treeLines);

            return builder;
        }

        public override void GatherFunctionRegistration(WASMBuild builds)
        { 
            builds.RegisterFunction(this);

            base.GatherFunctionRegistration(builds);
        }

    }
}
