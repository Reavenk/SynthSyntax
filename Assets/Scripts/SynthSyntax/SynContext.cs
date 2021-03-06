using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynContext : SynScope
    {
        public enum Type
        { 
            tyMain,
            tyScope,
            tyStruct
        }

        int totalGlobalBytes = -1;

        public SynContext()
            : base(null)
        { 
            this.RegisterType(new SynIntrinsic(this, "bool",   1));
            this.RegisterType(new SynIntrinsic(this, "int",    4));
            this.RegisterType(new SynIntrinsic(this, "uint",   4));
            this.RegisterType(new SynIntrinsic(this, "int64",  8));
            this.RegisterType(new SynIntrinsic(this, "uint64", 8));
            this.RegisterType(new SynIntrinsic(this, "int8",   1));
            this.RegisterType(new SynIntrinsic(this, "uint8",  1));
            this.RegisterType(new SynIntrinsic(this, "int16",  2));
            this.RegisterType(new SynIntrinsic(this, "uint16", 2));
            this.RegisterType(new SynIntrinsic(this, "float",  4));
            this.RegisterType(new SynIntrinsic(this, "float64",8));
        }

        public void RegisterType(SynType sty)
        { 
            if(this.typesDefs.ContainsKey(sty.typeName) == true)
                throw new System.Exception("Attempting to register typename that is already assigned.");

            this.typesDefs.Add(sty.typeName, sty);
        }

        public void ParseContext(List<Token> tokens)
        {
            SynLog.LogHeader("Starting Parse Content");
            SynLog.Log("\tTokens:");
            SynLog.Log(tokens);

            //      Get param globals first
            //
            //////////////////////////////////////////////////
            while (tokens.Count > 0)
            {
                SynVarValue parsedParam = SynVarValue.ParseExposedParam(tokens);
                if(parsedParam == null)
                    break;

                this.AddVariable(parsedParam);
            }

            //      Parse Sections
            //
            //////////////////////////////////////////////////
            int idx = 0;
            while(tokens.Count > 0)
            { 
                // Get rid of stray parenthesis
                if(tokens[idx].Matches(TokenType.tySymbol, ";") == true)
                {
                    tokens.RemoveAt(0);
                    continue;
                }

                // Struct parsing
                if(tokens[0].Matches(TokenType.tyWord, "struct") == true)
                {
                    SynStruct sst = SynStruct.Parse(this, tokens);
                    if (sst != null)
                    {
                        this.AddType(sst);
                        continue;
                    }
                }

                // Function parsing
                if(tokens[0].Matches(TokenType.tyWord, "entry") == true)
                { 
                    SynFuncDecl sfd = SynFuncDecl.Parse(this, tokens, "", true, SynFuncDecl.ParseType.Entry);
                    sfd.callType = SynFuncDecl.CallType.Entry;

                    if (sfd != null)
                    {
                        this.AddFunction(sfd);
                        continue;
                    }
                    else
                        throw new System.Exception("entry keyword not part of valid function.");
                }

                SynFuncDecl synthFn = 
                    SynFuncDecl.Parse(
                        this, tokens, 
                        "", 
                        true, 
                        SynFuncDecl.ParseType.RootContext);

                if(synthFn != null)
                { 
                    this.AddFunction(synthFn);
                    continue;
                }

                SynVarValue synthVar = SynVarValue.ParseBodyVar(tokens, SynVarValue.OuterScope.Global);
                if(synthVar != null)
                {
                    this.AddVariable(synthVar);
                    continue;
                }

                throw new System.Exception("Unknown token while parsing root context.");
            }

            //      Verify Structs
            //
            //////////////////////////////////////////////////
            SynLog.LogHeader("Verifying types");

            while (true)
            { 
                TypeConsolidate tc = this.ResolveStaticTypeAlignments();
                if(tc == TypeConsolidate.UndeterminedNoChange)
                    throw new System.Exception("Could not resolve all types");

                if(tc == TypeConsolidate.AllDetermined)
                    break;
            }

            SynLog.Log("Finished verifying struct successfully.");

            //      Gathering globals
            //
            //////////////////////////////////////////////////
            SynLog.LogHeader("Gathering globals");

            List<SynVarValue> globals = new List<SynVarValue>();

            foreach(SynScope s in this.EnumerateScopes())
                s.RegisterGlobals(globals);

            this.totalGlobalBytes = 0;
            foreach(SynVarValue svv in globals)
            { 
                svv.alignmentOffset = this.totalGlobalBytes;

                int byteSz = svv.type.GetByteSize();
                if(byteSz <= 0)
                    throw new SynthExceptionImpossible("Data type for global variable is zero in size.");

                SynLog.Log($"Added {svv.varName} to globals at offset {svv.alignmentOffset}");

                this.totalGlobalBytes += byteSz;
            }

            SynLog.Log($"Total global variable space is {this.totalGlobalBytes}.");

            //      Verify Functions
            //
            //////////////////////////////////////////////////
            SynLog.LogHeader("Verifying After function and variable collection pass.");
            this.Validate_AfterTypeAlignment(0);

            SynLog.Log("Finished verifying functions successfully.");

        }

        SynVarValue GetParamPhrase(List<Token> tokens)
        { 
            int idx = 0;
            if(tokens[idx].Matches(TokenType.tyWord, "param") == false)
                return null;

            if(
                tokens[idx + 1].Matches(TokenType.tyWord) == false ||
                tokens[idx + 2].Matches(TokenType.tyWord) == false)
            { 
                throw new System.Exception($"Error parsing param on line {tokens[idx].line}");
            }

            string typename = tokens[idx + 1].fragment;
            string varname = tokens[idx + 2].fragment;

            if(this.varLookups.ContainsKey(varname) == true)
                throw new System.Exception($"Redefining variable {varname} on line {tokens[idx].line}.");

            SynVarValue newParam = new SynVarValue();
            newParam.typeName = typename;
            newParam.varName = varname;
            newParam.varLoc = SynVarValue.VarLocation.Parameter;

            idx = 3;
            if(tokens[idx].Matches(TokenType.tySymbol, "=") == true)
            {
                ++idx;
                Parser.MovePastScopeTSemi(ref idx, tokens);
            }
            else if(tokens[idx].Matches(TokenType.tySymbol, ";") == false)
            { 
                throw new System.Exception($"Unexpected end to param {varname} on line {tokens[idx].line}.");
            }

            List<Token> declPhrase = tokens.GetRange(0, idx);
            tokens.RemoveRange(0, idx);

            newParam.declPhrase = declPhrase;

            this.AddVariable(newParam);

            return newParam;
        }

        public void ParseFile(string filepath)
        {
            SynLog.LogHeader("Parsing file " + filepath);

            using (var scope = new SynLog.LogScope())
            {
                string fileContents = System.IO.File.ReadAllText(filepath);
                this.ParseString(fileContents);
            }
        }

        public void ParseString(string synsynText)
        {
            int idx = 0;
            List<Token> tokens = Parser.ParseTokens(synsynText, ref idx);
            this.ParseContext(tokens);
        }

        public override void Validate_AfterTypeAlignment(int logIndent)
        {
            SynLog.LogIndent(logIndent, $"Started Context.Validate_AfterTypeAlignment()");

            if (varDefs.Count != 0)
                throw new SynthExceptionImpossible("Root context found with local members. Only global members should be possible.");

            foreach(SynFuncDecl scopeFn in this.EnumerateScopedFunctions())
            {
                if(scopeFn.isStatic == false)
                    throw new SynthExceptionImpossible("Root context found with local methods. Only static functions should be possible.");
            }

            base.Validate_AfterTypeAlignment(logIndent + 1);

            SynLog.LogIndent(logIndent, $"Ended Context.Validate_AfterTypeAlignment()");
        }

        public override SynContext CastContext()
        {
            return this;
        }
    }
}
