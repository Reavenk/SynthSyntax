using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthType_Struct : SynthType
    {
        public bool resolvedAlignments = false;

        public List<Token> declarationTokens = new List<Token>();

        public int byteSize = 0;


        public SynthType_Struct(SynthScope parentScope, string name)
            : base(parentScope, name, false)
        { 
        }

        public override int GetByteSize()
        {
            if(this.resolvedAlignments == false)
                throw new System.Exception(); // TODO:

            return this.byteSize;
        }

        public override bool Aligned()
        {
            return resolvedAlignments;
        }

        public static SynthType_Struct Parse(SynthScope parentScope, List<Token> tokens)
        {
            if (tokens[0].Matches(TokenType.tyWord, "struct") == false)
                return null;

            // TODO: Check that the name is available
            if (tokens[1].Matches(TokenType.tyWord) == false)
                throw new System.Exception(); // TODO: Err msg

            if (tokens[2].Matches(TokenType.tySymbol, "{") == false)
                throw new System.Exception(); // TODO: Err msg

            int idx = 2;
            Parser.MovePastScopeTSemi(ref idx, tokens);

            SynthType_Struct ret = new SynthType_Struct(parentScope, tokens[0].fragment);
            ret.typeName = tokens[1].fragment;
            ret.declarationTokens = tokens.GetRange(0, idx);
            tokens.RemoveRange(0, idx);

            ret.BreakApartParsedTokens();

            return ret;
        }

        public override void BreakApartParsedTokens()
        {
            SynthLog.Log($"Breaking apart type {this.typeName}.");
            SynthLog.LogFragments( this.declarationTokens);

            // For struct structname { ... }, remove everything except the ...
            this.declarationTokens.RemoveRange(0, 3);

            int lastIdx = this.declarationTokens.Count - 1;
            if (this.declarationTokens[lastIdx].MatchesSymbol(";"))
                this.declarationTokens.RemoveRange(lastIdx - 1, 2);
            else
                this.declarationTokens.RemoveRange(lastIdx, 1);

            while (this.declarationTokens.Count > 0)
            { 
                if(this.declarationTokens[0].MatchesSymbol(";") == true)
                {
                    this.declarationTokens.RemoveAt(0);
                    continue;
                }

                SynthRegion rgn = SynthRegion.Parse(this, this.declarationTokens);
                if(rgn != null)
                { 
                    this.regions.Add(rgn.name, rgn);
                    continue;
                }

                SynthFuncDecl fnParse = 
                    SynthFuncDecl.Parse(
                        this, 
                        this.declarationTokens, 
                        this.typeName, 
                        false, 
                        SynthFuncDecl.ParseType.StructContext);

                if (fnParse != null)
                {
                    this.AddFunction(fnParse);
                    continue;
                }

                SynthVarValue varParse = SynthVarValue.ParseBodyVar(this.declarationTokens, SynthVarValue.OuterScope.Struct);
                if(varParse != null)
                { 
                    this.AddVariable(varParse);
                    continue;
                }
            }
        }

        public override TypeConsolidate ResolveStaticTypeAlignments()
        {
            if(resolvedAlignments == true)
                return TypeConsolidate.AllDetermined;

            int alreadyAligned = 0;
            int newlyAligned = 0;
            foreach(var v in this.varDefs)
            { 
                if(v.type == null)
                { 
                    if(string.IsNullOrEmpty(v.typeName) == true)
                        throw new System.Exception("Unknown type.");

                    v.type = this.GetType(v.typeName);
                }

                if (v.type.Aligned() == false)
                {
                    ++alreadyAligned;
                    continue;
                }

                TypeConsolidate tc = v.type.ResolveStaticTypeAlignments();
                if ( tc == TypeConsolidate.AllDetermined)
                {
                    ++newlyAligned;
                    continue;
                }

                if(tc == TypeConsolidate.UndeterminedProgress)
                    return TypeConsolidate.UndeterminedProgress;
            }

            if(alreadyAligned + newlyAligned < this.varDefs.Count)
            { 
                if(newlyAligned == 0)
                    return TypeConsolidate.UndeterminedNoChange;
                else
                    return TypeConsolidate.UndeterminedProgress;
            }

            // All the Type references should now be set - so before we 
            // lookup the offsets, we're going to make sure the struct isn't
            // containing itself, or a child member that's containing
            // itself.
            HashSet<string> typesOfChildren = new HashSet<string>();
            this.RegisterContainedValueTypes(typesOfChildren);
            if(typesOfChildren.Contains(this.typeName) == true)
                throw new SynthExceptionCompile($"Datatype {this.typeName} found to contain recursive instances of itself as a child member.");

            int alignment = 0;
            foreach(var v in this.varDefs)
            { 
                v.alignmentOffset = alignment;
                alignment += v.type.GetByteSize();
            }
            this.byteSize = alignment;

            this.resolvedAlignments = true;
            return TypeConsolidate.AllDetermined;
        }

        public override void RegisterContainedValueTypes(HashSet<string> registered) 
        { 
            foreach( var v in this.varDefs)
            {
                if(v.dataType != SynthVarValue.VarValueDataType.Value)
                    continue;

                // If it's already been registered, don't do another registration
                // pass, this could lead to an infinite loop.
                if(registered.Add(v.typeName) == false)
                    continue;

                if(v.type != null)
                    v.type.RegisterContainedValueTypes(registered);
            }
        }

        public override void Validate_AfterTypeAlignment(int logIndent)
        {
            SynthLog.Log("");
            SynthLog.LogIndent(logIndent, $"Begin Struct.Validate_AfterTypeAlignment : {this.typeName}");
            base.Validate_AfterTypeAlignment(logIndent + 1);
            SynthLog.LogIndent(logIndent, $"End Struct.Validate_AfterTypeAlignment");
        }

        public override SynthType_Struct GetStructScope()
        {
            return this;
        }

        public override SynthFuncDecl GetDefaultConstructor()
        { 
            List<SynthFuncDecl> lstFns;
            if(this.functions.TryGetValue(this.typeName, out lstFns) == false)
                return null;

            foreach(SynthFuncDecl sfd in lstFns)
            { 
                if(sfd.isConstructor == false)
                    continue;

                if(sfd.parameterSet.Count != 1)
                    continue;

                // TODO: Runtime error checking/validation

                return sfd;
            }
            return null;
        }
    }
}
