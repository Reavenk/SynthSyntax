using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynStruct : SynType
    {
        public bool resolvedAlignments = false;

        public List<Token> declarationTokens = new List<Token>();

        public int byteSize = 0;


        public SynStruct(SynthScope parentScope, string name)
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

        public static SynStruct Parse(SynthScope parentScope, List<Token> tokens)
        {
            if (tokens[0].Matches(TokenType.tyWord, "struct") == false)
                return null;

            // TODO: Check that the name is available
            if (tokens[1].Matches(TokenType.tyWord) == false)
                throw new SynthExceptionSyntax(tokens[1], "Missing struct name.");

            if (tokens[2].Matches(TokenType.tySymbol, "{") == false)
                throw new SynthExceptionSyntax(tokens[2], "Missing struct body entry.");

            int idx = 2;
            Parser.MovePastScopeTSemi(ref idx, tokens);

            SynStruct ret = new SynStruct(parentScope, tokens[0].fragment);
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

        public override SynStruct GetStructScope()
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

        public override SynthFuncDecl GetDestructor()
        {
            List<SynthFuncDecl> lstFn;
            if(this.functions.TryGetValue("~" + this.typeName, out lstFn) == false)
                return null;

            if(lstFn.Count != 1)
                throw new SynthExceptionImpossible($"{this.typeName} found to have multiple destructors.");

            return lstFn[0];
        }


        /// <summary>
        /// This function is expected to be called during AST construction
        /// (of whatever needs the copy constructor). This means struct 
        /// processing and alignment should have already occured. This also
        /// means the function will be queued for being constructed into
        /// WASM binary like all other functions in a later pass.
        /// </summary>
        /// <param name="autocreate">If the function doesn't exist, auto
        /// create a default copy constructor and register it with the struct.
        /// </param>
        /// <returns>The found, or created, copy constructor for the struct.</returns>
        public override SynthFuncDecl GetCopyConstructor(bool autocreate, SynthContextBuilder scb)
        { 
            List<SynthFuncDecl> lstFns;
            if(this.functions.TryGetValue(this.typeName, out lstFns) == false)
            { 
                if(autocreate == false)
                    return null;

                // If we're autocreating, prepare a list for it to be registered in.
                lstFns = new List<SynthFuncDecl>();
                this.functions.Add(this.typeName, lstFns);
            }
            else
            { 
                foreach(SynthFuncDecl fns in lstFns)
                { 
                    if(fns.returnType != null)
                        continue;

                    if(fns.parameterSet.Count != 2)
                        continue;

                    if(
                        fns.parameterSet.Get(0).type != this ||
                        fns.parameterSet.Get(1).type != this)
                    {
                        continue;
                    }

                    return fns;
                }

                if(autocreate == false)
                    return null;
            }

            SynthFuncDecl sfdCC = new SynthFuncDecl(this);
            //
            SynthVarValue svvDst = new SynthVarValue();
            svvDst.varLoc = SynthVarValue.VarLocation.ThisRef;
            svvDst.dataType = SynthVarValue.VarValueDataType.Pointer;
            //
            SynthVarValue svvSrc = new SynthVarValue();
            svvSrc.varLoc = SynthVarValue.VarLocation.Parameter;
            svvSrc.dataType = SynthVarValue.VarValueDataType.Reference;

            sfdCC.parameterSet.AddParameter(svvDst);
            sfdCC.parameterSet.AddParameter(svvSrc);

            // TODO: Do explicit alignment here?

            // Add this ahead of time
            lstFns.Add(sfdCC);

            // Go through each variable in order and copy them by producing the proper AST
            for(int i = 0; i < this.varDefs.Count; ++i)
            { 
                SynthVarValue svv = this.varDefs[i];    // The member to copy
                
                if(svv.type.intrinsic == true)
                {
                    AST astSrcDeref = new AST(new Token(-1, svv.varName, TokenType.tyWord), scb, ASTOp.DerefName, null, svv.type, false, AST.DataManifest.Procedural);
                    AST astSrcGetMember = new AST(new Token(), scb, ASTOp.GetMemberVar, svvSrc, this, false, AST.DataManifest.Procedural, astSrcDeref);

                    AST astDstDeref = new AST(new Token(-1, svv.varName, TokenType.tyWord), scb, ASTOp.DerefName, null, svv.type, false, AST.DataManifest.Procedural);
                    AST astDstGetMember = new AST(new Token(), scb, ASTOp.GetMemberVar, svvSrc, this, false, AST.DataManifest.Procedural, astSrcDeref);

                    AST astSetVar = new AST(new Token(), scb, ASTOp.SetValue, null, null, false, AST.DataManifest.NoData, astSrcDeref, astSrcGetMember);
                    sfdCC.ast.branches.Add(astSetVar);
                }
                else
                {
                    // Else, it's a sub-struct, and we use GetCopyConstructor() recursively
                    // to copy it.
                    throw new SynthExceptionCompile("Class copy constructors are not supported.");
                }
            }

            return sfdCC;
        }
    }
}
