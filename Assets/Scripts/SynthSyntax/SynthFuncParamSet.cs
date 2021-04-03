using System.Collections.Generic;

namespace PxPre.SynthSyn
{
    /// <summary>
    /// Utility function to manage the parameters from a SynthFuncDecl.
    /// </summary>
    public class SynthFuncParamSet
    {
        public List<SynthVarValue> paramList = new List<SynthVarValue>();
        public List<ValueRef> paramRefs = new List<ValueRef>();
        public Dictionary<string, SynthVarValue> paramLookup = new Dictionary<string, SynthVarValue>();

        public int Count { get => this.paramList.Count; }

        int totalLocalIndices = 0;
        int totalMemStackBytes = 0;

        public int TotalLocalIndices {get => this.totalLocalIndices; }
        public int TotalMemStackByte {get => this.totalMemStackBytes; }

        public void AddThisParam(SynType styThis)
        {
            if (styThis == null)
                throw new SynthExceptionImpossible("No parent struct type to define 'this' for struct function.");

            if(this.paramList.Count != 0)
                throw new SynthExceptionImpossible("Cannot add 'this' parameter to a function that already has parameters.");

            SynthVarValue svvThis = new SynthVarValue();
            svvThis.type = styThis;
            svvThis.typeName = styThis.typeName;
            svvThis.varName = "this";
            svvThis.varLoc = SynthVarValue.VarLocation.ThisRef;

            this.AddParameter(svvThis);
        }

        public void AddParameter(SynthVarValue svvParam)
        {
            this.paramList.Add(svvParam);
            this.paramLookup.Add(svvParam.varName, svvParam);
            
            if(svvParam.varLoc == SynthVarValue.VarLocation.ThisRef)
            { 
                if(this.paramList.Count != 1)
                    throw new SynthExceptionImpossible("'this' parameter added in an illegal location.");
            }
        }

        public void _Validate(SynthFuncDecl sfd)
        {
            if(sfd.isStatic == false)
            { 
                if(
                    this.paramList.Count == 0 || 
                    this.paramList[0].varName != "this" ||
                    this.paramList[0].varLoc != SynthVarValue.VarLocation.ThisRef)
                { 
                    throw new SynthExceptionImpossible($"Member function {sfd.functionName} does not have a proper 'this' as the first parameter.");
                }
            }

            if(this.paramList.Count != this.paramLookup.Count)
                throw new SynthExceptionImpossible($"Function {sfd.functionName} parameters expected {this.paramList.Count} parameter lookups, but got {this.paramLookup.Count}");

            // this.paramRefs.Count isn't generated yet.
        }

        public SynthVarValue Get(string name)
        {
            SynthVarValue svv;
            this.paramLookup.TryGetValue(name, out svv);
            return svv;
        }

        public SynthVarValue Get(int idx)
        { 
            return this.paramList[idx];
        }

        public ValueRef GetRef(SynthVarValue svv)
        {
            for (int i = 0; i < this.paramList.Count; ++i)
            {
                if(this.paramList[i] == svv)
                    return GetRef(i);
            }
            return null;
        }

        public ValueRef GetRef(int idx)
        { 
            return this.paramRefs[idx];
        }

        public bool ExactlyMatches(IReadOnlyList<SynthVarValue> cmpAgainst, bool ignoreFirst)
        {
            if(ignoreFirst == true)
            {
                if(this.paramList.Count - 1 != cmpAgainst.Count)
                    return false;

                for (int i = 0; i < this.paramList.Count; ++i)
                {
                    if (this.paramList[i + 1].typeName != cmpAgainst[i].typeName)
                        return false;
                }
            }
            else
            {
                if(this.paramList.Count != cmpAgainst.Count)
                    return false;

                for(int i = 0; i < this.paramList.Count; ++i)
                { 
                    if(this.paramList[i].typeName != cmpAgainst[i].typeName)
                        return false;
                }
            }

            return true;
        }

        public bool ExactlyMatches(SynthFuncParamSet otherParams)
        { 
            return this.ExactlyMatches(otherParams.paramList, false);
        }

        public void PostTypeAlignment(SynthFuncDecl sty)
        {
            if(this.totalMemStackBytes != 0 )
                throw new SynthExceptionImpossible("Calculating total memstack size, but the counter has already been touched.");

            if(this.totalLocalIndices != 0 )
                throw new SynthExceptionImpossible("Calculating total local index alloc, but the counter has already been touched.");

            // Fill up this.paramRefs
            for (int i = 0; i < this.paramList.Count; ++i)
            {
                SynthVarValue svvParam = this.paramList[i];

                if (svvParam.varLoc == SynthVarValue.VarLocation.ThisRef)
                {
                    if (i != 0)
                        throw new SynthExceptionImpossible("'this' parameter added in an illegal location.");

                    // Not 100% sure this is the right location type 
                    // (wleu 03/30/2021)
                    ValueRef vr = new ValueRef(ValueLoc.PointerOnStack, 0, -1, svvParam.type, 1);
                    this.paramRefs.Add(vr);
                    ++this.totalLocalIndices;
                }
                else if (svvParam.varLoc != SynthVarValue.VarLocation.Parameter)
                    throw new SynthExceptionImpossible("Parameter variable that was attempted to be added wasn't constructed as a parameter.");
                else if (svvParam.type.intrinsic == true)
                {
                    ValueRef vr = new ValueRef(ValueLoc.LocalIdx, this.totalLocalIndices, -1, svvParam.type, 0);
                    this.paramRefs.Add(vr);
                    ++this.totalLocalIndices;
                }
                else
                {
                    ValueRef vr = new ValueRef(ValueLoc.ValueOnMemStack, -1, this.totalMemStackBytes, svvParam.type, 0);
                    this.paramRefs.Add(vr);
                    this.totalMemStackBytes += svvParam.type.GetByteSize();
                }
            }

            // Tranfer local ids and memory offsets to the global offset. The local
            // and global (prefixed fn*) will be the same.
            foreach(ValueRef vr in this.paramRefs)
            { 
                vr.fnIdx = vr.idx;
                vr.fnByteAlign = vr.byteAlign;
            }

            // Validation of default parameters.
            bool startedDefaultSection = false;
            foreach (SynthVarValue svv in this.paramList)
            {
                if (svv.varName == "this")
                    continue;

                if (svv.declPhrase.Count < 2)
                    throw new SynthExceptionImpossible($"Parameter {svv.varName} for function {sty.functionName} found with less than 2 tokens.");

                if (svv.declPhrase.Count > 2)
                {
                    // TODO: Hardcoded 2
                    if (svv.declPhrase[2].Matches(TokenType.tySymbol, "=") == false)
                        throw new SynthExceptionSyntax(svv.declPhrase[2], "Unexpected addition to parameter declaration on line.");

                    startedDefaultSection = true;
                }
                else if (startedDefaultSection == true)
                {
                    throw new SynthExceptionSyntax(svv.declPhrase[1], "All parameters after the first default parameter must also have default parameters.");
                }
            }
        }

        /// <summary>
        /// Check if the first parameter of the parameter list is the 'this' parameter.
        /// </summary>
        /// <returns></returns>
        public bool HasThis()
        {
            return 
                this.paramList.Count >= 1 && 
                this.paramList[0].varLoc == SynthVarValue.VarLocation.ThisRef;
        }
    }
}