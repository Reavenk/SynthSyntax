using System.Collections;
using System.Collections.Generic;
using UnityEngine;


namespace PxPre.SynthSyn
{
    public class SynthScope : SynthObj
    {
        /// <summary>
        /// The state of progress for a set of things being processed.
        /// </summary>
        public enum TypeConsolidate
        { 
            /// <summary>
            /// All entries have been sucessfully processed.
            /// </summary>
            AllDetermined,

            /// <summary>
            /// Not all entries are sucessfully processed, but during the
            /// current processing pass, not progress was made.
            /// </summary>
            UndeterminedNoChange,

            /// <summary>
            /// Not all entries are successfully processed, but during the
            /// curent processing pass, one or more entries were successfully
            /// processed.
            /// </summary>
            UndeterminedProgress
        }

        public enum VarDst
        { 
            Local,
            Global
        }

        public enum OperatorReversing
        { 
            OnlyNonReversible,
            OnlyReversible,
            IgnoreReversible
        }

        protected SynthScope parentScope;

        public List<Token> declPhrase = new List<Token>();
    
        protected Dictionary<string, SynType> typesDefs = new Dictionary<string, SynType>();

        protected Dictionary<string, List<SynthFuncDecl>> functions = new Dictionary<string, List<SynthFuncDecl>>();
        public IReadOnlyDictionary<string, List<SynthFuncDecl>> Functions {get{return this.functions; } }

        // Either local variables of a function, or member variables.
        protected List<SynthVarValue> varDefs = new List<SynthVarValue>();
        protected Dictionary<string, SynthVarValue> varLookups = new Dictionary<string, SynthVarValue>();

        // Static/Global values
        protected List<SynthVarValue> globalDefs = new List<SynthVarValue>();
        protected Dictionary<string, SynthVarValue> globalLookups = new Dictionary<string, SynthVarValue>();

        protected Dictionary<string, SynthRegion> regions = new Dictionary<string, SynthRegion>();

        public int memoryStackSize = -1;

        public SynthScope(SynthScope parent)
        { 
            this.parentScope = parent;
        }

        public bool IsNameClaimed(string name, bool recursion = true)
        { 
            if(this.typesDefs.ContainsKey(name) == true)
                return true;

            if(this.varLookups.ContainsKey(name) == true)
                return true;

            if(recursion == true && this.parentScope != null)
                return this.parentScope.IsNameClaimed(name, true);

            return false;
        }

        public SynStruct ExtractVariableDecl(List<Token> tokens, int start, ref int end)
        { 
            // TODO: Remove?
            return null;
        }

        public SynthFuncDecl ExtracFunctionDecl(List<Token> tokens, int start, ref int end)
        {
            // TODO: Remove?
            return null;
        }

        public virtual SynthVarValue GetVar(string name, bool recursion = true)
        { 
            SynthVarValue ret;
            if(this.varLookups.TryGetValue(name, out ret) == true)
                return ret;

            if (recursion == true && this.parentScope != null)
                return this.parentScope.GetVar(name, true);

            return null;
        }

        public SynType GetType(string typename, bool recursion = true)
        { 
            SynType ret;
            if(this.typesDefs.TryGetValue(typename, out ret) == true)
                return ret;

            if(recursion == true && this.parentScope != null)
                return this.parentScope.GetType(typename, true);

            return null;
        }

        public List<SynthFuncDecl> GetFunction(string functionName, bool recursion = true)
        { 
            List<SynthFuncDecl> ret;
            if(this.functions.TryGetValue(functionName, out ret) == true)
                return ret;

            if(recursion == true && this.parentScope != null)
                return this.parentScope.GetFunction(functionName, true);

            return null;
        }

        /// <summary>
        /// Ensure the structs have byte alignments calculated, and that every
        /// member variable type that's referenced is known.
        /// </summary>
        /// <returns></returns>
        public virtual TypeConsolidate ResolveStaticTypeAlignments()
        { 
            // contained by value.
            int alreadyDet = 0;
            int newlyDet = 0;

            foreach(KeyValuePair<string, SynType> kvp in this.typesDefs)
            {
                TypeConsolidate tc = kvp.Value.ResolveStaticTypeAlignments();

                if(tc == TypeConsolidate.AllDetermined)
                    ++alreadyDet;
                else if(tc == TypeConsolidate.UndeterminedProgress)
                    ++newlyDet;
            }

            if(alreadyDet + newlyDet == this.typesDefs.Count)
                return TypeConsolidate.AllDetermined;
            else if(newlyDet > 0)
                return TypeConsolidate.UndeterminedProgress;
            else
                return TypeConsolidate.UndeterminedNoChange;

        }

        public void AddVariable(SynthVarValue var)
        {
            if(var.varLoc == SynthVarValue.VarLocation.Static)
                this.AddGlobalVar(var);
            else
                this.AddLocalVariable(var);
        }

        public SynthVarValue AddLocalVariable(string varName, SynType ty)
        {
            return this.AddVariable(varName, ty, VarDst.Local, SynthVarValue.VarLocation.Local);
        }

        public SynthVarValue AddLocalVariable(string varName, string tyName)
        {
            return this.AddVariable(varName, tyName, VarDst.Local, SynthVarValue.VarLocation.Local);
        }

        public void AddLocalVariable(SynthVarValue var)
        {
            this.AddVariable(var, VarDst.Local);
        }

        public SynthVarValue AddGlobalVar(string varName, SynType ty)
        {
            return this.AddVariable(varName, ty, VarDst.Global, SynthVarValue.VarLocation.Static);
        }

        public SynthVarValue AddGlobalVar(string varName, string tyName)
        {
            return this.AddVariable(varName, tyName, VarDst.Global, SynthVarValue.VarLocation.Static);
        }

        public void AddGlobalVar(SynthVarValue var)
        { 
            this.AddVariable(var, VarDst.Global);
        }

        public SynthVarValue AddVariable(string varName, SynType ty, VarDst dst, SynthVarValue.VarLocation varLoc)
        {
            SynthVarValue newVar = new SynthVarValue();
            newVar.varLoc = varLoc;
            newVar.varName = varName;
            newVar.typeName = ty.typeName;
            newVar.type = ty;

            this.AddVariable(newVar, dst);
            return newVar;
        }

        public SynthVarValue AddVariable(string varName, string tyName, VarDst dst, SynthVarValue.VarLocation varLoc)
        {
            SynthVarValue newVar = new SynthVarValue();
            newVar.varLoc = varLoc;
            newVar.varName = varName;
            newVar.typeName = tyName;

            this.AddVariable(newVar, dst);
            return newVar;
        }

        public void AddVariable(SynthVarValue var, VarDst dst)
        {
            if(dst == VarDst.Global)
            {
                this.globalLookups.Add(var.varName, var);
                this.globalDefs.Add(var);
            }
            else if(dst == VarDst.Local)
            { 
                this.varLookups.Add(var.varName, var);
                this.varDefs.Add(var);
            }
            else 
                throw new SynthExceptionImpossible("Attemping to add variable to unknown destination");
        }

        public void AddFunction(SynthFuncDecl fnAdding)
        {
            List<SynthFuncDecl> fns;
            if(this.functions.TryGetValue(fnAdding.functionName, out fns) == false)
            { 
                fns = new List<SynthFuncDecl>();
                this.functions.Add(fnAdding.functionName, fns);
            }
            else 
            { 
                if(fnAdding.isDestructor == true)
                { 
                    foreach(SynthFuncDecl already in fns)
                    { 
                        if(already.isDestructor == true)
                            throw new SynthExceptionSyntax(fnAdding.declPhrase[0], "Attempting to declare multiple destructors.");
                    }
                }
                else
                {
                    // Check if there's a signature collision with function overloads
                    foreach(SynthFuncDecl already in fns)
                    { 
                        if(already.parameterSet.ExactlyMatches(fnAdding.parameterSet) == true)
                                throw new System.Exception($"Function declaration {fnAdding.functionName} already included");
                    }
                }
            }

            fns.Add(fnAdding);
        }

        public void AddType(SynType type)
        { 
            this.typesDefs.Add(type.typeName, type);
        }

        public virtual void BreakApartParsedTokens() {}

        /// <summary>
        /// Add all globals to the regsitry for proper alignment. Happens have all types
        /// known have been aligned.
        /// </summary>
        /// <param name="globalsRegistry">The registry to add globals to</param>
        public virtual void RegisterGlobals(List<SynthVarValue> globalsRegistry)
        { 
            globalsRegistry.AddRange(this.globalDefs);
        }

        /// <summary>
        /// Do validation and further processing after types, and function, have been validated
        /// and type have been aligned.
        /// </summary>
        public virtual void Validate_AfterTypeAlignment(int logIndent)
        {
            SynthLog.LogIndent(logIndent, "Started base.Validate_AfterTypeAlignment");

            foreach (SynthScope ss in this.EnumerateScopes())
                ss.Validate_AfterTypeAlignment(logIndent + 1);

            SynthLog.LogIndent(logIndent, "Ended base.Validate_AfterTypeAlignment");
        }

        public virtual IEnumerable<SynthScope> EnumerateScopes()
        {
            foreach( var v in this.typesDefs)
                yield return v.Value;

            foreach(var v in this.functions)
            {
                foreach(var w in v.Value)
                    yield return w;
            }

            foreach(var v in this.regions)
                yield return v.Value;
        }

        public IEnumerable<SynthFuncDecl> EnumerateScopedFunctions()
        { 
            foreach(var v in this.functions)
            { 
                foreach(var w in v.Value)
                    yield return w;
            }
        }

        public virtual SynthContext GetRootContext()
        { 
            if(this.parentScope == null)
                return null;

            return this.parentScope.GetRootContext();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="opName"></param>
        /// <param name="otherType"></param>
        /// <param name="reversible">If true, only reversible functions are </param>
        /// <returns></returns>
        public SynthFuncDecl GetOperator(string opName, SynType otherType, OperatorReversing revMode)
        { 
            string fullName = "operator" + opName;

            List<SynthFuncDecl> fns;
            if(this.functions.TryGetValue(fullName, out fns) == false)
                return null;

            foreach(SynthFuncDecl sfc in fns)
            { 
                if(sfc.parameterSet.Count != 1 || sfc.isStatic == true)
                    continue;

                switch(revMode)
                { 
                    case OperatorReversing.OnlyNonReversible:
                        if (sfc.isReversible == true)
                            continue;
                        break;

                    case OperatorReversing.OnlyReversible:
                        if(sfc.isReversible == false)
                            continue;
                        break;
                }

                if(sfc.parameterSet.Get(0).type == otherType)
                    return sfc;
            }

            return null;
        }

        public virtual void GatherFunctionRegistration(WASMBuild builds)
        {
            foreach(var v in this.functions)
            {
                foreach(var w in v.Value)
                    w.GatherFunctionRegistration(builds);
            }

            foreach(var v in this.typesDefs)
                v.Value.GatherFunctionRegistration(builds);

            foreach(var v in this.regions)
                v.Value.GatherFunctionRegistration(builds);
        }

        public SynthCanidateFunctions GetCanidateFunctions(string functionName)
        {
            SynthCanidateFunctions canFns = new SynthCanidateFunctions(this);
            this.FillFunctionList(functionName, this, canFns.functions);
            return canFns;
        }

        public List<SynthFuncDecl> GetFunctionList(string functionName, SynthScope scope)
        {
            List<SynthFuncDecl> ret = new List<SynthFuncDecl>();
            this.FillFunctionList(functionName, scope, ret);
            return ret;
        }

        protected void FillFunctionList(string functionName, SynthScope scope, List<SynthFuncDecl> lst)
        {
            foreach (var v in this.functions)
            {
                if(v.Key != functionName)
                    continue;

                foreach(SynthFuncDecl sfn in v.Value)
                {
                    if (sfn.functionName != functionName)
                        continue;

                    if(scope == this || sfn.isStatic)
                        lst.Add(sfn);
                }
            }

            if (this.parentScope != null)
                this.parentScope.FillFunctionList(functionName, scope, lst);
        }

        public virtual SynStruct GetStructScope()
        { 
            if(this.parentScope == null)
                return null;

            return this.parentScope.GetStructScope();
        }
    }
}
