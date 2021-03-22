using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class WASMBuild
    {
        /// <summary>
        /// 
        /// </summary>
        public struct FunctionType
        { 
            public readonly int index;
            public readonly WASM.Bin.TypeID retTy;
            public readonly List<WASM.Bin.TypeID> paramTys;

            public FunctionType(int index, WASM.Bin.TypeID retTy, List<WASM.Bin.TypeID> paramTys)
            { 
                this.index      = index;
                this.retTy      = retTy;
                this.paramTys   = paramTys;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        public class FunctionInfo
        { 
            public readonly int typeIndex;
            public readonly SynthFuncDecl function;
            public uint functionIndex;
            public bool imported;

            public FunctionInfo(SynthFuncDecl function, int typeIndex, bool imported)
            { 
                this.function = function;
                this.typeIndex = typeIndex;
                this.functionIndex = uint.MaxValue; // Calculated later
                this.imported = imported;
            }
        }

        public class WASMSection
        { 
            public int sectioncode;
            public byte [] payload;
        }

        // TODO: Everything's public for now, but we'll probably need to think about
        // better encapsulation in a future refactor.
        // (wleu 03/16/2021)

        public List<FunctionType> functionTypes = new List<FunctionType>();

        public List<FunctionInfo> functionInfos = new List<FunctionInfo>();
        public Dictionary<SynthFuncDecl, FunctionInfo> functionLookup = new Dictionary<SynthFuncDecl, FunctionInfo>();

        public SynthContext rootContext;

        public SynthStringRepo stringRepo = new SynthStringRepo();

        public WASMBuild(SynthContext rootContext)
        { 
            this.rootContext = rootContext;
        }

        public FunctionType TurnFnTypeIntoWASMType(SynthFuncDecl fnd)
        {
            WASM.Bin.TypeID retTy = GetTrueParamReturnType(fnd.returnType);
            List<WASM.Bin.TypeID> paramTy = new List<WASM.Bin.TypeID>();
            for(int i = 0; i < fnd.paramList.Count; ++i)
                paramTy.Add(GetTrueParamReturnType(fnd.paramList[i].type));

            for(int i = 0; i < this.functionTypes.Count; ++i)
            {
                FunctionType ftCanid = this.functionTypes[i];
                if (ftCanid.retTy != retTy)
                    continue;

                if(paramTy.Count != ftCanid.paramTys.Count)
                    continue;

                bool match = true;
                for(int j = 0; j < paramTy.Count; ++j)
                { 
                    if(paramTy[j] != ftCanid.paramTys[j])
                    { 
                        match = false;
                        break;
                    }
                }
                if(match == true)
                    return ftCanid;
            }

            FunctionType fnNew = new FunctionType(this.functionTypes.Count, retTy, paramTy);
            this.functionTypes.Add(fnNew);

            return fnNew;

        }

        WASM.Bin.TypeID GetTrueParamReturnType(SynthType st)
        {
            if(st == null)
                return WASM.Bin.TypeID.Empty;

            if (st.intrinsic == true)
            {
                switch (st.typeName)
                {
                    case "uint8":
                    case "int8":
                    case "uint16":
                    case "int16":
                    case "uint":
                    case "int":
                        return WASM.Bin.TypeID.Int32;

                    case "uint64":
                    case "int64":
                        return WASM.Bin.TypeID.Int64;

                    case "float":
                        return WASM.Bin.TypeID.Float32;

                    case "double":
                        return WASM.Bin.TypeID.Float64;

                }
            }
            else
            {
                // Return pointer back from memory stack
                return WASM.Bin.TypeID.Int32;
            }

            throw new SynthExceptionImpossible("Could not convert synth type to true WASM type.");
        }

        public int IndexFnTypeIntoWASMType(SynthFuncDecl fnd)
        { 
            return TurnFnTypeIntoWASMType(fnd).index;
        }

        public void RegisterFunction(SynthFuncDecl fn)
        {
            SynthLog.Log($"Registering function {fn.functionName}.");

            if(this.functionLookup.ContainsKey(fn) == true)
                throw new SynthExceptionImpossible("");

            // Register the type, and get the type index
            FunctionType fty = TurnFnTypeIntoWASMType(fn);

            FunctionInfo finfo = new FunctionInfo(fn, fty.index, fn.isExtern);
            this.functionInfos.Add(finfo);
            this.functionLookup.Add(fn, finfo);
        }

        public void RealignFunctions()
        {
            SynthLog.Log($"Realigning function indices.");

            List<FunctionInfo> origFnInfos = this.functionInfos;

            this.functionInfos = new List<FunctionInfo>();

            uint idx = 0;
            foreach(FunctionInfo fi in origFnInfos)
            { 
                if(fi.imported != true)
                    continue;

                fi.functionIndex = idx;
                ++idx;

                this.functionInfos.Add(fi);
            }

            foreach(FunctionInfo fi in origFnInfos)
            { 
                if(fi.imported == true)
                    continue;

                fi.functionIndex = idx;
                ++idx;

                this.functionInfos.Add(fi);
            }
        }

        public List<FunctionInfo> GetRangeImportFunctions()
        { 
            int idx = 0;
            if(this.functionInfos.Count == 0)
                return new List<FunctionInfo>();

            for(idx = 0; idx < this.functionInfos.Count; ++idx)
            { 
                if(this.functionInfos[idx].imported == false)
                    break;
            }

            return this.functionInfos.GetRange(0, idx);
        }

        public List<FunctionInfo> GetRangeNonImportFunctions()
        {
            int idx = 0;
            if (this.functionInfos.Count == 0)
                return new List<FunctionInfo>();

            for (idx = 0; idx < this.functionInfos.Count; ++idx)
            {
                if (this.functionInfos[idx].imported == false)
                    break;
            }

            return this.functionInfos.GetRange(idx, this.functionInfos.Count - idx);
        }

        public uint ? GetFunctionIndex(SynthFuncDecl fn)
        { 
            FunctionInfo fi;
            if(this.functionLookup.TryGetValue(fn, out fi) == false)
                return null;

            return fi.functionIndex;
        }
    }
}
