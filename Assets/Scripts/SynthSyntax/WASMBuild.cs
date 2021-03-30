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

        public readonly int stackMemByteCt = 1024;

        public WASMBuild(SynthContext rootContext, int stackMemByteCt = 1024)
        { 
            this.rootContext = rootContext;
            this.stackMemByteCt = stackMemByteCt;
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

        public int GetOrAddFunctionType(WASM.Bin.TypeID retTy, List<WASM.Bin.TypeID> paramTys)
        { 
            foreach(FunctionType fty in this.functionTypes)
            { 
                if(retTy != fty.retTy)
                    continue;

                if(fty.paramTys.Count != paramTys.Count)
                    continue;

                bool matches = true;
                for(int i = 0; i < paramTys.Count; ++i)
                { 
                    if(paramTys[i] != fty.paramTys[i])
                    { 
                        matches = false;
                        break;
                    }
                }

                if(matches == false)
                    continue;

                return fty.index;
            }

            // We didn't find a match, so we need to add the entry in.
            FunctionType ftyNew = new FunctionType(this.functionTypes.Count, retTy, paramTys);
            this.functionTypes.Add(ftyNew);

            return ftyNew.index;
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

        public byte[] BuildWASM()
        {
            SynthLog.LogHeader("Entered BuildWASM() from SynthContext.cs");

            //WASMBuild build = new WASMBuild(this);

            // Gather all the functions in the entire build, and make a collection
            // of their unique types.
            this.rootContext.GatherFunctionRegistration(this);
            // Reorganize function indices so imported functions come first. This is
            // required for WASM.
            this.RealignFunctions();

            List<byte> fileContent = new List<byte>();
            fileContent.AddRange(System.BitConverter.GetBytes(WASM.BinParse.WASM_BINARY_MAGIC));
            fileContent.AddRange(System.BitConverter.GetBytes(1));

            // TODO: Consider removing the WASMSection class (in another file)


            foreach(FunctionInfo fi in this.functionInfos)
            { 
                if(fi.function.isExtern == true)
                    continue;

                this.BuildFunction(fi.function);
            }
            //foreach (var kvp in this.rootContext.Functions)
            //{
            //    List<SynthFuncDecl> lst = kvp.Value;
            //    foreach (SynthFuncDecl sfd in lst)
            //    {
            //        if (sfd.isExtern == true)
            //            continue;
            //
            //        this.BuildFunction(sfd);
            //    }
            //}

            //
            //      FUNCTION TYPE DECLARATIONS
            //      "Type"s
            //////////////////////////////////////////////////

            int startFnType = this.GetOrAddFunctionType(WASM.Bin.TypeID.Result, new List<WASM.Bin.TypeID>());
            fileContent.Add((byte)WASM.Bin.Section.TypeSec);
            {
                List<byte> typeSection = new List<byte>();

                // Function type count
                // + 1 for the start function
                typeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)functionTypes.Count));

                for (int i = 0; i < this.functionTypes.Count; ++i)
                {
                    FunctionType fty = functionTypes[i];

                    // Function tag
                    typeSection.Add((byte)WASM.Bin.TypeID.Function);

                    // Param count
                    typeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)fty.paramTys.Count));
                    // Param values
                    for (int j = 0; j < fty.paramTys.Count; ++j)
                    {
                        // Types will never be big enough to need LEB encoding. Single byte is fine.
                        typeSection.Add((byte)fty.paramTys[j]);
                    }

                    // Only 1 return value max will ever be returned.
                    if (fty.retTy == WASM.Bin.TypeID.Empty)
                        typeSection.Add(0);
                    else
                    {
                        typeSection.Add(1);
                        typeSection.Add((byte)fty.retTy);
                    }
                }

                fileContent.AddRange(WASM.BinParse.EncodeSignedLEB(typeSection.Count));
                fileContent.AddRange(typeSection);
            }

            //
            //      IMPORTED FUNCTION DECLARATIONS
            //      "Import"s
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.ImportSec);
            {
                List<byte> importSection = new List<byte>();

                List<FunctionInfo> lstImported = GetRangeImportFunctions();

                // Function count
                importSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstImported.Count));

                for (int i = 0; i < lstImported.Count; ++i)
                {
                    string env = "ImportedFns";
                    string field = lstImported[i].function.functionName;

                    importSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)env.Length));
                    importSection.AddRange(System.Text.Encoding.ASCII.GetBytes(env));
                    //
                    importSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)field.Length));
                    importSection.AddRange(System.Text.Encoding.ASCII.GetBytes(field));

                    importSection.Add(0); // Import kind

                    importSection.AddRange(WASM.BinParse.EncodeSignedLEB((uint)lstImported[i].typeIndex));
                }

                fileContent.AddRange(WASM.BinParse.EncodeSignedLEB(importSection.Count));
                fileContent.AddRange(importSection);
            }

            //
            //      LOCAL FUNCTION DECLARACTIONS
            //      "Function"s
            //////////////////////////////////////////////////
            List<FunctionInfo> lstLocalFns = GetRangeNonImportFunctions();
            fileContent.Add((byte)PxPre.WASM.Bin.Section.FunctionSec);
            {
                List<byte> functionSection = new List<byte>();

                // Function Count
                functionSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstLocalFns.Count + 1));

                for (int i = 0; i < lstLocalFns.Count; ++i)
                    functionSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstLocalFns[i].typeIndex));

                functionSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)startFnType));

                fileContent.AddRange(WASM.BinParse.EncodeSignedLEB(functionSection.Count));
                fileContent.AddRange(functionSection);
            }

            int startIdx = -1; // The start index function
            startIdx = functionInfos.Count;

            //
            //      TABLE DECLARACTIONS
            //      "Table"s
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.TableSec);
            {
                List<byte> tableSections = new List<byte>();

                tableSections.AddRange(WASM.BinParse.EncodeUnsignedLEB(0));

                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)tableSections.Count));
                fileContent.AddRange(tableSections);
            }

            //
            //      MEMORY DECLARACTIONS
            //      "Memory"s
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.MemorySec);
            {
                List<byte> memorySection = new List<byte>();

                memorySection.AddRange(WASM.BinParse.EncodeUnsignedLEB(1));
                memorySection.Add(0); // flags
                memorySection.Add(2); // initial page ct


                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)memorySection.Count));
                fileContent.AddRange(memorySection);
            }

            //
            //      --
            //      "Globals"s
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.GlobalSec);
            {
                List<byte> globalsSection = new List<byte>();

                globalsSection.AddRange(WASM.BinParse.EncodeUnsignedLEB(1));
                //
                globalsSection.Add((byte)WASM.Bin.TypeID.Int32); // Type
                globalsSection.Add(0); // Not mutable
                globalsSection.Add((byte)WASM.Instruction.i32_const);
                globalsSection.AddRange(WASM.BinParse.EncodeSignedLEB(stackMemByteCt));
                globalsSection.Add((byte)WASM.Instruction.end);

                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)globalsSection.Count));
                fileContent.AddRange(globalsSection);
            }

            //      --
            //      "Export"s
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.ExportSec);
            {
                List<byte> exportSection = new List<byte>();

                uint exportedCt = 0;
                for (int i = 0; i < lstLocalFns.Count; ++i)
                {
                    if (lstLocalFns[i].function.isStatic == false || lstLocalFns[i].function.isEntry == false)
                        continue;

                    ++exportedCt;
                }

                ++exportedCt; // +1 for global MemStkSize
                exportSection.AddRange(WASM.BinParse.EncodeUnsignedLEB(exportedCt));

                // Iterate through it again the same way, but actually save out instead of count this time.
                for (int i = 0; i < lstLocalFns.Count; ++i)
                {
                    if (lstLocalFns[i].function.isStatic == false || lstLocalFns[i].function.isEntry == false)
                        continue;

                    exportSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstLocalFns[i].function.functionName.Length));
                    exportSection.AddRange(System.Text.Encoding.ASCII.GetBytes(lstLocalFns[i].function.functionName));
                    exportSection.Add(0); // kind
                    exportSection.AddRange(WASM.BinParse.EncodeUnsignedLEB(lstLocalFns[i].functionIndex));
                }

                // Add export of MemStkSize
                exportSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)"MemStkSize".Length));
                exportSection.AddRange(System.Text.Encoding.ASCII.GetBytes("MemStkSize"));
                exportSection.Add(3); // kind - TODO: Does PreWASM have this defined somewhere?
                exportSection.Add(0); // Global index

                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)exportSection.Count));
                fileContent.AddRange(exportSection);
            }

            //
            //      --
            //      "Start"
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.StartSec);
            {
                List<byte> startSection = new List<byte>();

                startSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)startIdx));


                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)startSection.Count));
                fileContent.AddRange(startSection);
            }

            //
            //      --
            //      "Elems"
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.ElementSec);
            {
                List<byte> elemsSection = new List<byte>();

                elemsSection.AddRange(WASM.BinParse.EncodeUnsignedLEB(0));

                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)elemsSection.Count));
                fileContent.AddRange(elemsSection);
            }

            //
            //      --
            //      "Code"
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.CodeSec);
            {
                List<byte> codeSection = new List<byte>();

                // Num functions
                codeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstLocalFns.Count + 1));

                for (int i = 0; i < lstLocalFns.Count; ++i)
                {
                    FunctionInfo finfo = lstLocalFns[i];
                    SynthFuncDecl fn = finfo.function;

                    if (fn.fnBin == null)
                        throw new SynthExceptionImpossible($"Attempting to save out WASM of function {fn.functionName} that hasn't been processed.");

                    // Function size
                    codeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)fn.fnBin.Length));
                    // Locals byte size
                    codeSection.AddRange(fn.fnBin);
                }

                WASMByteBuilder startFn = new WASMByteBuilder();
                startFn.AddLEB128(0); // Local declarations
                // Set the starting stack position (4) in memory. This is basically the stack position, but
                // we reserve the first 4 bytes for the stack information.
                startFn.Add_I32Const(0);
                startFn.Add_I32Const(4);
                startFn.Add_I32Store();
                startFn.Add_End();
                //
                codeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)startFn.BinCount()));
                codeSection.AddRange(startFn.Bin());

                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)codeSection.Count));
                fileContent.AddRange(codeSection);
            }

            //
            //      --
            //      "Data"
            //////////////////////////////////////////////////
            fileContent.Add((byte)WASM.Bin.Section.DataSec);
            {
                List<byte> dataSection = new List<byte>();

                dataSection.AddRange(WASM.BinParse.EncodeUnsignedLEB(0));

                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)dataSection.Count));
                fileContent.AddRange(dataSection);
            }

            return fileContent.ToArray();
        }

        // TODO: Perhaps this function should be moved into WASMBuild?
        public SynthContextBuilder BuildFunction(SynthFuncDecl fnd)
        {
            SynthContextBuilder builder = new SynthContextBuilder(null);

            if (fnd.ast != null)
                throw new SynthExceptionImpossible($"Attempting to build function {fnd.functionName} AST multiple times.");

            SynthLog.Log($"Entered SynthFuncDecl.Build() for {fnd.functionName}.");

            // NOTE: For now functions are non-typed (in the SynthSyn language) and 
            // are non addressable.
            fnd.ast =
                new TokenAST(
                    fnd.declPhrase[0],
                    builder,
                    TokenASTType.FunctionDecl,
                    fnd,
                    null,
                    false,
                    TokenAST.DataManifest.NoData);

            SynthLog.Log($"Building function AST for {fnd.functionName}.");
            SynthLog.Log("");

            List<SynthContextBuilder> builders = new List<SynthContextBuilder>();
            builders.Add(builder);

            //List<TokenTree> treeLines = new List<TokenTree>();
            for (int i = 0; i < fnd.executingLines.Count; ++i)
            {
                List<Token> execLine = fnd.executingLines[i];
                SynthLog.LogFragments(execLine);

                TokenTree rootLineNode = TokenTree.EatTokensIntoTree(execLine, fnd, true);
                //treeLines.Add(rootLineNode);

                // If it's a member function (not a static function) then full in the struct
                // we belong to as a he invoking scope. Or else set it to null. Its syntax scope
                // it still all the way where the source code is, but doesn't have a "this" member
                // function.
                SynthScope invokingScope = null;
                if (fnd.isStatic == false)
                    invokingScope = fnd.GetStructScope();

                TokenAST exprAST = builder.ProcessFunctionExpression(builders, this, invokingScope, fnd, rootLineNode);
                if (exprAST == null)
                {
                    // We shouldn't have received null, we should have thrown before this
                    throw new SynthExceptionImpossible(""); //TODO:
                }
                fnd.ast.branches.Add(exprAST);
            }

            SynthLog.Log("");
            SynthLog.Log($"Encountered {builders.Count} nested scopes.");
            for(int i = 0; i < builders.Count; ++i)
            {
                SynthLog.LogIndent(0, $"Scope {i}");
                SynthLog.LogIndent(1, $"Line number : {builders[i].lineNumber}");
                SynthLog.LogIndent(1, $"Stack Elements : Ele {builders[i].locStkEle.Count} - Vars {builders[i].locStkVars.Count}");
                SynthLog.LogIndent(1, $"Memory Stack : Ele {builders[i].memStkEle.Count} - Vars {builders[i].memStkVars.Count}");
                SynthLog.LogIndent(1, $"Total Memory Stack : {builders[i].totalMemoryStack} - Total Memory Stack Bytes : {builders[i].totalMemoryStackBytes}");
            }

            SynthLog.Log("");
            SynthLog.Log(fnd.ast.DumpDiagnostic());

            SynthLog.Log($"Finished building AST : {fnd.functionName}.");
            SynthLog.Log($"Converting AST to binary WASM : {fnd.functionName}.");

            // Before the AST is turned into actual WASM binary, we need to finalize the
            // byte alignment and the indices of local stack variables. This is done with
            // CompileAlignment, who's lower scopes should appear earlier in the builders
            // list before higher children scopes.
            foreach (SynthContextBuilder b in builders)
                b.CompileAlignment();

            // Gather all the local variables and declare them. This isn't as efficient as things
            // could be because after variables go out of scope, their positions on the stack
            // can be reused if they match types we encounter in the future, but that can be 
            // handled later.

            // The program binary
            WASMByteBuilder fnBuild = new WASMByteBuilder();

            // Gather all the local variables
            List<WASM.Bin.TypeID> localVarTys = new List<WASM.Bin.TypeID>();
            foreach (SynthContextBuilder b in builders)
            {
                foreach (SynthContextBuilder.ValueRef stkVal in b.locStkEle)
                {
                    if (stkVal.varType.intrinsic == false)
                        throw new SynthExceptionCompile(""); // TODO: Error msg

                    switch (stkVal.varType.typeName)
                    {
                        case "bool":
                        case "int8":
                        case "uint8":
                        case "int16":
                        case "uint16":
                        case "int":
                        case "int32":
                            localVarTys.Add(WASM.Bin.TypeID.Int32);
                            break;

                        case "int64":
                        case "uint64":
                            localVarTys.Add(WASM.Bin.TypeID.Int64);
                            break;

                        case "float":
                            localVarTys.Add(WASM.Bin.TypeID.Float32);
                            break;

                        case "double":
                            localVarTys.Add(WASM.Bin.TypeID.Float64);
                            break;
                    }
                }
            }

            List<KeyValuePair<WASM.Bin.TypeID, int>> consolidatedLocalTys = new List<KeyValuePair<WASM.Bin.TypeID, int>>();
            for (int i = 0; i < localVarTys.Count; ++i)
            {
                WASM.Bin.TypeID tyid = localVarTys[i];
                int ct = 1;
                ++i;

                for (; i < localVarTys.Count; ++i)
                {
                    if (localVarTys[i] != tyid)
                        break;

                    ++ct;
                }
                consolidatedLocalTys.Add(new KeyValuePair<WASM.Bin.TypeID, int>(tyid, ct));
            }

            fnBuild.AddLEB128((uint)consolidatedLocalTys.Count); // Local decl
            for (int i = 0; i < consolidatedLocalTys.Count; ++i)
            {
                fnBuild.AddLEB128((uint)consolidatedLocalTys[i].Value);

                // These are really bytes, but they'll end up being added as bytes since they're
                // small constants
                fnBuild.AddLEB128((uint)consolidatedLocalTys[i].Key); 
            }

            builder.BuildBSFunction(fnd, fnd.ast, this, builder, fnBuild);
            fnBuild.Add_End();
            fnd.fnBin = fnBuild.Bin();

            SynthLog.Log($"Exiting SynthFuncDecl.Build({fnd.functionName}).");
            return builder;
        }
    }
}
