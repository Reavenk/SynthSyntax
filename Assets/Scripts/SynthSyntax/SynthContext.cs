using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthContext : SynthScope
    {
        public enum Type
        { 
            tyMain,
            tyScope,
            tyStruct
        }

        int totalGlobalBytes = -1;

        public SynthContext()
            : base(null)
        { 
            this.RegisterType(new SynthType_Intrinsic(this, "bool",   1));
            this.RegisterType(new SynthType_Intrinsic(this, "int",    4));
            this.RegisterType(new SynthType_Intrinsic(this, "uint",   4));
            this.RegisterType(new SynthType_Intrinsic(this, "int64",  8));
            this.RegisterType(new SynthType_Intrinsic(this, "uint64", 8));
            this.RegisterType(new SynthType_Intrinsic(this, "int8",   1));
            this.RegisterType(new SynthType_Intrinsic(this, "uint8",  1));
            this.RegisterType(new SynthType_Intrinsic(this, "int16",  2));
            this.RegisterType(new SynthType_Intrinsic(this, "uint16", 2));
            this.RegisterType(new SynthType_Intrinsic(this, "float",  4));
            this.RegisterType(new SynthType_Intrinsic(this, "float64",8));
        }

        public void RegisterType(SynthType sty)
        { 
            if(this.typesDefs.ContainsKey(sty.typeName) == true)
                throw new System.Exception("Attempting to register typename that is already assigned.");

            this.typesDefs.Add(sty.typeName, sty);
        }

        public void ParseContext(List<Token> tokens)
        {
            SynthLog.LogHeader("Starting Parse Content");
            SynthLog.Log("\tTokens:");
            SynthLog.Log(tokens);

            //      Get param globals first
            //
            //////////////////////////////////////////////////
            while (tokens.Count > 0)
            {
                SynthVarValue parsedParam = SynthVarValue.ParseExposedParam(tokens);
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
                    SynthType_Struct sst = SynthType_Struct.Parse(this, tokens);
                    if (sst != null)
                    {
                        this.AddType(sst);
                        continue;
                    }
                }

                // Function parsing
                if(tokens[0].Matches(TokenType.tyWord, "entry") == true)
                { 
                    SynthFuncDecl sfd = SynthFuncDecl.Parse(this, tokens, "", true, SynthFuncDecl.ParseType.Entry);
                    sfd.isEntry = true;

                    if (sfd != null)
                    {
                        this.AddFunction(sfd);
                        continue;
                    }
                    else
                        throw new System.Exception("entry keyword not part of valid function.");
                }

                SynthFuncDecl synthFn = 
                    SynthFuncDecl.Parse(
                        this, tokens, 
                        "", 
                        true, 
                        SynthFuncDecl.ParseType.RootContext);

                if(synthFn != null)
                { 
                    this.AddFunction(synthFn);
                    continue;
                }

                SynthVarValue synthVar = SynthVarValue.ParseBodyVar(tokens, SynthVarValue.OuterScope.Global);
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
            SynthLog.LogHeader("Verifying types");

            while (true)
            { 
                TypeConsolidate tc = this.ResolveStaticTypeAlignments();
                if(tc == TypeConsolidate.UndeterminedNoChange)
                    throw new System.Exception("Could not resolve all types");

                if(tc == TypeConsolidate.AllDetermined)
                    break;
            }

            SynthLog.Log("Finished verifying struct successfully.");

            //      Gathering globals
            //
            //////////////////////////////////////////////////
            SynthLog.LogHeader("Gathering globals");

            List<SynthVarValue> globals = new List<SynthVarValue>();

            foreach(SynthScope s in this.EnumerateScopes())
                s.RegisterGlobals(globals);

            this.totalGlobalBytes = 0;
            foreach(SynthVarValue svv in globals)
            { 
                svv.alignmentOffset = this.totalGlobalBytes;

                int byteSz = svv.type.GetByteSize();
                if(byteSz <= 0)
                    throw new SynthExceptionImpossible("Data type for global variable is zero in size.");

                SynthLog.Log($"Added {svv.varName} to globals at offset {svv.alignmentOffset}");

                this.totalGlobalBytes += byteSz;
            }

            SynthLog.Log($"Total global variable space is {this.totalGlobalBytes}.");

            //      Verify Functions
            //
            //////////////////////////////////////////////////
            SynthLog.LogHeader("Verifying After function and variable collection pass.");
            this.Validate_AfterTypeAlignment(0);

            SynthLog.Log("Finished verifying functions successfully.");

        }

        SynthVarValue GetParamPhrase(List<Token> tokens)
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

            SynthVarValue newParam = new SynthVarValue();
            newParam.typeName = typename;
            newParam.varName = varname;
            newParam.isSynthParam = true;

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
            SynthLog.LogHeader("Parsing file " + filepath);

            using (var scope = new SynthLog.LogScope())
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
            SynthLog.LogIndent(logIndent, $"Started Context.Validate_AfterTypeAlignment()");

            if (varDefs.Count != 0)
                throw new SynthExceptionImpossible("Root context found with local members. Only global members should be possible.");

            foreach(SynthFuncDecl scopeFn in this.EnumerateScopedFunctions())
            {
                if(scopeFn.isStatic == false)
                    throw new SynthExceptionImpossible("Root context found with local methods. Only static functions should be possible.");
            }

            base.Validate_AfterTypeAlignment(logIndent + 1);

            SynthLog.LogIndent(logIndent, $"Ended Context.Validate_AfterTypeAlignment()");
        }

        public override SynthContext CastContext()
        {
            return this;
        }

        public byte [] BuildWASM()
        {
            SynthLog.LogHeader("Entered BuildWASM() from SynthContext.cs");

            WASMBuild build = new WASMBuild(this);

            // Gather all the functions in the entire build, and make a collection
            // of their unique types.
            this.GatherFunctionRegistration(build);
            // Reorganize function indices so imported functions come first. This is
            // required for WASM.
            build.RealignFunctions();

            List<byte> fileContent = new List<byte>();
            fileContent.AddRange( System.BitConverter.GetBytes(WASM.BinParse.WASM_BINARY_MAGIC));
            fileContent.AddRange( System.BitConverter.GetBytes(1));

            // TODO: Consider removing the WASMSection class (in another file)

            foreach (var kvp in this.functions)
            {
                List<SynthFuncDecl> lst = kvp.Value;
                foreach(SynthFuncDecl sfd in lst)
                { 
                    if(sfd.isExtern == true)
                        continue;

                    sfd.Build(build);
                }
            }

            //
            //      FUNCTION TYPE DECLARATIONS
            //      "Type"s
            //////////////////////////////////////////////////
            fileContent.Add( (byte)WASM.Bin.Section.TypeSec );
            { 
                List<byte> typeSection = new List<byte>();
            
                // Function type count
                typeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)build.functionTypes.Count));

                for(int i = 0; i < build.functionTypes.Count; ++i)
                { 
                    WASMBuild.FunctionType fty = build.functionTypes[i];

                    // Function tag
                    typeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)WASM.Bin.TypeID.Function));

                    // Param count
                    typeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)fty.paramTys.Count));
                    // Param values
                    for(int j = 0; j < fty.paramTys.Count; ++j)
                    {
                        // Types will never be big enough to need LEB encoding. Single byte is fine.
                        typeSection.Add((byte)fty.paramTys[j]);
                    }
            
                    // Only 1 return value max will ever be returned.
                    if(fty.retTy == WASM.Bin.TypeID.Empty)
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
            fileContent.Add( (byte)WASM.Bin.Section.ImportSec );
            { 
                List<byte> importSection = new List<byte>();
            
                List<WASMBuild.FunctionInfo> lstImported = build.GetRangeImportFunctions();

                // Function count
                importSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstImported.Count));
            
                for(int i = 0; i < lstImported.Count; ++i)
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
            List<WASMBuild.FunctionInfo> lstLocalFns = build.GetRangeNonImportFunctions();
            fileContent.Add( (byte)PxPre.WASM.Bin.Section.FunctionSec );
            {
                List<byte> functionSection = new List<byte>();

                // Function Count
                functionSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstLocalFns.Count));
            
                for(int i = 0; i < lstLocalFns.Count; ++i)
                    functionSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstLocalFns[i].typeIndex));
            
                fileContent.AddRange(WASM.BinParse.EncodeSignedLEB(functionSection.Count));
                fileContent.AddRange(functionSection);
            }

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
            
                memorySection.AddRange(WASM.BinParse.EncodeUnsignedLEB(0));
            
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
            
                globalsSection.AddRange(WASM.BinParse.EncodeUnsignedLEB(0));

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
                for(int i = 0; i < lstLocalFns.Count; ++i)
                { 
                    if( lstLocalFns[i].function.isStatic == false || lstLocalFns[i].function.isEntry == false)
                        continue;

                    ++exportedCt;
                }

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

                fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)exportSection.Count));
                fileContent.AddRange(exportSection);
            }

            //
            //      --
            //      "Start"
            //////////////////////////////////////////////////
            //fileContent.Add((byte)WASM.Bin.Section.StartSec);
            //{
            //    List<byte> startSection = new List<byte>();
            //
            //    int startFnID = 0;
            //    startSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)startFnID));
            //
            //
            //    fileContent.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)startSection.Count));
            //    fileContent.AddRange(startSection);
            //}

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
                codeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)lstLocalFns.Count));

                for (int i = 0; i < lstLocalFns.Count; ++i)
                {
                    WASMBuild.FunctionInfo finfo = lstLocalFns[i];
                    SynthFuncDecl fn = finfo.function;

                    if(fn.fnBin == null)
                        throw new SynthExceptionImpossible($"Attempting to save out WASM of function {fn.functionName} that hasn't been processed.");

                    // Function size
                    codeSection.AddRange(WASM.BinParse.EncodeUnsignedLEB((uint)fn.fnBin.Length));
                    // Locals byte size
                    codeSection.AddRange(fn.fnBin);
                }

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

    }
}
