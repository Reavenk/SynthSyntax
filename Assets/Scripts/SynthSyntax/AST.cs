using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class AST
    {
        /// <summary>
        /// How is the data represented?
        /// </summary>
        public enum DataManifest
        { 
            /// <summary>
            /// No data is being represented.
            /// </summary>
            NoData,

            /// <summary>
            /// The data is represented as the result of calculation
            /// or something stored in memory.
            /// </summary>
            Procedural,

            /// <summary>
            /// The data is from a constant declaration. These are values
            /// that are defined inline, and can be calculated during
            /// compile time (not supported ATM) and can be implicitly
            /// casted during compile time.
            /// </summary>
            ValueConst,

            /// <summary>
            /// The data is from a constant declaration that has been
            /// explicitly forced to its data type.
            /// 
            /// This is similar to a ValueConst, but its constant value
            /// cannot be further implicitly casted.
            /// </summary>
            ValueExplicit
        }

        public static DataManifest CombineManifests(DataManifest a, DataManifest b)
        {
            if(a == DataManifest.NoData || b == DataManifest.NoData)
                throw new SynthExceptionImpossible("Attempting to combine data sources that contain no data.");

            if(a == DataManifest.Procedural || b == DataManifest.Procedural)
                return DataManifest.Procedural;

            // Explicit values is lower in priority than prodecural, because once explicit values
            // are used in calculations, they become procedural.
            if(a == DataManifest.ValueExplicit || b == DataManifest.ValueExplicit)
                return DataManifest.ValueExplicit;

            if(a == DataManifest.ValueConst && b == DataManifest.ValueConst)
                return DataManifest.ValueConst;

            throw new SynthExceptionImpossible("Could not combine data source manifests.");
        }

        public static DataManifest CastManifest(DataManifest orig)
        { 
            switch(orig)
            { 
                case DataManifest.NoData:
                    throw new SynthExceptionImpossible("Attempted to convert manifest of cast that contains no data.");

                case DataManifest.Procedural:
                    return DataManifest.Procedural;

                case DataManifest.ValueConst:
                    return DataManifest.ValueConst;

                case DataManifest.ValueExplicit:
                    return DataManifest.Procedural;
            }

            throw new SynthExceptionImpossible("Attempted cast the manifest of unknown type.");
        }

        public static DataManifest CombineManifests(AST a, AST b)
        { 
            return CombineManifests(a.manifest, b.manifest);
        }

        public bool hasAddress;
        public SynObj synthObj;
        public SynType evaluatingType;
        public Token token;
        public ASTOp astType;
        public List<AST> branches;
        public SynNestingBuilder builder;
        public DataManifest manifest;

        public void SetBranches(params AST [] tas)
        { 
            this.branches = new List<AST>();
            this.branches.AddRange(tas);
        }

        public AST(Token t, SynNestingBuilder builder, ASTOp ast, SynObj so, SynType sevty, bool hasAddress, DataManifest manifest, params AST [] branches)
        { 
            this.builder        = builder;
            this.synthObj       = so;
            this.evaluatingType = sevty;
            this.token          = t;
            this.astType        = ast;
            this.hasAddress     = hasAddress;
            this.manifest       = manifest;

            this.branches = new List<AST>(); // Should we always allocate this?
            if(branches != null && branches.Length > 0)
                this.branches.AddRange(branches);
        }

        public AST Clone(bool deep = false)
        { 
            AST ret = 
                new AST(
                    this.token, 
                    this.builder,
                    this.astType, 
                    this.synthObj, 
                    this.evaluatingType, 
                    this.hasAddress,
                    this.manifest);

            if(deep == true)
            { 
                foreach(AST ast in branches)
                    ret.branches.Add(ast.Clone());
            }

            return ret;
        }

        public string DumpDiagnostic()
        { 
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            this._DumpDiagnostic(sb, 0);

            return sb.ToString();

        }

        public void _DumpDiagnostic(System.Text.StringBuilder sb, int depth)
        { 
            string indent = new string('\t', depth);

            sb.Append($"{indent}TYPE : {this.astType}\n");
            sb.Append($"{indent}TOKEN : {this.token.type} {this.token.fragment}\n");
            sb.Append($"{indent}hasaddr[{this.hasAddress}]\n");

            if(this.evaluatingType != null)
                sb.Append($"{indent}EVTY : {this.evaluatingType.typeName}\n");

            if(this.branches.Count == 0)
            {
                sb.Append($"{indent}{{}}\n");
            }
            else
            {
                sb.Append($"{indent}{{\n");

                foreach(AST ta in this.branches)
                    ta._DumpDiagnostic(sb, depth + 1);

                sb.Append($"{indent}}}\n");
            }
        }
    }
}