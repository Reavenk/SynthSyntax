using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class TokenAST
    {
        public bool hasAddress;
        public SynthObj synthObj;
        public SynthType evaluatingType;
        public Token token;
        public TokenASTType astType;
        public List<TokenAST> branches;

        public void SetBranches(params TokenAST [] tas)
        { 
            this.branches = new List<TokenAST>();
            this.branches.AddRange(tas);
        }

        public TokenAST(Token t, TokenASTType ast, SynthObj so, SynthType sevty, bool hasAddress, params TokenAST [] branches)
        { 
            this.synthObj       = so;
            this.evaluatingType = sevty;
            this.token          = t;
            this.astType        = ast;
            this.hasAddress     = hasAddress;

            this.branches = new List<TokenAST>(); // Should we always allocate this?
            if(branches != null && branches.Length > 0)
                this.branches.AddRange(branches);
        }

        public TokenAST Clone(bool deep = false)
        { 
            TokenAST ret = 
                new TokenAST(
                    this.token, 
                    this.astType, 
                    this.synthObj, 
                    this.evaluatingType, 
                    this.hasAddress);

            if(deep == true)
            { 
                foreach(TokenAST ast in branches)
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

                foreach(TokenAST ta in this.branches)
                    ta._DumpDiagnostic(sb, depth + 1);

                sb.Append($"{indent}}}\n");
            }
        }
    }
}