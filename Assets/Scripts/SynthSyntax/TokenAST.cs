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
    }
}