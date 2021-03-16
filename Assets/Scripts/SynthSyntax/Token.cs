using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public struct Token
    {
        public int line;
        public string fragment;
        public TokenType type;

        public Token(int line, string fragment, TokenType type)
        {
            this.line = line;
            this.fragment = fragment;
            this.type = type;
        }

        public bool Matches(TokenType ty)
        { 
            return this.type == ty;
        }

        public bool Matches(string fragment)
        { 
            return this.fragment == fragment;
        }

        public bool Matches(TokenType ty, string fragment)
        { 
            return this.type == ty && this.fragment == fragment;
        }

        public bool MatchesSymbol(string fragment)
        { 
            return this.Matches( TokenType.tySymbol, fragment);
        }

        public bool Matches(TkScan tks)
        { 
            if(tks.fragment != null)
            { 
                if(this.fragment != tks.fragment)
                    return false;
            }
            if(tks.ty.HasValue == true)
            { 
                if(this.type != tks.ty)
                    return false;
            }
            return true;
        }
    }

    public struct TkScan
    { 
        public string fragment;
        public TokenType? ty;

        public TkScan(TokenType ty, string fragment)
        { 
            this.fragment = fragment;
            this.ty = ty;
        }

        public TkScan(TokenType ty)
        { 
            this.ty = ty;
            this.fragment = null;
        }

        public TkScan(string fragment)
        { 
            this.ty = null;
            this.fragment = fragment;
        }

    }
}