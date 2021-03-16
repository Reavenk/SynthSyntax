using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public static class TokenASTOps
    {
        public enum ExpressionSide
        { 
            Left,
            Right,
            Unspecified
        }

        public static TokenAST ConsolidateExpression( SynthScope sc, List<Token> tokens, ExpressionSide side)
        { 
            //if(tokens.Count == 0)
            //    return new TokenAST();

            //return TokenTree.EatTokensIntoTree(tokens);
            //
            //return PerformPivot(sc, lst, side);
            return null;
        }

        static string [] equalOps = new string [] { "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^="};
    }
}