using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynRegion : SynScope
    { 
        public string name;
        public SynFuncDecl entry;
        public SynFuncDecl exit;

        public int bodyStart = -1;

        public SynRegion(SynScope parent)
            : base(parent)
        { }

        public static SynRegion Parse(SynScope parentScope, List<Token> tokens)
        { 
            int idx = 0;
            if(tokens[idx].Matches(TokenType.tyWord, "region") == false)
                return null;

            ++idx;

            if(tokens[idx].Matches(TokenType.tyWord) == false)
                throw new SynthExceptionSyntax(tokens[idx], $"Invalid region name.");

            string scopeName = tokens[idx].fragment;
            ++idx;

            if(tokens[idx].Matches(TokenType.tySymbol, "{") == false)
                throw new SynthExceptionSyntax(tokens[idx], $"Expected region to start with an opening brace."); 

            int bodyStart = idx;
            int end = bodyStart;
            Parser.MovePastCurlScope(ref end, tokens);

            SynRegion ret = new SynRegion(parentScope);
            ret.name = scopeName;
            ret.bodyStart = bodyStart;
            ret.declPhrase = tokens.GetRange(0, end);
            tokens.RemoveRange(0, end);

            ret.BreakApartParsedTokens();

            return ret;
        }

        public override void BreakApartParsedTokens()
        {
            List<Token> inner = this.declPhrase.GetRange(bodyStart + 1, this.declPhrase.Count - bodyStart - 2);
            int line = inner[0].line;

            SynFuncDecl decl = 
                SynFuncDecl.Parse(
                    this, 
                    inner, 
                    null, 
                    false, 
                    SynFuncDecl.ParseType.Region|SynFuncDecl.ParseType.Externable);

            if(decl == null)
                throw new SynthExceptionSyntax(line, "Only entry and exit functions are allowed in regions.");

            if(string.IsNullOrEmpty(decl.returnTyName) == false)
                throw new SynthExceptionSyntax(line, "Region functions cannot have return types.");

            if(decl.functionName == "entry")
            { 
                if(this.entry != null)
                    throw new SynthExceptionSyntax(line, "A region entry function being redeclared.");

                this.entry = decl;
            }
            else if(decl.functionName == "exit")
            {
                if (this.exit != null)
                    throw new SynthExceptionSyntax(line, "A region entry function being redeclared.");

                // TODO: Compare exit parameters with entry.
                this.exit = decl;
            }
        }

        public override void Validate_AfterTypeAlignment(int logIndent)
        {
            SynLog.LogIndent(logIndent, $"Starting SynthRegion.Validate_AfterTypeAlignment : {this.name}");

            base.Validate_AfterTypeAlignment(logIndent + 1);

            SynLog.LogIndent(logIndent, "Ending SynthRegion.Validate_AfterTypeAlignment");
        }
    }
}