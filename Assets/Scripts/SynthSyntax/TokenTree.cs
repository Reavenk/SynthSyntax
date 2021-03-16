using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class TokenTree
    { 
        public Token root;

        // Only used for special nestings keywords:
        // ["if", "while", "for"]
        public string keyword = ""; 

        public List<Token> toksToProcess = new List<Token>();
        public List<TokenTree> nodes = new List<TokenTree>();

        public TokenTree()
        { }

        public TokenTree(string keyword)
        { 
            this.keyword = keyword;
        }

        public static TokenTree EatTokensIntoTree(List<Token> tokens)
        {
            List<TokenTree> nodes = new List<TokenTree>();

            if(tokens.Count == 0)
                throw new SynthExceptionImpossible("Attempting to process tokens collection of size 0.");

            if(tokens[0].Matches(TokenType.tyWord, "for") == true)
            { 
                if(tokens[1].MatchesSymbol("(") == false)
                    throw new SynthExceptionSyntax(tokens[1], "Invalid syntax, for loop.");

                int endPredicate = 1;
                Parser.MovePastParenScope(ref endPredicate, tokens);

                List<Token> orderTokens = tokens.GetRange(2, endPredicate - 2);

                int orderSection = 0;
                Parser.MovePastScopeTComma(ref orderSection, orderTokens);
                List<Token> initTokens = orderTokens.GetRange(0, orderSection);
                orderTokens.RemoveRange(0, orderSection);
                //
                orderSection = 0;
                Parser.MovePastScopeTComma(ref orderSection, orderTokens);
                List<Token> predTokens = orderTokens.GetRange(0, orderSection);
                orderTokens.RemoveRange(0, orderSection);
                //
                orderSection = 0;
                Parser.MovePastScopeTComma(ref orderSection, orderTokens); // TODO: Don't end on semicolon
                List<Token> incrTokens = orderTokens.GetRange(0, orderSection);
                orderTokens.RemoveRange(0, orderSection);

                if(orderTokens.Count > 0)
                    throw new SynthExceptionSyntax(orderTokens[0], "Unknown content in for loop.");

                TokenTree nodeOrder = new TokenTree("order");
                nodeOrder.nodes.Add(EatTokensIntoTree(initTokens));
                nodeOrder.nodes.Add(EatTokensIntoTree(predTokens));
                nodeOrder.nodes.Add(EatTokensIntoTree(incrTokens));

                TokenTree nodeBody = ConsumeBody(tokens, endPredicate);

                TokenTree ret = new TokenTree("for");
                ret.root = tokens[0];
                ret.nodes.Add(nodeOrder);
                ret.nodes.Add(nodeBody);

                return ret;
            }

            if(tokens[0].Matches(TokenType.tyWord, "if") == true)
            {
                if (tokens[1].MatchesSymbol("(") == false)
                    throw new SynthExceptionSyntax(tokens[1], "Invalid syntax, if statement.");

                int endPredicate = 1;
                Parser.MovePastParenScope(ref endPredicate, tokens);

                List<Token> predToks = tokens.GetRange(2, endPredicate - 3);
                TokenTree nodeBody = ConsumeBody(tokens, endPredicate);

                TokenTree ret = new TokenTree("if");
                ret.root = tokens[0];
                ret.nodes.Add(EatTokensIntoTree(predToks));
                ret.nodes.Add(nodeBody);
                return ret;
            }

            if(tokens[0].Matches(TokenType.tyWord, "while") == true)
            {
                if (tokens[1].MatchesSymbol("(") == false)
                    throw new SynthExceptionSyntax(tokens[1], "Invalid syntax, while statement.");

                int endPredicate = 1;
                Parser.MovePastParenScope(ref endPredicate, tokens);

                List<Token> predToks = tokens.GetRange(2, endPredicate - 3);
                TokenTree nodeBody = ConsumeBody(tokens, endPredicate);

                TokenTree ret = new TokenTree("while");
                ret.root = tokens[0];
                ret.nodes.Add(EatTokensIntoTree(predToks));
                ret.nodes.Add(nodeBody);

                return ret;
            }

            if(tokens[0].Matches(TokenType.tyWord, "do") == true)
            {
                if (tokens[1].MatchesSymbol("{") == false)
                    throw new SynthExceptionSyntax(tokens[1], "Invalid syntax, do-while statement.");

                int endBody = 1;
                Parser.MovePastParenScope(ref endBody, tokens);

                if(
                    tokens[endBody].MatchesSymbol("}") == false ||
                    tokens[endBody + 1].MatchesSymbol("while") == false ||
                    tokens[endBody + 2].MatchesSymbol("(") == false)
                {
                    throw new SynthExceptionSyntax(tokens[endBody], "Invalid end of do-while");
                }

                int parenStart = endBody + 2;
                int parenEnd = parenStart;
                Parser.MovePastParenScope(ref parenEnd, tokens);
                List<Token> predTokens = tokens.GetRange(parenStart, parenEnd - parenStart);
                TokenTree nodeBody = ConsumeBody(tokens, 1);

                TokenTree ret = new TokenTree("dowhile");
                ret.root = tokens[0];
                ret.nodes.Add(EatTokensIntoTree(predTokens));
                ret.nodes.Add(nodeBody);

                return ret;
            }

            while (tokens.Count > 0)
            {
                if (tokens[0].MatchesSymbol(")") == true)
                    throw new System.Exception($"Unexpected end parenthesis on line {tokens[0].line}.");

                if (tokens[0].MatchesSymbol("]") == true)
                    throw new System.Exception($"Unexpected end bracket on line {tokens[0].line}.");

                if (tokens[0].MatchesSymbol("}") == true)
                    throw new System.Exception($"Unexpected end brace on line {tokens[0].line}.");

                if (
                    tokens[0].MatchesSymbol("(") ||
                    tokens[0].MatchesSymbol("[") ||
                    tokens[0].MatchesSymbol("{"))
                {
                    int scopeIdx = 0;
                    Parser.MovePastScope(ref scopeIdx, tokens, tokens[0].fragment, new HashSet<string>());

                    TokenTree tt = new TokenTree();
                    tt.root = tokens[0];
                    tt.toksToProcess = tokens.GetRange(1, tokens.Count - 2);
                    tokens.RemoveRange(0, tokens.Count);

                    nodes.Add(tt);
                }
                else
                { 
                    TokenTree tt = new TokenTree();
                    tt.root = tokens[0];
                    tokens.RemoveAt(0);

                    nodes.Add(tt);
                }
            }

            return ConsolidateTokenTree(nodes);
        }

        public static TokenTree ConsumeBody(List<Token> tokens, int bodyStart)
        { 
            TokenTree ret = new TokenTree("body");

            if (tokens[bodyStart].Matches("{") == true)
            {
                List<Token> body = tokens.GetRange(2, tokens.Count - 2);

                while (body.Count > 0)
                {
                    int endBody = 0;
                    Parser.MovePastScopeTSemi(ref endBody, body);

                    List<Token> phrase = tokens.GetRange(0, endBody);
                    tokens.RemoveRange(0, endBody);
                    ret.nodes.Add(EatTokensIntoTree(phrase));
                }
            }
            else
            {
                int endBody = bodyStart;
                Parser.MovePastScopeTSemi(ref endBody, tokens);

                if (endBody != tokens.Count - 1)
                    throw new SynthExceptionSyntax(tokens[0], "Invalid for loop body.");

                List<Token> body = tokens.GetRange(2, tokens.Count - 2);
                ret.nodes.Add(EatTokensIntoTree(body));
            }

            return ret;
        }

        public static TokenTree ConsolidateTokenTree(List<TokenTree> nodes, bool foundEquals = false)
        { 
            while (nodes.Count > 0)
            { 
                if(nodes[nodes.Count - 1].root.Matches( TokenType.tySymbol, ";") == true)
                    nodes.RemoveAt(nodes.Count - 1);
            }

            if(nodes.Count == 0)
                throw new SynthExceptionImpossible("Consolidating tree nodes with a count of 0.");

            if(nodes.Count == 1)
                return nodes[0];

            for(int i = 0; i < nodes.Count; ++i)
            {
                if(
                    nodes[i].root.Matches(TokenType.tySymbol, "="   ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "+="  ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "-="  ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "*="  ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "%="  ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "/="  ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "|="  ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "^="  ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "&="  ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "~="  ) == true)
                {
                    return CreatePivot(nodes, i, true);
                }
            }

            // TODO: Casts
            // TODO: Function calls
            // TODO: Indexing
            // for(int i = 1; i < nodes.Count; ++i)
            // { 
            //     if(nodes[i].root.Matches(TokenType.tyWord) == true &&
            //         nodes[i-1].root.Matches
            // }

            for (int i = 0; i < nodes.Count; ++i)
            {
                if (
                    nodes[i].root.Matches(TokenType.tySymbol, "==") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "!=") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, ">" ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, ">=") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "<" ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "<=") == true)
                {
                    return CreatePivot(nodes, i, foundEquals);
                }
            }

            for (int i = nodes.Count - 1; i >= 0; --i)
            { 
                if(
                    nodes[i].root.Matches(TokenType.tySymbol, "*") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "/") == true )
                {
                    return CreatePivot(nodes, i, foundEquals);
                }
            }

            for (int i = nodes.Count - 1; i >= 0; --i)
            {
                if (
                    nodes[i].root.Matches(TokenType.tySymbol, "*") == true)
                {
                    return CreatePivot(nodes, i, foundEquals);
                }
            }

            for (int i = nodes.Count - 1; i >= 0; --i)
            {
                if (
                    nodes[i].root.Matches(TokenType.tySymbol, "+") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "-") == true)
                {
                    return CreatePivot(nodes, i, foundEquals);
                }
            }

            for (int i = nodes.Count - 1; i >= 0; --i)
            {
                if (
                    nodes[i].root.Matches(TokenType.tySymbol, "&") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "|") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "^") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "~") == true)
                {
                    return CreatePivot(nodes, i, foundEquals);
                }
            }

            for (int i = nodes.Count - 1; i >= 0; --i)
            {
                if ( nodes[i].root.Matches(TokenType.tySymbol, ".") == true )
                {
                    return CreatePivot(nodes, i, foundEquals);
                }
            }


            for (int i = 0; i < nodes.Count - 1; ++i)
            { 
                if(
                    nodes[i].root.Matches(TokenType.tyWord) == true && 
                    nodes[i].root.MatchesSymbol("(") == true)
                { 
                }
            }

            throw new SynthExceptionSyntax(nodes[0].root, "Unknown syntax.");
        }

        public static TokenTree CreatePivot(List<TokenTree> nodes, int idx, bool foundEquals)
        {
            ThrowIfAtEdge(nodes, idx);

            TokenTree ret = nodes[idx];
            ret.nodes.Add(ConsolidateTokenTree(nodes.GetRange(0, idx), foundEquals));
            ret.nodes.Add(ConsolidateTokenTree(nodes.GetRange(idx + 1, nodes.Count - idx - 1), foundEquals));

            return ret;

        }

        public static void ThrowIfAtEdge(List<TokenTree> nodes, int index)
        { 
            if(index <= 0 || index >= nodes.Count - 1)
                throw new SynthExceptionSyntax(nodes[index].root, $"Token {nodes[index].root.fragment} found at an invalid position.");
        }
    }
}