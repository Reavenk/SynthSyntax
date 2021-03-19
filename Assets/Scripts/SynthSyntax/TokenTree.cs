using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class TokenTree
    { 
        /// <summary>
        /// The token the tree was created from. 
        /// 
        /// Used for:
        /// - Parsing when building the AST.
        /// - Validation.
        /// - A reference to the source file when throwing syntax errors.
        /// </summary>
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
                if(tokens[0].MatchesSymbol(";") == true)
                { 
                    if(tokens.Count == 1)
                        break;

                    throw new SynthExceptionSyntax(tokens[0], "Unexpected semicolon");
                }

                if (tokens[0].MatchesSymbol(")") == true)
                    throw new SynthExceptionSyntax(tokens[0], "Unexpected end parenthesis.");

                if (tokens[0].MatchesSymbol("]") == true)
                    throw new SynthExceptionSyntax(tokens[0], "Unexpected end bracket.");

                if (tokens[0].MatchesSymbol("}") == true)
                    throw new SynthExceptionSyntax(tokens[0], "Unexpected end brace.");

                // All the nesting calls are pretty much duplicates. For now we'll allow
                // the duplication, but may refactor later. 
                //
                // ATM we'll be lucky if everything parses and functions correctly - regardless
                // of how clean and consolidated things are.
                // (wleu 03/18/2021)
                if (tokens[0].MatchesSymbol("(")  == true)
                {
                    // It's either a cast, or region for function parameters.
                    int scopeIdx = 0;
                    Parser.MovePastScope(ref scopeIdx, tokens, ")", null); 

                    TokenTree tt = new TokenTree();
                    tt.root = tokens[0];
                    tt.toksToProcess = tokens.GetRange(1, scopeIdx - 2);
                    tokens.RemoveRange(0, scopeIdx);

                    nodes.Add(tt);
                }
                else if(tokens[0].MatchesSymbol("[") == true)
                {
                    // For now, it's just an indexing. This should evaluate
                    // to one tree expression.
                    int scopeIdx = 0;
                    Parser.MovePastScope(ref scopeIdx, tokens, "]", null);

                    TokenTree tt = new TokenTree();
                    tt.root = tokens[0];
                    tt.toksToProcess = tokens.GetRange(1, tokens.Count - 2);
                    tokens.RemoveRange(0, tokens.Count);

                    nodes.Add(tt);
                }
                else if(tokens[0].MatchesSymbol("{") == true)
                {
                    // Outside of a function declaration, an if/for/while/etc construct, 
                    // it's just an anonymous scope.
                    //
                    // This should evaluate to an arbitrary number of tree expressions
                    // as lines of function body code.
                    int scopeIdx = 0;
                    Parser.MovePastScope(ref scopeIdx, tokens, "]", null);

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
                {
                    nodes.RemoveAt(nodes.Count - 1);
                    continue;
                }
                break;
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
                    nodes[i].root.Matches(TokenType.tySymbol, ">>=" ) == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "<<=" ) == true )
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

            // This might still need some figuring out, but if we're at the start
            // of an expression (with a type inside) then it's a cast.
            if(nodes[0].root.MatchesSymbol("(") == true)
            { 
                nodes[0].keyword = "cast";
                nodes[0].nodes.AddRange(nodes.GetRange(1, nodes.Count - 1));
                nodes.RemoveRange(1, nodes.Count - 1);
            }

            // TODO:
            // To be refactored for more rigerous syntax processing. Right now we 
            // work from the backwards inwards; but really at this point we should 
            // be consuming the end - and if we can't process the very last item, 
            // things are already FUBARED.
            for (int i = nodes.Count - 1; i >= 0 ; --i)
            {
                
                if (nodes[i].root.Matches(TokenType.tySymbol, ".") == true)
                {
                    return CreatePivot(nodes, i, foundEquals);
                }
                else if (nodes[i].root.MatchesSymbol("(") == true)
                { 
                    // Either a cast or function parameters. Either way, we separate with a comma for now.
                    TokenTree nodeIdx = new TokenTree("paren");
                    nodeIdx.root = nodes[i].root;

                    List<Token> parenToks = nodes[i].toksToProcess;
                    while(parenToks.Count > 0)
                    { 
                        int iter = 0;
                        Parser.MovePastScopeTComma(ref iter, parenToks);
                        nodes[i].nodes.Add(EatTokensIntoTree(parenToks.GetRange(0, iter)));
                        parenToks.RemoveRange(0, iter);
                    }

                    List<TokenTree> pre = nodes.GetRange(0, i);
                    nodeIdx.nodes.Add(nodes[i]);
                    nodeIdx.nodes.Add(ConsolidateTokenTree(pre, true));

                    return nodeIdx;

                }
                else if(nodes[i].root.MatchesSymbol("[") == true)
                { 
                    // The new indexing item. We don't use the original (nodes[i]) because
                    // we need to have seperate and distinct tree items - the AST being 
                    // indexed (the stuff to the left of the []), and the AST for the indexing 
                    // key (the stuff inside the []).
                    TokenTree nodeIdx = new TokenTree("index");
                    // The parsed token in the same.
                    nodeIdx.root = nodes[i].root;
                    // Break down the original's children as roots
                    nodes[i].nodes.Add(ConsumeBody(nodes[i].toksToProcess, 0));
                    // Everything that builds up the thing to index is built as a recursive call.
                    
                    List<TokenTree> pre = nodes.GetRange(0, i);
                    nodeIdx.nodes.Add(ConsolidateTokenTree(pre, true));

                    return nodeIdx;
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

        public static void ThrowIfAtEdge(List<TokenTree> nodes, int index, bool gstart = true, bool gend = true)
        { 
            if((gstart == true && index <= 0) || (gend == true && index >= nodes.Count - 1))
            {
                throw new SynthExceptionSyntax(nodes[index].root, $"Token {nodes[index].root.fragment} found at an invalid position.");
            }
        }
    }
}