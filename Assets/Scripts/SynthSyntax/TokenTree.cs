using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class TokenTree
    { 
        // These properties that are shortcuts into the this.root members
        // aren't actually meant to be used. They're included so the values
        // will be visible in the debugger without needing to expand
        // the this.root nest.
        public TokenType _tokenType { get => this.root.type; }
        public string _fragment { get => this.root.fragment; }

        // TODO: These should no doubt be turned into enums,
        // the game goes for the "keyword" string member.
        const string keyCast = "cast";
        const string keyVarDecl = "vardecl";

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

        public static TokenTree EatTokensIntoTree(List<Token> tokens, SynthScope scope, bool rootExpression)
        {
            // When we start processing the tokens here, these are already
            // single-line expressions that have been separated by semicolons.
            // We strip them out at this point if we see any, for the sake
            // of making the rest of the process simpler.
            while(tokens.Count > 0 && tokens[tokens.Count - 1].MatchesSymbol(";") == true)
                tokens.RemoveAt(tokens.Count - 1);

            List<TokenTree> nodes = new List<TokenTree>();

            if(tokens.Count == 0)
                throw new SynthExceptionImpossible("Attempting to process tokens collection of size 0.");

            if(rootExpression == true)
            { 
                // Is a local variable being declared?
                if(tokens[0].Matches(TokenType.tyWord) == true)
                { 
                    SynthType sty = scope.GetType(tokens[0].fragment);
                    if(sty != null)
                    { 
                        TokenTree ttVarDecl = new TokenTree(keyVarDecl);
                        ttVarDecl.root = tokens[0];

                        TokenTree ttName = new TokenTree("varname");
                        ttName.root = tokens[1];

                        ttVarDecl.nodes.Add(ttName);

                        if(tokens.Count > 2)
                        { 
                            
                            if(tokens.Count < 4 || tokens[2].MatchesSymbol("=") == false)
                                throw new SynthExceptionSyntax(tokens[2], "Invalid syntax for declaring local variable.");

                            List<Token> setExpr = tokens.GetRange(3, tokens.Count - 3);
                            ttVarDecl.nodes.Add(EatTokensIntoTree(setExpr, scope, false));

                            return ttVarDecl;
                        }
                    }
                }
            }

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
                nodeOrder.nodes.Add(EatTokensIntoTree(initTokens, scope, false));
                nodeOrder.nodes.Add(EatTokensIntoTree(predTokens, scope, false));
                nodeOrder.nodes.Add(EatTokensIntoTree(incrTokens, scope, false));

                TokenTree nodeBody = ConsumeBody(tokens, endPredicate, scope);

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
                TokenTree nodeBody = ConsumeBody(tokens, endPredicate, scope);

                TokenTree ret = new TokenTree("if");
                ret.root = tokens[0];
                ret.nodes.Add(EatTokensIntoTree(predToks, scope, false));
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
                TokenTree nodeBody = ConsumeBody(tokens, endPredicate, scope);

                TokenTree ret = new TokenTree("while");
                ret.root = tokens[0];
                ret.nodes.Add(EatTokensIntoTree(predToks, scope, false));
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
                TokenTree nodeBody = ConsumeBody(tokens, 1, scope);

                TokenTree ret = new TokenTree("dowhile");
                ret.root = tokens[0];
                ret.nodes.Add(EatTokensIntoTree(predTokens, scope, false));
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

            return ConsolidateTokenTree(nodes, scope, false);
        }

        public static TokenTree ConsumeBody(List<Token> tokens, int bodyStart, SynthScope scope)
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
                    ret.nodes.Add(EatTokensIntoTree(phrase, scope, false));
                }
            }
            else
            {
                int endBody = bodyStart;
                Parser.MovePastScopeTSemi(ref endBody, tokens);

                if (endBody != tokens.Count - 1)
                    throw new SynthExceptionSyntax(tokens[0], "Invalid for loop body.");

                List<Token> body = tokens.GetRange(2, tokens.Count - 2);
                ret.nodes.Add(EatTokensIntoTree(body, scope, false));
            }

            return ret;
        }

        public static TokenTree ConsolidateTokenTree(List<TokenTree> nodes, SynthScope scope, bool rootExpression)
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

            if(nodes[0].root.MatchesSymbol("(") == true)
            { 
                // TODO: Check for a cast here?
                //if(nodes.Count != 1)
                //    throw new SynthExceptionSyntax(nodes[0].root, "Unexpected extra symbols after parenthesis statement.");
                //
                //if(nodes[0].toksToProcess.Count == 0)
                //    throw new SynthExceptionSyntax(nodes[0].root, "Empty parenthesis statement.");
                //
                //return EatTokensIntoTree(nodes[0].toksToProcess);
            }

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
                    return CreatePivot(nodes, i, scope, false);
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
                    return CreatePivot(nodes, i, scope, false);
                }
            }

            for (int i = nodes.Count - 1; i >= 0; --i)
            {
                if (
                    nodes[i].root.Matches(TokenType.tySymbol, "+") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "-") == true)
                {
                    return CreatePivot(nodes, i, scope, false);
                }
            }

            for (int i = nodes.Count - 1; i >= 0; --i)
            { 
                if(
                    nodes[i].root.Matches(TokenType.tySymbol, "*") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "/") == true ||
                    nodes[i].root.Matches(TokenType.tySymbol, "%") == true)
                {
                    return CreatePivot(nodes, i, scope, false);
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
                    return CreatePivot(nodes, i, scope, false);
                }
            }

            if(nodes[0].root.Matches(TokenType.tySymbol, "(") == true)
            { 
                TokenTree retParen = null;
                if(nodes[0].toksToProcess.Count == 1)
                { 
                    // If a single word is in parenthesis, it's a cast.
                    if(nodes[0].toksToProcess[0].Matches(TokenType.tyWord) == true)
                    {
                        retParen = new TokenTree("cast");
                        retParen.root = nodes[0].toksToProcess[0];
                    }
                }
                // A nested expression (including what could still be a cast)
                if(retParen == null) 
                    retParen = EatTokensIntoTree(nodes[0].toksToProcess, scope, false);

                // if the entire phrase is inside a parenthesis, relay it back.
                if(nodes.Count == 1)
                    return retParen;

                if(retParen.keyword != null)
                    throw new SynthExceptionSyntax(nodes[0].root, "Expected a cast, but did not find any expression to cast.");

                List<TokenTree> castedExpr = nodes.GetRange(1, nodes.Count - 1);
                TokenTree castedTree = ConsolidateTokenTree(castedExpr, scope, false);
                retParen.nodes.Add(castedTree);

                return retParen;
            }

            if (nodes.Count == 1)
                return nodes[0];

            // TODO:
            // To be refactored for more rigerous syntax processing. Right now we 
            // work from the backwards inwards; but really at this point we should 
            // be consuming the end - and if we can't process the very last item, 
            // things are already FUBARED.
            for (int i = nodes.Count - 1; i >= 0 ; --i)
            {
                
                if (nodes[i].root.Matches(TokenType.tySymbol, ".") == true)
                {
                    return CreatePivot(nodes, i, scope, false);
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
                        nodes[i].nodes.Add(EatTokensIntoTree(parenToks.GetRange(0, iter), scope, false));
                        parenToks.RemoveRange(0, iter);
                    }

                    List<TokenTree> pre = nodes.GetRange(0, i);
                    nodeIdx.nodes.Add(nodes[i]);
                    nodeIdx.nodes.Add(ConsolidateTokenTree(pre, scope, false));

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
                    nodes[i].nodes.Add(ConsumeBody(nodes[i].toksToProcess, 0, scope));
                    // Everything that builds up the thing to index is built as a recursive call.
                    
                    List<TokenTree> pre = nodes.GetRange(0, i);
                    nodeIdx.nodes.Add(ConsolidateTokenTree(pre, scope, false));

                    return nodeIdx;
                }
            }

            throw new SynthExceptionSyntax(nodes[0].root, "Unknown syntax.");
        }

        public static TokenTree CreatePivot(List<TokenTree> nodes, int idx, SynthScope scope, bool rootExpression)
        {
            ThrowIfAtEdge(nodes, idx);

            TokenTree ret = nodes[idx];
            ret.nodes.Add(ConsolidateTokenTree(nodes.GetRange(0, idx), scope, false));
            ret.nodes.Add(ConsolidateTokenTree(nodes.GetRange(idx + 1, nodes.Count - idx - 1), scope, false));

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