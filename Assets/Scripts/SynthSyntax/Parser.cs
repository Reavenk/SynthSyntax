using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public static class Parser
    {
        
        public static bool Matches(List<Token> tokens, int idx, params TkScan [] eles)
        { 
            if(tokens.Count <= idx + eles.Length)
                return false;

            for(int i = 0; i < eles.Length; ++i)
            {
                if(tokens[idx + i].Matches(eles[i]) == false)
                    return false;
            }
            return true;
        }

        public static bool Matches(string str, ref int idx, string compare)
        { 
            if(idx + compare.Length >= str.Length)
                return false;

            for(int i = 0; i < compare.Length; ++i)
            { 
                if(str[idx + i] != compare[i])
                    return false;
            }
            idx += compare.Length;
            return true;
        }

        public static bool Matches(string str, ref int idx, char compare)
        {
            if(idx >= str.Length)
                return false;

            if(str[idx] == compare)
            { 
                ++idx;
                return true;
            }
            return false;
        }

        public static bool EatWhitespace(string str, ref int idx, ref int curLine)
        { 
            while(true)
            {
                if(idx >= str.Length)
                    return false;

                if (str[idx] == ' ' || str[idx] == '\t' || str[idx] == '\n' || str[idx] == '\r')
                { 
                    if(str[idx] == '\n')
                        ++curLine;

                    ++idx;
                    continue;
                }
                break;

            }
            return true;
        }

        public static void MovePastScopeSymbol(ref int idx, List<Token> tokens, string symFrag, bool startAt = true)
        {
            // Ensure we start at a parenthesis
            if (startAt == false)
                MoveToAnyScopeBracket(ref idx, tokens);

            if (tokens[idx].MatchesSymbol(symFrag) == false)
                throw new System.Exception("");

            // And then do a normal move past scope
            Parser.MovePastScopeTSemi(ref idx, tokens);
        }

        public static void MovePastParenScope(ref int idx, List<Token> tokens, bool startAt = true)
        {
            MovePastScopeSymbol(ref idx, tokens, "(", startAt);
        }

        public static void MovePastSquareScope(ref int idx, List<Token> tokens, bool startAt = true)
        {
            MovePastScopeSymbol(ref idx, tokens, "[", startAt);
        }

        public static void MovePastCurlScope(ref int idx, List<Token> tokens, bool startAt = true)
        {
            MovePastScopeSymbol(ref idx, tokens, "{", startAt);
        }

        public static void MoveToSymbolCollection(ref int idx, List<Token> tokens, HashSet<string> allowedFrags)
        {
            while (true)
            {
                if (idx >= tokens.Count)
                    throw new System.Exception("");

                if (tokens[idx].Matches(TokenType.tySymbol) == true)
                {
                    if (allowedFrags.Contains(tokens[idx].fragment) == true)
                        break;
                }

                ++idx;
            }
        }

        static HashSet<string> scopeFragments = new HashSet<string> { "(", ")", "{", "}", "[", "]" };
        public static void MoveToAnyScopeBracket(ref int idx, List<Token> tokens)
        {
            MoveToSymbolCollection(ref idx, tokens, scopeFragments);
        }

        /// <summary>
        /// Move past nested scopes, and recognize semicolons as terminators.
        /// </summary>
        /// <param name="idx"></param>
        /// <param name="tokens"></param>
        public static void MovePastScopeTSemi(ref int idx, List<Token> tokens)
        {
            HashSet<string> terminator = new HashSet<string> { ";" };
            MovePastScope(ref idx, tokens, "}", terminator);

        }

        /// <summary>
        /// Move past scopes, and recognize commas as terminators.
        /// </summary>
        /// <param name="idx"></param>
        /// <param name="tokens"></param>
        public static void MovePastScopeTComma(ref int idx, List<Token> tokens, bool allowEnd = true)
        {
            HashSet<string> terminator = new HashSet<string> { "," };
            MovePastScope(ref idx, tokens, "}", terminator, allowEnd);

        }

        /// <summary>
        /// Move to the end of the token scope.
        /// 
        /// Given the nesting with (, { and [ characters, move to the end, making 
        /// sure we don't end prematurely in a nest.
        /// 
        /// The function can end either at the ending symbol. or at specified 
        /// sentinel tokens at the highest nesting.
        /// </summary>
        /// <param name="idx">The index location.</param>
        /// <param name="tokens">The tokens to parse through.</param>
        /// <param name="endScope">The ending character.</param>
        /// <param name="termSymFrags">
        /// The sentinel character to stop at. If null, the function terminates
        /// as soon as it enters a scope and returns back out of the highest scope.
        /// </param>
        public static void MovePastScope(ref int idx, List<Token> tokens, string endScope, HashSet<string> termSymFrags, bool allowEnd = false)
        {
            // TBH, this function isn't cleanly implemented or defined.
            // When everything is off the ground, it should be respeced,
            // and rewritten in a cleaner manner.
            // (wleu 03/18/2021)

            // TODO: We may want to add these nests to a small utility 
            // management struct.
            int squareNest = 0;
            int parenNest = 0;
            int curlNest = 0;

            while (true)
            {
                bool decr = false;

                if (idx >= tokens.Count)
                {
                    if(allowEnd == true && squareNest == 0 && parenNest == 0 && curlNest == 0)
                        return;

                    throw new System.Exception("Unexpected end of file");
                }

                if (
                    termSymFrags != null &&
                    (
                        tokens[idx].Matches(TokenType.tySymbol) &&
                        termSymFrags.Contains(tokens[idx].fragment) == true
                    ))
                {

                    if (squareNest == 0 && parenNest == 0 && curlNest == 0)
                    {
                        ++idx;
                        return;
                    }
                }
                else if (tokens[idx].Matches(TokenType.tySymbol, "[") == true)
                {
                    ++squareNest;
                }
                else if (tokens[idx].Matches(TokenType.tySymbol, "]") == true)
                {
                    --squareNest;
                    decr = true;

                    if (squareNest < 0)
                        throw new System.Exception($"Unexpected square bracket mismatch on line {tokens[idx].line}");
                }
                else if (tokens[idx].Matches(TokenType.tySymbol, "(") == true)
                {
                    ++parenNest;
                }
                else if (tokens[idx].Matches(TokenType.tySymbol, ")") == true)
                {
                    --parenNest;
                    decr = true;

                    if (parenNest < 0)
                        throw new System.Exception($"Unexpected parenthesis mismatch on line {tokens[idx].line}.");
                }
                else if (tokens[idx].Matches(TokenType.tySymbol, "{") == true)
                {
                    ++curlNest;
                }
                else if (tokens[idx].Matches(TokenType.tySymbol, "}") == true)
                {
                    --curlNest;
                    decr = true;

                    if (curlNest < 0)
                        throw new System.Exception($"Unexpected curl bracket mismatch on line {tokens[idx].line}.");
                }

                if (decr == true && squareNest == 0 && parenNest == 0 && curlNest == 0)
                {
                    if (termSymFrags == null)
                    {
                        ++idx;
                        return;
                    }

                    if (string.IsNullOrEmpty(endScope) == true || tokens[idx].MatchesSymbol(endScope) == true)
                    {
                        ++idx;

                        if (termSymFrags == null)
                            return;

                        while (
                            idx < tokens.Count &&
                            tokens[idx].Matches( TokenType.tySymbol) && 
                            termSymFrags.Contains(tokens[idx].fragment) == true)
                        { 
                            ++idx;
                        }

                        return;
                    }
                }

                ++idx;
            }
        }

        public static bool EatLeadingSymbol(List<Token> tokens, string symbol)
        {
            bool ret = false;

            while (
                tokens.Count > 0 &&
                tokens[0].MatchesSymbol(symbol) == true)
            {
                tokens.RemoveAt(0);
                ret = true;
            }

            return ret;
        }

        public static bool EatLeadingSemicolons(List<Token> tokens)
        {
            return EatLeadingSymbol(tokens, ";");
        }


        public static bool EatEndingSymbol(List<Token> tokens, string symbol)
        {
            bool ret = false;

            while (
                tokens.Count > 0 &&
                tokens[tokens.Count - 1].MatchesSymbol(";") == true)
            {
                tokens.RemoveAt(tokens.Count - 1);
                ret = true;
            }

            return ret;
        }

        public static bool EatEndingSemicolons(List<Token> tokens)
        { 
            return EatEndingSymbol(tokens, ";");
        }

        public static List<Token> ParseTokens(string str, ref int idx)
        {
            int curLine = 1;

            HashSet<char> singleSyms =
                new HashSet<char>
                {
                    '+', '-', '*', '/', '%', '<', '[', '(', '{', '}', ')', ']', '&', '|', '&', '~', '^', '=', ',', ';', '&', '.'
                };

            HashSet<char> numberInterupt =
                new HashSet<char>
                {

                };

            HashSet<char> wordInterupt =
                new HashSet<char>
                {

                };

            List<string> multiSyms =
                new List<string>
                {
                    "+=",
                    "-=",
                    "/=",
                    "*=",
                    "%=",
                    "++",
                    "--",
                    ">=",
                    "<=",
                    "&&",
                    "||",
                    "==",
                    "!=",
                    "|=",
                    "&=",
                    "^=",
                    "<<",
                    ">>",
                    "<<=",
                    ">>=",
                };

            List<Token> tokens = new List<Token>();
            for (; idx < str.Length;)
            {
                if (Parser.EatWhitespace(str, ref idx, ref curLine) == false)
                    break;

                if (Parser.Matches(str, ref idx, "//") == true)
                {
                    for (; idx < str.Length; ++idx)
                    {
                        if (str[idx] != '\n' && str[idx] != '\r')
                            continue;

                        if (str[idx] == '\n')
                            ++curLine;
                        break;
                    }
                    continue;
                }

                if (Parser.Matches(str, ref idx, "/*") == true)
                {
                    for (; idx < str.Length; ++idx)
                    {
                        if (Parser.Matches(str, ref idx, "*/") == true)
                            break;

                        if (str[idx] == '\n')
                            ++curLine;
                    }
                    continue;
                }

                if (str[idx] == '"')
                {
                    ++idx;
                    int startidx = idx;
                    while (true)
                    {
                        if (idx >= str.Length)
                            throw new System.Exception(); // TODO

                        if (Parser.Matches(str, ref idx, "\\\"") == true)
                            continue;

                        if (str[idx] == '"')
                        {
                            tokens.Add(new Token(curLine, str.Substring(startidx, idx - startidx), TokenType.tyString));
                            ++idx;
                            break;
                        }
                        ++idx;
                    }
                    continue;
                }

                bool cont = false;
                foreach (string ms in multiSyms)
                {
                    if (Parser.Matches(str, ref idx, ms) == true)
                    {
                        tokens.Add(new Token(curLine, ms, TokenType.tySymbol));
                        cont = true;
                        break;
                    }
                }
                if (cont == true)
                    continue;

                if (singleSyms.Contains(str[idx]) == true)
                {
                    tokens.Add(new Token(curLine, str[idx].ToString(), TokenType.tySymbol));
                    ++idx;
                    continue;
                }

                if (char.IsNumber(str[idx]) == true)
                {
                    int decimals = 0;
                    int startIndex = idx;
                    while (idx < str.Length && (char.IsNumber(str[idx]) || str[idx] == '.' || str[idx] == 'f'))
                    {
                        if (str[idx] == '.')
                            ++decimals;

                        ++idx;
                    }
                    if (decimals > 1)
                        throw new System.Exception();

                    string sub = str.Substring(startIndex, idx - startIndex);
                    if (str[idx - 1] == 'f')
                    {
                        sub = sub.Substring(0, sub.Length - 1);
                        float f = float.Parse(sub); // TODO: Most robust testing
                        tokens.Add(new Token(curLine, sub, TokenType.tyFloat));
                    }
                    else if (decimals == 1)
                    {
                        double d = double.Parse(sub); // TODO: More robust testing
                        tokens.Add(new Token(curLine, sub, TokenType.tyDouble));
                    }
                    else if (decimals == 0)
                    {
                        if (str[idx - 1] == 'n')
                        {
                            long n = long.Parse(sub);
                            sub = sub.Substring(0, sub.Length - 1);
                            tokens.Add(new Token(curLine, sub, TokenType.tyLong));
                        }
                        else
                        {
                            int i = int.Parse(sub);
                            tokens.Add(new Token(curLine, sub, TokenType.tyInt));
                        }
                    }
                    else
                        throw new System.Exception($"Invalid number type {sub} on line {curLine}");

                    continue;
                }

                if (char.IsLetter(str[idx]) || str[idx] == '$' || str[idx] == '_')
                {
                    int startIndex = idx;
                    for (; idx < str.Length; ++idx)
                    {
                        if (char.IsLetterOrDigit(str[idx]) || str[idx] == '$' || str[idx] == '_')
                            continue;

                        break;
                    }
                    string word = str.Substring(startIndex, idx - startIndex);
                    tokens.Add(new Token(curLine, word, TokenType.tyWord));
                    continue;
                }

                throw new System.Exception($"Encountered unknown text during SynthSyn parsing on line {curLine}.");
            }

            return tokens;
        }
    }
}