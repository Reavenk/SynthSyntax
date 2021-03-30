using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{

    /// <summary>
    /// Represents a declaration of a variable type, with an optional phrase
    /// for its default value.
    /// </summary>
    public class SynthVarValue : SynthObj
    {
        public enum VarValueDataType
        { 
            Value,
            Pointer,
            Reference
        }

        public enum OuterScope
        { 
            Global,

            /// <summary>
            /// Depending on how this is used, it can mean either the local parameter, or 
            /// for parsing class methods.
            /// </summary>
            Function,
            Struct,
            Parameter
        }

        public enum VarLocation
        {
            Static,
            Parameter,
            Local,
            Member,
            ThisRef
        }

        public string varName;

        public string typeName;
        public SynthType type;

        public VarLocation varLoc;

        public List<Token> declPhrase;
        public int declBaseEnd = -1; // Where does the base declaraction end?

        /// <summary>
        /// For a given context in a set of SynthVarValue, what is the value's offset
        /// in the stack.
        /// </summary>
        public int alignmentOffset;

        public VarValueDataType dataType = VarValueDataType.Value;

        // If the variable is initialized inline - what are the values initialized to. If the variable
        // doesn't have any member (is an intrinsic) then use the string key "this".
        public Dictionary<string, SynthVarValue> childrenValues;

        /// <summary>
        /// Given a set of tokens, parse the data of a variable declaration, with a 
        /// possible assign expression.
        /// </summary>
        /// <param name="tokens">The tokens to parse.</param>
        /// <param name="sentinel">The sentinel symbol. When in a fully collapsed
        /// scope, this determines when a statement ends.</param>
        /// <param name="sentinelEnd">If true, the statement can end by reaching the end
        /// of the tokens list - else, an error will be thrown if we detect parsing to the 
        /// end of the tokens list without a sentinel.</param>
        /// <returns>The parsed variable declaration, or null if a variable declaration
        /// was not found. If parsing is sure we're parsing a variable declaration but it
        /// has been detected to be malformed, an exception is thrown.</returns>
        public static SynthVarValue Parse(List<Token> tokens, string sentinel, bool sentinelEnd, OuterScope scope)
        { 
            int idx = 0;

            if(tokens[idx].Matches(TokenType.tyWord, "static") == true)
            {
                if(scope == OuterScope.Parameter)
                    throw new SynthExceptionSyntax(tokens[idx], $"Parameters cannot be declared static.");

                scope = OuterScope.Global;
                ++idx;
            }

            if(tokens[idx].Matches(TokenType.tyWord) == false)
                return null;

            string typeName = tokens[idx].fragment;
            SynthVarValue.VarValueDataType dataType = VarValueDataType.Value;
            string varName = "";

            ++idx;
            if(tokens[idx].MatchesSymbol("*") == true)
            {
                dataType = VarValueDataType.Pointer;
                ++idx;
            }
            else if(tokens[idx].Matches("&") == true)
            {
                dataType = VarValueDataType.Reference;
                ++idx;
            }

            if (tokens[idx].Matches(TokenType.tyWord) == false)
                return null;

            varName = tokens[idx].fragment;

            ++idx;
            int baseEnd = idx;

            if (tokens.Count == idx)
            { 
                if(sentinelEnd == false)
                    throw new SynthExceptionSyntax(tokens[0], "Unexpected end while parsing parameters.");

            }
            else
            {
                if (tokens[idx].MatchesSymbol(sentinel) == true)
                {  
                    // Do nothing except eat up the condition.
                }
                else if(tokens[idx].MatchesSymbol("=") == true)
                    Parser.MovePastScopeTSemi(ref idx, tokens);
                else
                    throw new SynthExceptionSyntax(tokens[idx], $"Invalid ending to variable declaration.");
            }

            SynthVarValue ret   = new SynthVarValue();
            ret.typeName        = typeName;
            ret.varName         = varName;
            ret.declPhrase      = tokens.GetRange(0, idx);
            tokens.RemoveRange(0, idx);
            ret.dataType        = dataType;
            ret.declBaseEnd     = baseEnd;

            if(scope == OuterScope.Global)
                ret.varLoc = VarLocation.Static;
            else if(scope == OuterScope.Function)
                ret.varLoc = VarLocation.Local;
            else if(scope == OuterScope.Struct)
                ret.varLoc = VarLocation.Member;
            else if(scope == OuterScope.Parameter)
                ret.varLoc = VarLocation.Parameter;
            else
                throw new SynthExceptionImpossible("Unexpected variable location.");

            return ret;
        }

        public static SynthVarValue ParseBodyVar(List<Token> tokens, OuterScope scope)
        {
            if(scope == OuterScope.Parameter)
                throw new SynthExceptionImpossible($"At line {tokens[0].line}, attempting to parse body variable with parameter scope.");

            return Parse(tokens, ";", false, scope);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="tokens">The tokens to analyze for a variable statement at the beginning.</param>
        /// <param name="isGlobal">Defines if the variable is being parsed in a global context. If
        /// the variable is found in local scope, it can still be made into a global variable if
        /// it starts with the static keyword.</param>
        /// <returns></returns>
        public static SynthVarValue ParseParameter(List<Token> tokens)
        { 
            return Parse(tokens, ",", true, OuterScope.Parameter);
        }

        public static SynthVarValue ParseExposedParam(List<Token> tokens)
        { 
            if(tokens[0].Matches(TokenType.tyWord, "param") == false)
                return null;

            
            int line = tokens[0].line; // Just in case we need to throw, we have the line cached.

            tokens.RemoveAt(0);

            SynthVarValue ret = ParseBodyVar(tokens, OuterScope.Global);
            if(ret == null)
                throw new SynthExceptionSyntax(line, "Param syntax error");

            if(ret.dataType != VarValueDataType.Value)
                throw new SynthExceptionSyntax(line, "Param cannot be a pointer or reference.");

            ret.varLoc = VarLocation.Parameter;

            return ret;
        }

        public int GetByteSize()
        { 
            switch(this.dataType)
            {
            case VarValueDataType.Pointer:
            case VarValueDataType.Reference:
                return 4;

            case VarValueDataType.Value:
                return this.type.GetByteSize();

            default:
                throw new SynthExceptionImpossible("Unknown variable type.");
            }
        }

        public override SynthVarValue CastVarDecl()
        {
            return this;
        }
    }
}