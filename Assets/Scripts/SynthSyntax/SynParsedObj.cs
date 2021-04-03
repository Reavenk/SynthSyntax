using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynParsedObj
    {
        public readonly SynScope parentScope;
        public List<Token> declPhrase = new List<Token>();

        public SynParsedObj(SynScope parentScope)
        { 
            this.parentScope = parentScope;
        }
    }
}