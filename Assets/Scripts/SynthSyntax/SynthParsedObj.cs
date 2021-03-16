using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthParsedObj
    {
        public readonly SynthScope parentScope;
        public List<Token> declPhrase = new List<Token>();

        public SynthParsedObj(SynthScope parentScope)
        { 
            this.parentScope = parentScope;
        }
    }
}