using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthCanidateFunctions : SynthObj
    {
        public SynthScope scope = null;
        public List<SynthFuncDecl> functions = new List<SynthFuncDecl>();

        public SynthCanidateFunctions(SynthScope scope)
        { 
            this.scope = scope;
        }

        public override SynthCanidateFunctions CastCanidateFunctions() 
        { 
            return this; 
        }
    }
}