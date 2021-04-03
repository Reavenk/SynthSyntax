using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynCanidateFuncs : SynObj
    {
        public SynScope scope = null;
        public List<SynFuncDecl> functions = new List<SynFuncDecl>();

        public SynCanidateFuncs(SynScope scope)
        { 
            this.scope = scope;
        }

        public override SynCanidateFuncs CastCanidateFunctions() 
        { 
            return this; 
        }
    }
}