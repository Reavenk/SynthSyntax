using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynObj
    {
        public int lineNumber = -1;

        public virtual SynFuncDecl CastFuncDecl() { return null; }
        public virtual SynCanidateFuncs CastCanidateFunctions(){return null; }
        public virtual SynContext CastContext() {return null; }
        public virtual SynType CastType() {return null; }
        public virtual SynVarValue CastVarDecl() { return null; }
        public virtual SynNestingBuilder CastNest() { return null; }
    }
}
