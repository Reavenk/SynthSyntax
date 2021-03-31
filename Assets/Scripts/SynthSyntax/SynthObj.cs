using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthObj
    {
        public int lineNumber = -1;

        public virtual SynthFuncDecl CastFuncDecl() { return null; }
        public virtual SynthCanidateFunctions CastCanidateFunctions(){return null; }
        public virtual SynthContext CastContext() {return null; }
        public virtual SynthType CastType() {return null; }
        public virtual SynthVarValue CastVarDecl() { return null; }
        public virtual SynthContextBuilder CastNest() { return null; }
    }
}
