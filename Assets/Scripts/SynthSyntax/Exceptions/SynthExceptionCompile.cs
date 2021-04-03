using System.Collections;
using System.Collections.Generic;
using UnityEngine;


namespace PxPre.SynthSyn
{
    /// <summary>
    /// An exception type that's thrown when the compiler "currently" can't handle
    /// a syntax or feature that's eventually planned for feature-complete.
    /// </summary>
    public class SynthExceptionCompile : System.Exception
    { 
        public SynthExceptionCompile()
            : base()
        { 
            SynLog.Log("COMPILE ERROR!");
        }

        public SynthExceptionCompile(string why)
            : base(why)
        {
            SynLog.Log("COMPILE!: " + why);
        }
    }
}
