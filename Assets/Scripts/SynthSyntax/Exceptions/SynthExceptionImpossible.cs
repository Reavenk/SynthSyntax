using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    /// <summary>
    /// Exceptions for states that should never happen.
    /// </summary>
    public class SynthExceptionImpossible : System.Exception
    {

        public SynthExceptionImpossible()
            : base()
        {
            SynthLog.Log("IMPOSSIBLE!");
        }

        public SynthExceptionImpossible(string why)
            : base(why)
        { 
            SynthLog.Log("IMPOSSIBLE!: " + why);
        }

    }
}