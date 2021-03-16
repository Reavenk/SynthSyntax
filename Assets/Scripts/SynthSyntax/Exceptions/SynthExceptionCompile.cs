using System.Collections;
using System.Collections.Generic;
using UnityEngine;


namespace PxPre.SynthSyn
{
    public class SynthExceptionCompile : System.Exception
    { 
        public SynthExceptionCompile()
            : base()
        { 
            SynthLog.Log("COMPILE ERROR!");
        }

        public SynthExceptionCompile(string why)
            : base(why)
        {
            SynthLog.Log("COMPILE!: " + why);
        }
    }
}
