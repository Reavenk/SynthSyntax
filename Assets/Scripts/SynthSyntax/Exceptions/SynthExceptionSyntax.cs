using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthExceptionSyntax : System.Exception
    {
        public SynthExceptionSyntax()
            : base()
        {
            SynthLog.Log("SYNTAXERROR!");
        }

        public SynthExceptionSyntax(int line, string why)
            : base(why)
        {
            SynthLog.Log($"Syntax Error line {line}: " + why);
        }

        public SynthExceptionSyntax(Token t, string why)
            : base(why)
        {
            SynthLog.Log($"Syntax Error line {t.line}: " + why);
        }
    }
}