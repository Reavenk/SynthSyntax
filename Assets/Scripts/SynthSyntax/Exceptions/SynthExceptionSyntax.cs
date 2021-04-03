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
            SynLog.Log("SYNTAXERROR!");
        }

        public SynthExceptionSyntax(int line, string why)
            : base(why)
        {
            SynLog.Log($"Syntax Error line {line}: " + why);
        }

        public SynthExceptionSyntax(Token t, string why)
            : base($"Syntax Error line {t.line}: " + why)
        {
            SynLog.Log($"Syntax Error line {t.line}: " + why);
        }
    }
}