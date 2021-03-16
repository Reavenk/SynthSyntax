using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthType_Intrinsic : SynthType
    {
        public int byteSize;

        public SynthType_Intrinsic(SynthScope parent, string name, int byteSize)
            : base(parent, name, true)
        { 
            this.byteSize = byteSize;
        }

        public override int GetByteSize()
        { 
            return this.byteSize;
        }

        public override TypeConsolidate ResolveStaticTypeAlignments()
        { 
            return TypeConsolidate.AllDetermined;
        }

        public override bool Aligned()
        { 
            return true;
        }
    }
}
