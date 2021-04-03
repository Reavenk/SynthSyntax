using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynIntrinsic : SynType
    {
        public int byteSize;

        public SynIntrinsic(SynScope parent, string name, int byteSize)
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
