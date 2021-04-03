using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    // Custom types
    public abstract class SynType : SynthScope
    {    
        public string typeName;
        public bool intrinsic;

        public SynType(SynthScope parent, string name, bool intrinsic)
            : base(parent)
        { 
            this.typeName = name;
            this.intrinsic = intrinsic;
        }

        public abstract int GetByteSize();
        public abstract bool Aligned();

        // TODO: Allow types of the same name that are different
        // because of different scopes
        public virtual void RegisterContainedValueTypes(HashSet<string> registered){ }

        public override SynType CastType()
        {
            return this;
        }

        public virtual SynthFuncDecl GetDefaultConstructor() => null;
        public virtual SynthFuncDecl GetDestructor() => null;
        public virtual SynthFuncDecl GetCopyConstructor(bool autocreate, SynthContextBuilder scb) => null;
    }


}
