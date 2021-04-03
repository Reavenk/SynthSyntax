using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    // Custom types
    public abstract class SynType : SynScope
    {    
        public string typeName;
        public bool intrinsic;

        public SynType(SynScope parent, string name, bool intrinsic)
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

        public virtual SynFuncDecl GetDefaultConstructor() => null;
        public virtual SynFuncDecl GetDestructor() => null;
        public virtual SynFuncDecl GetCopyConstructor(bool autocreate, SynNestingBuilder scb) => null;
    }


}
