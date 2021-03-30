using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class WASMFn
    { 
        /// <summary>
        /// Return type
        /// </summary>
        public WASM.Bin.TypeID retTys;

        /// <summary>
        /// Function parameters.
        /// </summary>
        public List<WASM.Bin.TypeID> paramTys = new List<WASM.Bin.TypeID>();

        /// <summary>
        /// Function binary data.
        /// </summary>
        public byte [] fnBin;
    }
}
