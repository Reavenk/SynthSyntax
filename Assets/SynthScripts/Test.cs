using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using PxPre.SynthSyn;

public class Test : MonoBehaviour
{
    // Start is called before the first frame update
    void Start()
    {
        SynthContext synCtx = null;
        
        using( var logScope = new SynthLog.LogScope())
        {
            synCtx = new SynthContext();
            //synCtx.ParseFile("Assets/SynthScripts/Vec2f.synsyn");
            //synCtx.ParseFile("Assets/SynthScripts/Math.synsyn");
            //synCtx.ParseFile("Assets/SynthScripts/Rectf.synsyn");
            //synCtx.ParseFile("Assets/SynthScripts/Ctx2d.synsyn");
            //synCtx.ParseFile("Assets/SynthScripts/basic.synsyn");
            synCtx.ParseFile("Assets/SynthScripts/Vali_Simple.synsyn");
            //synCtx.ParseFile("Assets/SynthScripts/Vali_Simple2.synsyn");

            byte [] rb = synCtx.BuildWASM();
            System.IO.File.WriteAllBytes("TestBuild.wasm", rb);
        }
    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
