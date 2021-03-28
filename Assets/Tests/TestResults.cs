using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TestResults
{
    public int endings = 0;
    public List<int> results = new List<int>();
    public List<float> fResults = new List<float>();

    protected void ValidateOneEnding()
    {
        if (this.endings != 1)
            throw new System.Exception("Expected the tests to be ended exactly one time.");
    }

    public void TestExpectations(params int [] exp)
    { 
        if(this.results.Count != exp.Length)
            throw new System.Exception($"Results count expected {exp.Length} but instead got {this.results.Count}.");

        for(int i = 0; i < exp.Length; ++i)
        { 
            if(exp[i] != this.results[i])
                throw new System.Exception($"Test result for index {i} expected {exp[i]} but got {this.results[i]}.");
        }
    }

    public void TestExpectations(params float [] exp)
    { 
        if(this.fResults.Count != exp.Length)
            throw new System.Exception($"Float results count expected {exp.Length} but instead of {this.fResults.Count}.");

        for(int i = 0; i < exp.Length; ++i)
        {
            if(System.Math.Abs(exp[i] - this.fResults[i]) > 0.0001f)
                throw new System.Exception($"Test float result for {i} expected {exp[i]} but got {this.fResults[i]}.");
        }
    }

}
