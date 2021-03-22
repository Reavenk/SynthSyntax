using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TestResults
{
    public int endings = 0;
    public List<int> results = new List<int>();

    public void TestExpectations(params int [] exp)
    { 
        if(this.endings != 1)
            throw new System.Exception("Expected the tests to be ended exactly one time.");

        if(results.Count != exp.Length)
            throw new System.Exception($"Results count expected {exp.Length} but instead got {results.Count}.");

        for(int i = 0; i < exp.Length; ++i)
        { 
            if(exp[i] != results[i])
                throw new System.Exception($"Test result for index {i} expected {exp[i]} but got {results[i]}.");
        }
    }
}
