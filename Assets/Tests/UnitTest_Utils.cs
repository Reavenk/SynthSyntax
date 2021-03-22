using System.Collections;
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using PxPre.SynthSyn;
using PxPre.WASM;

public static class UnitTest_Utils
{
    static TestResults currentTest = null;

    public static TestResults StartTest()
    { 
        currentTest = new TestResults();
        return currentTest;
    }

    public static void LogTest(int idx)
    {
        UnityEngine.Debug.Log($"Called LogTest({idx})");
        currentTest.results.Add(idx);
    }

    public static void EndTest()
    {
        UnityEngine.Debug.Log("called EndTest()");
        ++currentTest.endings;
    }

    public static void SetupTestingContext(ExecutionContext ec)
    { 
        ec.importData.SetFunction(
            "ImportedFns", 
            "LogTest", 
            new ImportFunction_Refl(
                typeof(UnitTest_Utils).GetMethod(
                    "LogTest", 
                    System.Reflection.BindingFlags.Public|System.Reflection.BindingFlags.Static)));

        ec.importData.SetFunction(
            "ImportedFns",
            "EndTest",
            new ImportFunction_Refl(
                typeof(UnitTest_Utils).GetMethod(
                    "EndTest",
                    System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)));
    }

    /// <summary>
    /// A "contained" unit test is a synthsyn script has has all the data for the
    /// unit test contained within its scripts. Some of the test information isn't
    /// in the script logic itself, but within markup text in the comments
    /// </summary>
    /// <param name="filepath">The synthsyn file to load.</param>
    public static void PerformContainedTest(string filepath)
    {
        string cwd = System.IO.Directory.GetCurrentDirectory();
        string fullPath = System.IO.Path.Combine(cwd, filepath);

        Debug.Log($"Starting contained unit test at {filepath}.");
        Debug.Log($"Using the full path {fullPath}");

        string testName;
        List<int> expected = new List<int>();

        if(LoadContainTestMarkup(filepath, out testName, expected) == false)
            throw new System.Exception($"Test at {filepath} was expected to be contained, but is missing test information.");

        using (var logScope = new SynthLog.LogScope())
        {
            SynthContext ctx = new SynthContext();
            ctx.ParseFile(filepath);
            byte[] wasmBin = ctx.BuildWASM();

            System.IO.File.WriteAllBytes("test.Wasm", wasmBin);

            Module mod = Module.LoadBinary(wasmBin);
            ExecutionContext exc = new ExecutionContext(mod, true);

            UnitTest_Utils.SetupTestingContext(exc);
            TestResults tr = UnitTest_Utils.StartTest();
            exc.RunFunction(mod.GetExportedFunction("DoTest"));

            tr.TestExpectations(expected.ToArray());
        }
    }

    public static bool LoadContainTestMarkup(string testFile, out string testName, List<int> results)
    {
        testName = string.Empty;

        int setName = 0;
        int loadResults = 0;

        string [] lines = System.IO.File.ReadAllLines(testFile);
        foreach(string l in lines)
        { 
            string str = l.Trim();
            if(str.StartsWith("//") == false)
                continue;

            int mkuPos = str.IndexOf(">>");
            if(mkuPos == -1)
                continue;

            int mkuVP = str.IndexOf(":");
            if(mkuVP == -1 || mkuVP < mkuPos)
                continue;

            string key = str.Substring(mkuPos + 3, mkuVP - mkuPos - 3); 
            key = key.Trim();

            if(key.Equals("name", System.StringComparison.OrdinalIgnoreCase) == true)
            { 
                testName = str.Substring(mkuVP + 1).Trim();
                if(
                    testName.Length >= 2 && 
                    testName[0] == '\"' && 
                    testName[testName.Length - 1] =='\"')
                {
                    testName = testName.Substring(1, testName.Length - 2);
                }
                ++setName;
            }
            else if(key.Equals("results", System.StringComparison.OrdinalIgnoreCase) == true)
            { 
                string resStr = str.Substring(mkuVP + 1).Trim();
                if(
                    resStr.Length >= 2 &&
                    resStr[0] == '{' &&
                    resStr[resStr.Length - 1] == '}')
                {
                    resStr = resStr.Substring( 1, resStr.Length - 2);
                }
                string[] segs = resStr.Split(new char[]{',' });
                foreach(string se in segs)
                {
                    results.Add(int.Parse(se));
                }
                ++loadResults;
            }
        }

        return 
            setName == 1 && 
            loadResults == 1;
    }
}
