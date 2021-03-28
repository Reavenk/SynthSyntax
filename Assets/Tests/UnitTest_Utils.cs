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

    public static void LogTestf(float v)
    {
        UnityEngine.Debug.Log($"Called LogTestf({v})");
        currentTest.fResults.Add(v);
    }

    public static void EndTest()
    {
        UnityEngine.Debug.Log("called EndTest()");
        ++currentTest.endings;
    }

    public static void SetupTestingContext(ExecutionContext ec)
    { 
        StoreDeclarations.ModuleRecord impMod = ec.importData.storeDecl.GetModuleRecord("ImportedFns");
        foreach(string str in impMod.functions.Keys)
        { 
            if(str == "LogTest")
            {
                ec.importData.SetFunction(
                    "ImportedFns",
                    "LogTest",
                    new ImportFunction_Refl(
                        typeof(UnitTest_Utils).GetMethod(
                            "LogTest",
                            System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)));
            }
            else if(str == "LogTestf")
            {
                ec.importData.SetFunction(
                    "ImportedFns",
                    "LogTestf",
                    new ImportFunction_Refl(
                        typeof(UnitTest_Utils).GetMethod(
                            "LogTestf",
                            System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)));
            }
        }

        



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
        List<int> expected;
        List<float> expectedf;

        if(LoadContainTestMarkup(filepath, out testName, out expected, out expectedf) == false)
            throw new System.Exception($"Test at {filepath} was expected to be contained, but is missing test information.");

        if(expected == null && expectedf == null)
            throw new System.Exception($"Test at {filepath} do not have expected answer keys to test against.");

        using (var logScope = new SynthLog.LogScope())
        {
            SynthContext ctx = new SynthContext();
            ctx.ParseFile(filepath);
            WASMBuild wasmBuild = new WASMBuild(ctx);
            byte[] wasmBin = wasmBuild.BuildWASM();

            System.IO.File.WriteAllBytes("test.Wasm", wasmBin);

            Module mod = Module.LoadBinary(wasmBin);
            ExecutionContext exc = new ExecutionContext(mod, true);

            SetupTestingContext(exc);
            TestResults tr = StartTest();
            exc.RunFunction(mod.GetExportedFunction("DoTest"));

            if(expected != null)
                tr.TestExpectations(expected.ToArray());

            if(expectedf != null)
                tr.TestExpectations(expectedf.ToArray());

        }
    }

    public static bool LoadContainTestMarkup(
        string testFile, 
        out string testName, 
        out List<int> results,
        out List<float> fresults)
    {
        testName = string.Empty;

        results = null;
        fresults = null;

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
                results = new List<int>();

                string [] res = GetResultsTokens(str, mkuVP);
                foreach(string r in res)
                    results.Add(int.Parse(r.Trim()));

                ++loadResults;
            }
            else if(key.Equals("resultsf", System.StringComparison.OrdinalIgnoreCase) == true)
            { 
                fresults = new List<float>();

                string[] res = GetResultsTokens(str, mkuVP);
                foreach (string r in res)
                    fresults.Add(float.Parse(r.Trim()));

                ++loadResults;
            }
        }

        return 
            setName == 1 && 
            loadResults == 1;
    }

    static string [] GetResultsTokens(string resultsLine, int colonIdx)
    {
        string resStr = resultsLine.Substring(colonIdx + 1).Trim();
        if (
            resStr.Length >= 2 &&
            resStr[0] == '{' &&
            resStr[resStr.Length - 1] == '}')
        {
            resStr = resStr.Substring(1, resStr.Length - 2);
        }
        string[] segs = resStr.Split(new char[] { ',' });
        return segs;
    }
}
