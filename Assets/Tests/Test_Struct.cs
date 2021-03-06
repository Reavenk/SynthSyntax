using NUnit.Framework;

namespace Tests
{
    public class Test_Struct
    {
        [Test]
        public void Test_01_Basic()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructBasic.synsyn");
        }

        [Test]
        public void Test_02_DefaultConstructor()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructDefaultConstructor.synsyn");
        }

        [Test]
        public void Test_03_ConstructorOverload_1()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructConstructorOverload_1.synsyn");
        }

        [Test]
        public void Test_04_ConstructorOverload_2()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructConstructorOverload_2.synsyn");
        }

        [Test]
        public void Test_05_Deconstructor()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructDestructor.synsyn");
        }

        [Test]
        public void Test_06_Multiple()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructMulti.synsyn");
        }

        [Test]
        public void Test_07_MethodSimple()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructMethodSimple.synsyn");
        }

        [Test]
        public void Test_08_MethodSelfMod()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructMethodSelfMod.synsyn");
        }

        [Test]
        public void Test_09_MethodReturnIntrinsic()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructMethodReturnInstrinsic.synsyn");
        }

        [Test]
        public void Test_10_MethodReturnMemory()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructMethodReturnMemory.synsyn");
            throw new System.Exception("Unimplemented!");
        }
    }
}
