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
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructDeconstructor.synsyn");
        }

        [Test]
        public void Test_06_Multiple()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructMulti.synsyn");
        }

        [Test]
        public void Test_07_MethodSimple()
        {
        }

        [Test]
        public void Test_07_MethodSelfMod()
        {
        }
    }
}
