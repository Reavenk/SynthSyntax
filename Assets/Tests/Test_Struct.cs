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
        public void Test_03_ConstructorOverload()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructConstructorOverload.synsyn");
        }

        [Test]
        public void Test_04_Multiple()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructMulti.synsyn");
        }
    }
}
