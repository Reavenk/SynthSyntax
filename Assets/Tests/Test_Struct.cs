using NUnit.Framework;

namespace Tests
{
    public class Test_Struct
    {
        [Test]
        public void Test_StructBasic()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructBasic.synsyn");
        }

        [Test]
        public void Test_StructConstructor()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructConstructor.synsyn");
        }

        [Test]
        public void Test_StructConstructorOverload()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Struct/Vali_StructConstructorOverload.synsyn");
        }
    }
}
