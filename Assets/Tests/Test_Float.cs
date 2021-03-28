using NUnit.Framework;

namespace Tests
{
    public class Test_Float
    {
        [Test]
        public void Test_FloatBasic()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatBasic.synsyn");
        }

        [Test]
        public void Test_FloatAdd()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatAdd.synsyn");
        }

        [Test]
        public void Test_FloatSub()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatSub.synsyn");
        }

        [Test]
        public void Test_FloatNegation()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatNegation.synsyn");
        }

        [Test]
        public void Test_FloatMul()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatMul.synsyn");
        }

        [Test]
        public void Test_FloatDiv()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatDiv.synsyn");
        }

        [Test]
        public void Test_FloatOrder()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatOrder.synsyn");
        }

        [Test]
        public void Test_FloatLocalGet()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatLocalGet.synsyn");
        }

        [Test]
        public void Test_FloatLocalSet()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Float/Vali_FloatLocalSet.synsyn");
        }
    }
}