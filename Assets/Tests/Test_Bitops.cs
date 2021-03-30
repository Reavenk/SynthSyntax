using NUnit.Framework;

namespace Tests
{
    public class Test_Bitops
    {
        [Test]
        public void Test001_IntAnd()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntAnd.synsyn");
        }

        [Test]
        public void Test002_IntOr()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntOr.synsyn");
        }

        [Test]
        public void Test003_IntXOr()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntXor.synsyn");
        }

        [Test]
        public void Test004_IntInv()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntInv.synsyn");
        }

        [Test]
        public void Test005_IntShiftLeft()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntShiftLeft.synsyn");
        }

        [Test]
        public void Test006_IntShiftRight()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntShiftRight.synsyn");
        }
    }
}