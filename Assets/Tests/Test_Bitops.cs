using NUnit.Framework;

namespace Tests
{
    public class Test_Bitops
    {
        [Test]
        public void Test_Bitops_IntAnd()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntAnd.synsyn");
        }

        [Test]
        public void Test_Bitops_IntOr()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntOr.synsyn");
        }

        [Test]
        public void Test_Bitops_IntXOr()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntXor.synsyn");
        }

        [Test]
        public void Test_Bitops_IntInv()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntInv.synsyn");
        }

        [Test]
        public void Test_Bitops_IntShiftLeft()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntShiftLeft.synsyn");
        }

        [Test]
        public void Test_Bitops_IntShiftRight()
        {
            UnitTest_Utils.PerformContainedTest("SynthScripts/Bitops/Vali_IntShiftRight.synsyn");
        }
    }
}