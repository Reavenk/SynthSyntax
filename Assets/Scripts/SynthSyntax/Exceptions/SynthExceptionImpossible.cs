namespace PxPre.SynthSyn
{
    /// <summary>
    /// Exceptions for states that should never happen.
    /// 
    /// Building the program requires agreements between multiple systems working
    /// in concert. Each system is a pass that transforms the data, along with 
    /// providing system-domain-specific error checking that creates assertion
    /// for future passes in other systems. If a previous pass and previous system
    /// does their job correctly, certain states and errors should be impossible.
    /// 
    /// An impossible exception is used to detect is previous system did not prepare
    /// the data and compile state correctly, or do correct error checking.
    /// </summary>
    public class SynthExceptionImpossible : System.Exception
    {

        public SynthExceptionImpossible()
            : base()
        {
            SynLog.Log("IMPOSSIBLE!");
        }

        public SynthExceptionImpossible(string why)
            : base(why)
        { 
            SynLog.Log("IMPOSSIBLE!: " + why);
        }

    }
}