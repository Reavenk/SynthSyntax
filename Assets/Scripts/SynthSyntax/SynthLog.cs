using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;

namespace PxPre.SynthSyn
{
    public static class SynthLog
    { 
        public class LogScope : System.IDisposable
        { 
            public LogScope()
            { 
                IncrementLogScope();
            }

            void System.IDisposable.Dispose()
            { 
                DecrementLogScope();
            }
        }

        public static int logScope = 0;
        public static System.IO.TextWriter logOut = null;

        public static void IncrementLogScope()
        { 
            if(logScope == 0)
                logOut = new System.IO.StreamWriter("synsyn.log");

            ++logScope;
        }

        public static void DecrementLogScope()
        { 
            --logScope;

            if(logScope == 0)
                logOut.Close();
        }

        public static void LogHeader(string str)
        { 
            string hstr = "\n\n" + new string('/', 80) + "\n";
            hstr += "// " + str + "\n";
            hstr += new string('/', 80);

            Log(hstr);
        }

        public static void Log(string str)
        {
#if UNITY_EDITOR
            Debug.Log(str);
#endif

            if(logOut != null)
                logOut.WriteLine(str);
        }

        public static void LogIndent(int indent, string str)
        { 
            string indented = new string('\t', indent) + str;
            Log(indented);
        }

        public static void Log(List<Token> tokens)
        {
#if UNITY_EDITOR
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            for(int i = 0; i < tokens.Count; ++i)
            {
                string str = string.Format("{0, 4}) {1, -15} {2}\n", i, tokens[i].type.ToString(), tokens[i].fragment);
                sb.Append(str);
            }

            Log(sb.ToString());
#endif
        }

        public static void LogFragments(List<Token> tokens)
        { 
            Log(string.Join( " ", tokens.Select(x=>x.fragment)));
        }

        public static void LogTree(TokenTree tt)
        {
            LogTree(tt, 0);
        }

        private static void LogTree(TokenTree tt, int indent)
        {
            string str = $"TTree ty:[{tt.root.type}] --- frag:[{tt.root.fragment}]";

            if(string.IsNullOrEmpty(tt.keyword) == false)
                str += $" - keyw : {tt.keyword}";

            LogIndent(indent, str);

            foreach(TokenTree ch in tt.nodes)
                LogTree(ch, indent + 1 );
        }
    }
}