using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace PxPre.SynthSyn
{
    public class SynthStringRepo
    {
        // UTF16;
        const int BytesPerChar = 2; 

        // 4 bytes for dummy reference counter
        // 4 bytes for the length
        const int BytePadding = 8;

        public List<StringInfo> storedStrings = new List<StringInfo>();
        public Dictionary<string, StringInfo> stringLookup = new Dictionary<string, StringInfo>();

        public class StringInfo
        { 
            public readonly int index;
            public readonly string payload;
            public readonly int alignment;
            public readonly int size;

            public StringInfo(int index, string payload, int alignment, int size)
            { 
                this.index      = index;
                this.payload    = payload;
                this.alignment  = alignment;
                this.size       = size;
            }
        }

        public int totalSize {get; private set; } = 0;

        public int GetStringIndex(string str)
        {
            return this.GetStringInfo(str).index;
        }

        public string GetString(int idx)
        { 
            return this.GetStringInfo(idx).payload;
        }

        public StringInfo GetStringInfo(int idx)
        {
            return this.storedStrings[idx];
        }

        public StringInfo GetStringInfo(string str)
        {
            StringInfo ret;
            if(this.stringLookup.TryGetValue(str, out ret) == true)
                return ret;

            int size = str.Length * BytesPerChar + BytePadding;
            ret = new StringInfo(this.storedStrings.Count, str, totalSize, size);
            this.totalSize += size;

            this.storedStrings.Add(ret);
            this.stringLookup.Add(str, ret);

            return ret;
        }
    }
}