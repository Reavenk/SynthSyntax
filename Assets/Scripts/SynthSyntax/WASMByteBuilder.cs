using System.Collections.Generic;

namespace PxPre.SynthSyn
{
    public class WASMByteBuilder
    {
        public List<byte> bin = new List<byte>();

        public void AddLEB128(int v)
        {
            this.bin.AddRange(WASM.BinParse.EncodeSignedLEB(v));
        }

        public void AddLEB128(uint v)
        { 
            this.bin.AddRange(WASM.BinParse.EncodeUnsignedLEB(v));
        }

        public void AddLEB128(long v)
        {
            this.bin.AddRange(WASM.BinParse.EncodeSignedLEB(v));
        }

        public void AddLEB128(ulong v)
        {
            this.bin.AddRange(WASM.BinParse.EncodeUnsignedLEB(v));
        }

        public void AddFloat(float v)
        {
            this.bin.AddRange(System.BitConverter.GetBytes(v));
        }

        public void AddFloat64(double v)
        { 
            this.bin.AddRange(System.BitConverter.GetBytes(v));
        }

        public void AddInstr(WASM.Instruction instr)
        { 
            this.bin.Add((byte)instr);
        }

        public byte [] Bin()
        {
            return this.bin.ToArray();
        }

        public void Add(WASMByteBuilder wbb)
        { 
            this.bin.AddRange(wbb.bin);
        }

        public void Add(IEnumerable<byte> ie)
        { 
            this.bin.AddRange(ie);
        }

        public void AddInstrI32Const(int v)
        { 
            this.AddInstr(WASM.Instruction.i32_const);
            this.AddLEB128(v);
        }

        public void AddInstrI32Const(uint v)
        {
            this.AddInstr(WASM.Instruction.i32_const);
            this.AddLEB128(v);
        }

        public void AddInstrI32End()
        {
            this.AddInstr(WASM.Instruction.end);
        }

        public void AddInstrI32Store(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i32_store);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }
    }
}