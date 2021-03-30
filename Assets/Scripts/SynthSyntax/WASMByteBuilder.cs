using System.Collections.Generic;

namespace PxPre.SynthSyn
{
    public class WASMByteBuilder
    {
        protected List<byte> bin = new List<byte>();

        public byte[] Bin()
        {
            return this.bin.ToArray();
        }

        public int BinCount()
        { 
            return this.bin.Count;
        }

        /// <summary>
        /// Development function to bottleneck access to bin.
        /// </summary>
        /// <param name="b"></param>
        protected void BinAdd(byte b)
        { 
            this.bin.Add(b);
        }

        /// <summary>
        /// Development function to bottleneck access to bin.
        /// </summary>
        /// <param name="bs"></param>
        protected void BinAdd(IEnumerable<byte> bs)
        { 
            this.bin.AddRange(bs);
        }

        public void AddLEB128(int v)
        {
            this.BinAdd(WASM.BinParse.EncodeSignedLEB(v));
        }

        public void AddLEB128(uint v)
        { 
            this.BinAdd(WASM.BinParse.EncodeUnsignedLEB(v));
        }

        public void AddLEB128(long v)
        {
            this.BinAdd(WASM.BinParse.EncodeSignedLEB(v));
        }

        public void AddLEB128(ulong v)
        {
            this.BinAdd(WASM.BinParse.EncodeUnsignedLEB(v));
        }

        public void AddFloat(float v)
        {
            this.BinAdd(System.BitConverter.GetBytes(v));
        }

        public void AddFloat64(double v)
        { 
            this.BinAdd(System.BitConverter.GetBytes(v));
        }

        public void AddInstr(WASM.Instruction instr)
        { 
            this.BinAdd((byte)instr);
        }

        public void Add(WASMByteBuilder wbb)
        { 
            this.bin.AddRange(wbb.bin);
        }

        public void Add(IEnumerable<byte> ie)
        { 
            this.bin.AddRange(ie);
        }

        public void Add_End()
        {
            this.AddInstr(WASM.Instruction.end);
        }
        public void Add_F32Const(float v)
        {
            this.AddInstr(WASM.Instruction.f32_const);
            this.AddFloat(v);
        }

        public void Add_F32Load(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.f32_load);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_F32Store(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.f32_store);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_F64Const(double v)
        {
            this.AddInstr(WASM.Instruction.f64_const);
            this.AddFloat64(v);
        }

        public void Add_F64Load(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.f64_load);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_F64Store(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.f64_store);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I32Const(int v)
        {
            this.AddInstr(WASM.Instruction.i32_const);
            this.AddLEB128(v);
        }

        public void Add_I32Const(uint v)
        {
            this.AddInstr(WASM.Instruction.i32_const);
            this.AddLEB128(v);
        }

        public void Add_I32Load8_s(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i32_load8_s);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I32Load8_u(uint align = 0, uint offset = 0)
        { 
            this.AddInstr(WASM.Instruction.i32_load8_u);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I32Load16_s(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i32_load16_s);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I32Load16_u(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i32_load16_u);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I32Load(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i32_load);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I32Store(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i32_store);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I32Store8(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i32_store8);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I32Store16(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i32_store16);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I64Const(long v)
        {
            this.AddInstr(WASM.Instruction.i64_const);
            this.AddLEB128(v);
        }

        public void Add_I64Const(ulong v)
        {
            this.AddInstr(WASM.Instruction.i64_const);
            this.AddLEB128(v);
        }

        public void Add_I64Load(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i64_load);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_I64Store(uint align = 0, uint offset = 0)
        {
            this.AddInstr(WASM.Instruction.i64_store);
            this.AddLEB128(align);
            this.AddLEB128(offset);
        }

        public void Add_LocalGet(uint idx)
        {
            this.AddInstr(WASM.Instruction.local_get);
            this.AddLEB128(idx);
        }

        public void Add_LocalSet(uint idx)
        { 
            this.AddInstr(WASM.Instruction.local_set);
            this.AddLEB128(idx);
        }
    }
}