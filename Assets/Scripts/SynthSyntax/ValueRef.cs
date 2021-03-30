namespace PxPre.SynthSyn
{
    public enum ValueLoc
    {
        /// <summary>
        /// The ValueRef return doesn't represent an operation
        /// that resulted in a return value.
        /// </summary>
        NoValue,

        /// <summary>
        /// Intrinsic value on the stack
        /// </summary>
        ValueOnStack,

        ValueOnMemStack,

        /// <summary>
        /// The value exists on the heap, and the ValueRef is representing
        /// the address to the base location.
        /// </summary>
        ValueOnHeap,

        /// <summary>
        /// The value exists as an int32 pointer on the WASM stack.
        /// </summary>
        AddrLocal,

        /// <summary>
        /// The index of a value that's an intrinsic on the WASM stack.
        /// 
        /// This is used for intrinsic parameters and intrinsic local
        /// values.
        /// </summary>
        LocalIdx,

        /// <summary>
        /// Intrinsic or non-instrinsic value who's pointer is on the stack. 
        /// The pointer is an int.
        /// </summary>
        PointerOnStack,
    }

    /// <summary>
    /// Represents and kind of value that's being calculated during runtime.
    /// 
    /// Values exist as either intrinsic types (both to the SynthSyn language
    /// and to WASM) or pointers to more complex aggregated types (i.e., structs/arrays).
    /// 
    /// These values can exists in several forms and locations, represented by the
    /// ValueLoc enum.
    /// </summary>
    public class ValueRef : SynthObj
    {
        public ValueLoc valLoc;

        /// <summary>
        /// The local index on the stack - 
        /// or if the variable doesn't exist on the stack, the pointer
        /// to it.
        /// </summary>
        public int idx;

        /// <summary>
        /// The number of bytes on the memory -
        /// or if the variable doesn't exist on the stack, the memory.
        /// </summary>
        public int byteAlign;

        /// <summary>
        /// The global scope version of idx;
        /// </summary>
        public int fnIdx;

        /// <summary>
        /// The global scope version of fnByteAlign.
        /// </summary>
        public int fnByteAlign;

        /// <summary>
        /// The pointer indirection amount. A value of 0 is the actual variable
        /// value. A value of 1 is a pointer. A value of 2 is a pointer to a 
        /// pointer, etc.
        /// </summary>
        public int pointerAmt = 0;

        /// <summary>
        /// How to interpret the ValueRef.
        /// </summary>
        public SynthType varType;

        public ValueRef(ValueLoc valLoc, int idx, int byteAlign, SynthType varType, int pointerAmt = 0)
        {
            this.valLoc = valLoc;
            this.idx = idx;
            this.byteAlign = byteAlign;
            this.varType = varType;
            this.pointerAmt = pointerAmt;
        }

        public void PutInstrinsicValueOnStack(WASMByteBuilder fnBuild)
        {
            if (this.varType.intrinsic == false)
                throw new SynthExceptionImpossible("Attempting to get the value of a non-intrinsic type for an intrinsic operation.");

            // If it's already on the stack, we're done
            if (this.valLoc == ValueLoc.ValueOnStack)
                return;

            if (this.valLoc == ValueLoc.LocalIdx)
            {
                switch (this.varType.typeName)
                {
                    case "bool":
                    case "int8":
                    case "uint8":
                    case "int16":
                    case "uint16":
                    case "int":
                    case "uint":
                    case "int64":
                    case "uint64":
                    case "float":
                    case "double":
                        fnBuild.Add_LocalGet((uint)this.fnIdx);
                        break;

                    default:
                        throw new SynthExceptionImpossible($"Failed to retrieve intrinsic value of type {this.varType.typeName}.");
                }
                return;
            }

            if (this.valLoc != ValueLoc.PointerOnStack)
                throw new SynthExceptionCompile("Attempting to get intrinsic value from unsupported location.");

            switch (this.varType.typeName)
            {
                case "int8":
                    fnBuild.Add_I32Load8_s();
                    break;
                case "uint8":
                    fnBuild.Add_I32Load8_u();
                    break;
                case "int16":
                    fnBuild.Add_I32Load16_s();
                    break;
                case "uint16":
                    fnBuild.Add_I32Load16_u();
                    break;
                case "int":
                case "uint":
                    fnBuild.Add_I32Load();
                    break;
                case "int64":
                case "uint64":
                    fnBuild.Add_I64Load();
                    break;

                case "float":
                    fnBuild.Add_F32Load();
                    break;

                case "double":
                    fnBuild.Add_F64Load();
                    break;

                default:
                    throw new SynthExceptionImpossible($"Failed to retrieve intrinsic value of type {this.varType.typeName}.");
            }
        }

        public ValueRef PutLocalVarAddressOnStack(WASMByteBuilder fnBuild)
        {
            if (this.valLoc == ValueLoc.ValueOnMemStack)
            {
                // Load the stack pointer at [0]
                fnBuild.Add_I32Const(0);
                fnBuild.Add_I32Load();

                // Load the offset of the variable from the
                // base of the stack (if needed)
                if (this.fnByteAlign != 0)
                {
                    fnBuild.Add_I32Const(this.fnByteAlign);
                    fnBuild.AddInstr(WASM.Instruction.i32_add);
                }
                return new ValueRef(ValueLoc.PointerOnStack, -1, -1, this.varType, 1);
            }

            throw new SynthExceptionImpossible($"Could not put value of type {this.valLoc} on the stack.");
        }
    }
}