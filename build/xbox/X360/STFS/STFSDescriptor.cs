// NOTE This class is protected under GPL License as well as terms and conditions.
/* */ // Most notably, you must not obfuscate/protect this code, you must include an open source
/* */ // to your project that uses this code, and you must also not make profit on it.
/* */ // For more details, access:
// *http://www.gnu.org/
// *License included in the library source
// *License located at X360.PublicResources.GPL30
// *X360.XAbout.GNUProtected for GNU and TaC (Terms and Conditions)
/* */ // You agree to these terms when you use this code.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using X360.IO;
using X360.Other;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace X360.STFS
{
    [StructLayout(LayoutKind.Sequential, Pack = 1, Size = 7)]
    internal class BlockRecord
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
        byte[] xFlags;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)]
        byte[] xThisBlock = new byte[3];
        [MarshalAs(UnmanagedType.I8)]
        byte xLevel;

        public uint ThisBlock
        {
            get { return (uint)(xThisBlock[0] << 16 | xThisBlock[1] << 8 | xThisBlock[2]); }
            set
            {
                xThisBlock[0] = (byte)((value >> 16) & 0xFF);
                xThisBlock[1] = (byte)((value >> 8) & 0xFF);
                xThisBlock[2] = (byte)(value & 0xFF);
            }
        }

        public TreeLevel ThisLevel { get { return (TreeLevel)xLevel; } set { xLevel = (byte)value; } }

        public byte Indicator { get { return (byte)(xFlags[0] >> 6); } set { xFlags[0] = (byte)((value & 3) << 6);}}

        public uint Flags
        {
            get { return (uint)(xFlags[0] << 24 | xFlags[1] << 16 | xFlags[2] << 8 | xFlags[3]);}
            set { xFlags = BitConv.GetBytes(value, true); }
        }

        public BlockRecord() { xFlags = new byte[] { 0, 0, 0, 0 }; }

        public BlockRecord(uint xFlagIn) { Flags = xFlagIn; }

        public BlockRecord(HashStatus xStatus, uint xNext) { Flags = (uint)((uint)xStatus << 30 | (xNext & 0xFFFFFF)); }

        /* Data Block Stuff */
        public HashStatus Status { get { return (HashStatus)(xFlags[0] >> 6); } set { xFlags[0] = (byte)((int)value << 6);} }

        public uint NextBlock { get { return (uint)(xFlags[1] << 16 | xFlags[2] << 8 | xFlags[3]); } set { Flags = (uint)((xFlags[0] << 24) | (int)(value & 0xFFFFFF)); }}

        public void MarkOld()
        {
            Status = HashStatus.Old;
            NextBlock = Constants.STFSEnd;
        }

        /* Table Stuff */
        public byte Index { get { return (byte)(Indicator & 1); }}

        public HashFlag AllocationFlag { get { return (HashFlag)Indicator; } set { Indicator = (byte)value; }}

        public int BlocksFree
        {
            get { return (int)((Flags >> 15) & 0x7FFF); }
            set
            {
                // Sets unused/free blocks in each table, checks for errors
                // if blocksfree is 0xAA for Level 1 or 0x70E4 for L2, whole table can be full o shit, cause theres no used blocks :P
                if (value < 0)
                    value = 0;
                Flags = (uint)((int)(Indicator << 30) | (int)value << 15);
            }
        }

        public bool Switch()
        {
            try
            {
                switch (AllocationFlag)
                {
                    case HashFlag.Unallocated: Flags = (uint)((1 << 30) | BlocksFree << 15); return true;
                    case HashFlag.AllocatedFree: Flags = (uint)((2 << 30) | BlocksFree << 15); return true;
                    case HashFlag.AllocatedInUseOld: Flags = (uint)((3 << 30) | BlocksFree << 15); return true;
                    case HashFlag.AllocatedInUseCurrent: Flags = (uint)((2 << 30) | BlocksFree << 15); return true;
                }
                return false;
            }
            catch { return false; }
        }
    }

    /// <summary>
    /// Object to hold STFS information
    /// </summary>
    public sealed class STFSDescriptor
    {
        [CompilerGenerated]
        internal byte[] xStruct;
        [CompilerGenerated]
        uint[] xSpaceBetween = new uint[3];
        [CompilerGenerated]
        byte xBaseByte;
        [CompilerGenerated]
        internal uint xBlockCount;
        [CompilerGenerated]
        internal BlockRecord TopRecord = new BlockRecord();
        
        internal uint[] SpaceBetween { get { return xSpaceBetween; }}
        /// <summary>
        /// Block size of package
        /// </summary>
        public uint BlockCount { get { return xBlockCount; }}
        /// <summary>
        /// Directory Block Count
        /// </summary>
        public ushort DirectoryBlockCount { get { return (ushort)(xStruct[1] << 8 | xStruct[0]); }}
        internal ushort xDirectoryBlockCount
        { 
            set
            {
                // Max is 0x3FF blocks (0xFFFF entries / 0x40 per block)
                xStruct[0] = (byte)(value & 0xFF);
                xStruct[1] = (byte)((value >> 8) & 3);
            }
        }
        /// <summary>
        /// Directory starting block
        /// </summary>
        public uint DirectoryBlock { get { return (uint)(xStruct[4] << 16 | xStruct[3] << 8 | xStruct[2]); }}
        internal uint xDirectoryBlock
        {
            set
            {
                List<byte> x = new List<byte>();
                x.AddRange(BitConv.GetBytes(value, false));
                for (int i = 0; i < 3; i++)
                    xStruct[2 + i] = x[i];
            }
        }
        internal byte Shift = 0;
        internal ushort BaseBlock { get { return (ushort)(xBaseByte << 0xC); }}
        /// <summary>
        /// STFS Type
        /// </summary>
        public STFSType ThisType { get { return (STFSType)Shift; }}
        /// <summary>
        /// Old block count
        /// </summary>
        public uint OldBlockCount { get { return (uint)TopRecord.BlocksFree; } }
        uint[] BlockLevel { get { return Constants.BlockLevel; } }

        internal STFSDescriptor(STFSType xType, uint xTotalBlocks)
        {
            XSetStructure(xType);
            xStruct = new byte[] { 0, 0, 0, 0, 0 };
            if (xTotalBlocks > SpaceBetween[2])
            {
                xStruct = null;
                throw STFSExcepts.MaxOver;
            }
            xBlockCount = xTotalBlocks;
            xBaseByte = (byte)((ThisType == STFSType.Type0) ? 0xB : 0xA);
        }

        void XSetStructure(STFSType xType)
        {
            switch (xType)
            {
                case STFSType.Type0:
                    {
                        xSpaceBetween[0] = 0xAB;
                        xSpaceBetween[1] = 0x718F;
                        xSpaceBetween[2] = 0xFE7DA; // Max Block
                        Shift = 0;
                    }
                    break;

                case STFSType.Type1:
                    {
                        xSpaceBetween[0] = 0xAC;
                        xSpaceBetween[1] = 0x723A;
                        xSpaceBetween[2] = 0xFD00B; // Max Block before size of package over does FATX limit
                        Shift = 1;
                    }
                    break;
                default: break;
            }
        }

        internal STFSDescriptor(byte[] xDescriptor, uint xTotalBlocks, uint xOldBlocks, byte xType)
        {
            xStruct = xDescriptor;
            XSetStructure((STFSType)(xType & 1));
            TopRecord = new BlockRecord(((uint)(xType >> 1) << 30 | (uint)xOldBlocks << 15));
            if (xTotalBlocks > SpaceBetween[2])
            {
                xStruct = null;
                return;
            }
            xBlockCount = xTotalBlocks;
            xBaseByte = (byte)((ThisType == STFSType.Type0) ? 0xB : 0xA);
            xOldBlocks = 0;
        }

        internal STFSDescriptor(STFSPackage xPackage)
        {
            xPackage.xIO.Position = 0x340;
            xPackage.xIO.IsBigEndian = true;
            int xBlockInfo = xPackage.xIO.ReadInt32();
            xBaseByte = (byte)(((xBlockInfo + 0xFFF) & 0xF000) >> 0xC);
            xPackage.xIO.Position = 0x379;
            if (xPackage.xIO.ReadByte() != 0x24) // Struct Size
                throw STFSExcepts.Type;
            if (xPackage.xIO.ReadByte() != 0) // Reversed
                throw STFSExcepts.Type;
            /* STRUCT OF THE NEXT 6 BYTES:
             * byte for block separation
             * Little Endian File Table block count short (2 bytes)
             * 3 bytes in Little Endian for the starting block of the File Table */
            byte idx = (byte)(xPackage.xIO.ReadByte() & 3);
            xStruct = xPackage.xIO.ReadBytes(5);
            xPackage.xIO.Position = 0x395;
            xBlockCount = xPackage.xIO.ReadUInt32();
            uint xOldBlocks = xPackage.xIO.ReadUInt32();
            // Checks the type of Structure
            if (xBaseByte == 0xB)
            {
                if (idx == 1)
                    XSetStructure(STFSType.Type0);
                else throw STFSExcepts.Type;
            }
            else if (xBaseByte == 0xA)
            {
                if (idx == 0 || idx == 2)
                    XSetStructure(STFSType.Type1);
                else throw STFSExcepts.Type;
            }
            else throw STFSExcepts.Type;
            if (xBlockCount > SpaceBetween[2])
                throw STFSExcepts.MaxOver;
            TopRecord = new BlockRecord(((uint)((idx >> 1) & 1) << 30 | (uint)xOldBlocks << 15));
            // Grab Real Block Count
            for (uint i = (xBlockCount - 1); i >= 0; i--)
            {
                xBlockCount = (i + 1);
                if (GenerateDataOffset(i) < xPackage.xIO.Length)
                    break;
            }
        }

        internal uint GenerateDataBlock(uint xBlock)
        {
            if (xBlock >= 0x4AF768)
                return Constants.STFSEnd;
            try
            {
                // Gets 0xAA section, shifts it for 1 or 2 tables per section, and adds original block
                uint num = ((((xBlock / BlockLevel[0]) + 1) << Shift) + xBlock);
                if (xBlock < BlockLevel[0]) // if the block is less than 0xAA, return teh sexy
                    return num;
                // Gets current 0x70e4 section, adjusts to table count
                num += (((xBlock / BlockLevel[1]) + 1) << Shift); 
                if (xBlock < BlockLevel[1]) // If the block is less than 0x70e4, return teh sexy
                    return num;
                // There is only going to be 1 0x4AF768 section, add to base
                return num + (uint)(1 << Shift);
            }
            catch { throw STFSExcepts.General; }
        }

        internal uint GenerateHashBlock(uint xBlock, TreeLevel xTree)
        {
            if (xBlock >= 0x4AF768)
                return Constants.STFSEnd;
            try
            {
                switch (xTree)
                {
                    case TreeLevel.L0:
                        {
                            // Get Base Level 0 Table
                            uint num = (xBlock / BlockLevel[0]) * SpaceBetween[0];
                            // Adjusts the result for Level 1 table count
                            if (xBlock >= BlockLevel[0])
                            {
                                num += (((xBlock / BlockLevel[1]) + 1) << Shift);
                                // Adjusts for the Level 2 table
                                if (xBlock >= BlockLevel[1])
                                    num += (uint)(1 << Shift);
                            }
                            return num;
                        }
                    case TreeLevel.L1:
                        {
                            // Grab the number of Table 1 blocks
                            if (xBlock < BlockLevel[1])
                                return xSpaceBetween[0];
                            else return (uint)((SpaceBetween[1] * (xBlock / BlockLevel[1])) + (1 << Shift));
                        }
                        
                    // Only one Level 2 table
                    case TreeLevel.L2: return SpaceBetween[1];

                    default: return Constants.STFSEnd;
                }
            }
            catch { throw STFSExcepts.General; }
        }

        internal long GenerateHashOffset(uint xBlock, TreeLevel xTree)
        {
            if (xBlock >= 0x4AF768)
                return Constants.STFSEnd;
            try
            {
                uint result = GenerateHashBlock(xBlock, xTree);
                long xReturn = BlockToOffset(result);
                switch (xTree)
                {
                    case TreeLevel.L0: xReturn += (0x18 * (xBlock % BlockLevel[0])); break;

                    case TreeLevel.L1: xReturn += (0x18 * ((xBlock / BlockLevel[0]) % BlockLevel[0])); break;

                    case TreeLevel.L2: xReturn += (0x18 * ((xBlock / BlockLevel[1]) % BlockLevel[0])); break;
                }
                return xReturn;
            }
            catch { throw STFSExcepts.General; }
        }

        internal long GenerateDataOffset(uint xBlock)
        {
            try
            {
                if (xBlock >= 0x4AF768)
                    return Constants.STFSEnd;
                return BlockToOffset(GenerateDataBlock(xBlock));
            }
            catch { throw STFSExcepts.General; }
        }

        internal long BlockToOffset(uint xBlock)
        {
            try
            {
                long xReturn = 0;
                xReturn += ((xBlock * 0x1000) + BaseBlock);
                return xReturn;
            }
            catch { throw STFSExcepts.General; }
        }

        internal long GenerateBaseOffset(uint xBlock, TreeLevel xTree)
        {
            try { return BlockToOffset(GenerateHashBlock(xBlock, xTree)); }
            catch { throw STFSExcepts.General; }
        }

        /// <summary>
        /// Returns the STFS Descriptor
        /// </summary>
        /// <returns></returns>
        internal byte[] GetData()
        {
            byte idx = 1;
            if (ThisType == STFSType.Type1)
                idx = (byte)(TopRecord.Index << 1);
            // Returns the Descriptor in a data fashion
            List<byte> xReturn = new List<byte>();
            xReturn.AddRange(new byte[] { 0x24, 0 });
            xReturn.Add(idx);
            xReturn.AddRange(xStruct);
            xReturn.AddRange(new byte[20]);
            xReturn.AddRange(BitConv.GetBytes(xBlockCount, true));
            xReturn.AddRange(BitConv.GetBytes(TopRecord.BlocksFree, true));
            return xReturn.ToArray();
        }

        #region Random note
        /* // For Reference, my offset to block psudocode:
         * // Same thing as subtracting baseblock and dividing by 0x1000
         * uint Block = (uint)((Offset >> 0xC) - xBaseByte)) 
         * if (Block > BlockLevel[1]) // If block is greater than level 1 offsets, remove those hashtables
         *     Block -= (((Block / BlockLevel[1]) + 1) << Shift)
         * if (Block > BlockLevel[0]) // If block is greater than level 0 offsets
         *     Block -= (((Block / BlockLevel[0]] + 1) << Shift)
         * Block -= (1 << Shift)
         * Return Block
         * // Due note: No one needs the function, so I don't do much research wif it other than
         * // finding the max block, no check to see if the Offset is a hash offset */
        #endregion
    }
}