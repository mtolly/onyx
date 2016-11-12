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
using System.IO;
using System.Windows.Forms;
using System.Drawing;
using System.Security;
using System.Security.Cryptography;
using X360.IO;
using X360.Other;
using X360.Security.Cryptography;
using X360.Security.Cryptography.Kerberos;
using X360.STFS;
using System.Runtime.CompilerServices;

namespace X360.Profile
{
    /// <summary>
    /// Account type
    /// </summary>
    public enum AccountType : byte {
        /// <summary>
        /// Stock account
        /// </summary>
        Stock,
        /// <summary>
        /// Dev/Demo/Etc Account
        /// </summary>
        Kits }

    /// <summary>
    /// Pass code values
    /// </summary>
    public enum PassCode : byte
    {
        /// <summary></summary>
        Null = 0,
        /// <summary></summary>
        UpDPad = 1,
        /// <summary></summary>
        DownDPad = 2,
        /// <summary></summary>
        LeftDPad = 3,
        /// <summary></summary>
        RightDPad = 4,
        /// <summary></summary>
        X = 5,
        /// <summary></summary>
        Y = 6,
        /// <summary></summary>
        LTrigger = 9,
        /// <summary></summary>
        RTrigger = 0xA,
        /// <summary></summary>
        LBumper = 0xB,
        /// <summary></summary>
        RBumper = 0xC
    }

    /// <summary>
    /// Account memberships
    /// </summary>
    public enum Membership : byte
    {
        /// <summary></summary>
        None,
        /// <summary></summary>
        Silver = 0x30,
        /// <summary></summary>
        Gold = 0x60 }

    /// <summary>
    /// Object to hold a user Account
    /// </summary>
    public sealed class UserAccount
    {
        [CompilerGenerated]
        bool xSuccess = false;
        [CompilerGenerated]
        internal DJsIO IO = null;
        [CompilerGenerated]
        DJsIO xBackup = null;
        [CompilerGenerated]
        byte[] xHVKey;
        [CompilerGenerated]
        ulong xXUID = 0;
        [CompilerGenerated]
        bool xIsLive = false;
        [CompilerGenerated]
        AccountType at;
        [CompilerGenerated]
        RC4 xRC4;
        [CompilerGenerated]
        HMACSHA1 xHS;

        /// <summary>
        /// Account XUID
        /// </summary>
        public ulong XUID { get { return xXUID; }}
        /// <summary>
        /// Can go on Xbox Live
        /// </summary>
        public bool IsLiveEnabled { get { return xIsLive; }}
        /// <summary>
        /// Parse success
        /// </summary>
        public bool Success { get { return xSuccess; }}
        /// <summary>
        /// Gets/Sets account type
        /// </summary>
        public AccountType ThisType
        {
            get { return at; }
            set
            {
                if (IO == null)
                    return;
                if (!Enum.IsDefined(typeof(AccountType), value))
                    return;
                at = value;
                if (value == AccountType.Stock)
                    xHVKey = new byte[] { 0xE1, 0xBC, 0x15, 0x9C, 0x73, 0xB1, 0xEA, 0xE9, 0xAB, 0x31, 0x70, 0xF3, 0xAD, 0x47, 0xEB, 0xF3 };
                else
                    xHVKey = new byte[] { 0xDA, 0xB6, 0x9A, 0xD9, 0x8E, 0x28, 0x76, 0x4F, 0x97, 0x7E, 0xE2, 0x48, 0x7E, 0x4F, 0x3F, 0x68 };
                xHS.Key = xHVKey;
            }
        }

        /// <summary>
        /// Initializes an instance
        /// </summary>
        /// <param name="xAcc"></param>
        /// <param name="xType"></param>
        /// <param name="CreateBackup"></param>
        public UserAccount(DJsIO xAcc, AccountType xType, bool CreateBackup)
        {
            if (xAcc.Length != 404 || !Enum.IsDefined(typeof(AccountType), xType))
                return;
            IO = xAcc;
            xHS = new HMACSHA1();
            ThisType = xType;
            DJsIO xfill;
            if (xDecrypt(out xfill))
            {
                if (CreateBackup)
                {
                    xBackup = new DJsIO(true);
                    xBackup.Position = 0;
                    xBackup.Write(IO.ReadStream());
                }
                xfill.Position = 0;
                if (((xfill.ReadByte() >> 5) & 1) == 1)
                {
                    xIsLive = true;
                    xfill.Position = 0x28;
                    xXUID = xfill.ReadUInt64();
                }
                xSuccess = true;
            }
        }

        byte[] xComputeHeaderKey(byte[] xConfounder, byte[] xPayload)
        {
            List<byte> xReturn = new List<byte>();
            xReturn.AddRange(xConfounder);
            xReturn.AddRange(xPayload);
            byte[] buff = xHS.ComputeHash(xReturn.ToArray());
            byte[] buff2 = new byte[0x10];
            Array.Copy(buff, buff2, 0x10);
            return buff2;
        }

        byte[] xComputeRC4Key(byte[] xHeaderKey)
        {
            byte[] buff = xHS.ComputeHash(xHeaderKey);
            byte[] buff2 = new byte[0x10];
            Array.Copy(buff, buff2, 0x10);
            return buff2;
        }

        bool xDecrypt(out DJsIO PayLoad)
        {
            PayLoad = null;
            try
            {
                IO.Position = 0;
                byte[] xHeaderKey = IO.ReadBytes(0x10);
                xRC4 = new RC4(xComputeRC4Key(xHeaderKey));
                byte[] xData = IO.ReadBytes(0x184);
                byte[] xPayload;
                byte[] xConfounder;
                if (!xRC4.KerberosDecrypt(xData, out xConfounder, 8, out xPayload))
                    return false;
                bool xsuccess = xComputeHeaderKey(xConfounder, xPayload).HexString() == xHeaderKey.HexString();
                if (xsuccess)
                    PayLoad = new DJsIO(xPayload, true);
                return xsuccess;
            }
            catch { return false; }
        }

        DJsIO GetDecrypt()
        {
            if (!xSuccess)
                return null;
            DJsIO xReturn;
            xDecrypt(out xReturn);
            return xReturn;
        }

        /// <summary>
        /// Grab the decrypted data
        /// </summary>
        /// <returns></returns>
        public byte[] GetDecryptedData()
        {
            DJsIO x = GetDecrypt();
            return x.ReadStream();
        }

        bool xEncrypt(ref DJsIO xNewPayload)
        {
            try
            {
                if (!xSuccess)
                    return false;
                if (xIsLive)
                {
                    byte[] xService = (ThisType == AccountType.Stock ? 
                        new byte[] { 0x50, 0x52, 0x4F, 0x44 } : // PROD
                        new byte[] { 0x50, 0x41, 0x53, 0x54 }); // PART
                    xNewPayload.Position = 0x34;
                    xNewPayload.Write(xService);
                    xNewPayload.Flush();
                }
                List<byte> xReturn = new List<byte>();
                byte[] xConfounder = xRC4.NewConfounder(8);
                byte[] NewPay = xNewPayload.ReadStream();
                xNewPayload.Dispose();
                byte[] xHeaderKey = xComputeHeaderKey(xConfounder, NewPay);
                xRC4.KeyBinary = xComputeRC4Key(xHeaderKey);
                xReturn.AddRange(xHeaderKey);
                xReturn.AddRange(xRC4.KerberosEncrypt(ref xConfounder, ref NewPay));
                IO.Position = 0;
                IO.Write(xReturn.ToArray());
                IO.Flush();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Restore back up info
        /// </summary>
        /// <returns></returns>
        public bool RestoreBackup()
        {
            if (xBackup == null)
                return false;
            try
            {
                IO.Position = 0;
                IO.Write(xBackup.ReadStream());
                IO.Flush();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Grabs the account gamertag
        /// </summary>
        /// <returns></returns>
        public string GetGamertag()
        {
            DJsIO xTemp = GetDecrypt();
            if (xTemp == null)
                return "";
            xTemp.Position = 8;
            xTemp.IsBigEndian = true;
            return xTemp.ReadString(StringForm.Unicode, 15);
        }

        /// <summary>
        /// Saves the gamertag
        /// </summary>
        /// <param name="xNewTag"></param>
        /// <returns></returns>
        public bool SaveGamertag(string xNewTag)
        {
            if (!xSuccess)
                return false;
            DJsIO xTemp;
            if (!xDecrypt(out xTemp))
                return false;
            xTemp.Position = 8;
            xTemp.Write(xNewTag, StringForm.Unicode, 15, PadLocale.Right, PadType.Null);
            return xEncrypt(ref xTemp);
        }

        /// <summary>
        /// Grabs the pass code
        /// </summary>
        /// <returns></returns>
        public PassCode[] GetPassCode()
        {
            DJsIO xTemp = GetDecrypt();
            if (xTemp == null)
                return new PassCode[0];
            xTemp.Position = 0;
            if (((xTemp.ReadByte() >> 4) & 1) == 1)
            {
                List<PassCode> xReturn = new List<PassCode>();
                for (int i = 0; i < 4; i++)
                    xReturn.Add((PassCode)xTemp.ReadByte());
                if (!xReturn.Contains(PassCode.Null))
                    return xReturn.ToArray();
            }
            xTemp.Position = 0;
            xTemp.Write((byte)((xIsLive ? 1 : 0) << 5 | 0 << 4));
            xTemp.Position = 0x38;
            xTemp.Write(new byte[4]);
            xEncrypt(ref xTemp);
            return new PassCode[0];
        }

        /// <summary>
        /// Sets the pass code
        /// </summary>
        /// <param name="xPass"></param>
        /// <returns></returns>
        public bool SetPassCode(PassCode[] xPass)
        {
            if (xPass == null || xPass.Length != 4 ||
                 xPass.Contains(PassCode.Null))
                return false;
            DJsIO xTemp = GetDecrypt();
            if (xTemp == null)
                return false;
            xTemp.Position = 0x38;
            for (int i = 0; i < 4; i++)
                xTemp.Write((byte)xPass[0]);
            return xEncrypt(ref xTemp);
        }

        /// <summary>
        /// Grabs the membership
        /// </summary>
        /// <returns></returns>
        public Membership GetMembership()
        {
            DJsIO xTemp = GetDecrypt();
            if (xTemp == null)
                return Membership.None;
            xTemp.Position = 0x31;
            return (Membership)xTemp.ReadByte();
        }

        /// <summary>
        /// Save membership type
        /// </summary>
        /// <param name="xMemberType"></param>
        /// <returns></returns>
        public bool SaveMembership(Membership xMemberType)
        {
            DJsIO xTemp = GetDecrypt();
            if (xTemp == null)
                return false;
            xTemp.Position = 0x31;
            xTemp.Write((byte)xMemberType);
            return xEncrypt(ref xTemp);
        }

        /// <summary>
        /// Force into a HDD account
        /// </summary>
        /// <returns></returns>
        public bool ForceIntoHDDAccount()
        {
            DJsIO xTemp = GetDecrypt();
            if (xTemp == null)
                return false;
            xTemp.Position = 0;
            xTemp.Write(new byte[9]);
            xTemp.Position = 0x26;
            xTemp.Write(new byte[(int)xTemp.Length - 0x26]);
            return xEncrypt(ref xTemp);
        }

        /*
        public bool FixHash()
        {
         * unknown on how to do this :(
        }
         */
    }

    /// <summary>
    /// Object to hold extra profile functions
    /// </summary>
    public static class ProfileTools
    {
        /// <summary>
        /// Takes a filename and attempts to extract an ID from it
        /// </summary>
        /// <param name="GPDName"></param>
        /// <returns></returns>
        public static uint GPDNameToID(string GPDName)
        {
            try
            {
                GPDName = GPDName.Replace("\0", "");
                GPDName = GPDName.ToLower();
                GPDName = GPDName.Replace(".gpd", "");
                return Convert.ToUInt32(GPDName, 16);
            }
            catch { return 0; }
        }
    }
}