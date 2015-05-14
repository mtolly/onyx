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
using System.Security.Cryptography;
using System.Runtime.CompilerServices;
using System.Diagnostics;
using X360.IO;
using X360.Security.Cryptography.Kerberos;

namespace X360.Security.Cryptography.Kerberos
{
    /// <summary>
    /// RC4 Kerberos extensions
    /// </summary>
    [DebuggerStepThrough]
    public static class KerbExtenz
    {
        /// <summary>
        /// Runs a Kerberos implemented RC4 decryption
        /// </summary>
        /// <param name="xConfounder">The confounder input</param>
        /// <param name="xPayload">The payload input</param>
        /// <param name="x">RC4 reference</param>
        /// <param name="xData">Data to be decrypted</param>
        /// <param name="xConLen">Length of the Confounder</param>
        /// <returns></returns>
        public static bool KerberosDecrypt(this RC4 x, byte[] xData, out byte[] xConfounder, int xConLen, out byte[] xPayload)
        {
            xPayload = new byte[0];
            xConfounder = new byte[0];
            try
            {
                DJsIO xOut = new DJsIO(x.RunAlgorithm(xData), true);
                xOut.Position = 0;
                xConfounder = xOut.ReadBytes(xConLen);
                xPayload = xOut.ReadBytes(xData.Length - xConLen);
                xOut.Dispose();
            }
            catch { return false; }
            return true;
        }

        /// <summary>
        /// Runs a Kerberos implemented RC4 Encryption
        /// </summary>
        /// <param name="xConfounder">Outputs new Confounder</param>
        /// <param name="xPayload">Outputs the payload</param>
        /// <param name="x">RC4 Reference</param>
        /// <returns></returns>
        public static byte[] KerberosEncrypt(this RC4 x, ref byte[] xConfounder, ref byte[] xPayload)
        {
            List<byte> xIn = new List<byte>();
            xIn.AddRange(xConfounder);
            xIn.AddRange(xPayload);
            return x.RunAlgorithm(xIn.ToArray());
        }

        /// <summary>
        /// Generates a new counfounder of your choice
        /// </summary>
        /// <param name="x"></param>
        /// <param name="ConLen"></param>
        /// <returns></returns>
        public static byte[] NewConfounder(this RC4 x, long ConLen)
        {
            byte[] xReturn = new byte[ConLen];
            Random xRand = new Random();
            for (int i = 0; i < 8; i++)
                xReturn[i] = (byte)xRand.Next(0, 0xFF);
            return xReturn;
        }
    }
}

namespace X360.Security.Cryptography
{
    /// <summary>
    /// Cryptographic exceptions
    /// </summary>
    [DebuggerStepThrough]
    public static class CryptoExcepts
    {
        [CompilerGenerated]
        static readonly Exception xCryptoSign = new Exception("Error when taking the signature");
        [CompilerGenerated]
        static readonly Exception xCryptoVeri = new Exception("Unknown error when trying to verify signature");
        [CompilerGenerated]
        static readonly Exception xRC4KeyError = new Exception("Unknown error when trying to set key");
        [CompilerGenerated]
        static readonly Exception xRC4AlgoError = new Exception("Unknown error when trying to run algorithm");
        [CompilerGenerated]
        static readonly Exception xKVError = new Exception("Error Parsing Keyvault");
        [CompilerGenerated]
        static readonly Exception xKVSize = new Exception("Keyvault not the correct size");
        [CompilerGenerated]
        static readonly Exception xCertConflict = new Exception("Certificate not the same as the keys");
        [CompilerGenerated]
        static readonly Exception xParamError = new Exception("Params invalid");

        /// <summary>Signing error</summary>
        public static Exception CryptoSign { get { return xCryptoSign; } }
        /// <summary>Verifying error</summary>
        public static Exception CryptoVeri { get { return xCryptoVeri; } }
        /// <summary>Assigning RC4 Key</summary>
        public static Exception RC4KeyError { get { return xRC4KeyError; } }
        /// <summary>RC4 Algorithm Error</summary>
        public static Exception RC4AlgoError { get { return xRC4AlgoError; } }
        /// <summary>Keyvault error</summary>
        public static Exception KVError { get { return xKVError; } }
        /// <summary>KeyVault error</summary>
        public static Exception KVSize { get { return xKVSize; } }
        /// <summary>Certificate error</summary>
        public static Exception CertConflict { get { return xCertConflict; } }
        /// <summary>Invalid parameter</summary>
        public static Exception ParamError { get { return xParamError; } }
    }

    /// <summary>
    /// Initializes a new instance of an RC4 class
    /// </summary>
    [DebuggerStepThrough]
    public sealed class RC4
    {
        // RC4 Key
        byte[] xKeyRC4_Bb; // Before key algo
        byte[] xKeyRC4_Ba; // After after key algo

        /// <summary>
        /// Initializes this class with a specified binary key
        /// </summary>
        /// <param name="xKey"></param>
        public RC4(byte[] xKey) { KeyBinary = xKey; }

        /// <summary>
        /// Initializes this class with a specified ASCII string key
        /// </summary>
        /// <param name="xKey"></param>
        public RC4(string xKey) { KeyASCII = xKey;}

        /// <summary>
        /// Gets or sets the key as an ASCII string
        /// </summary>
        public string KeyASCII
        {
            get { return Encoding.ASCII.GetString(xKeyRC4_Bb); }
            set { KeyBinary = Encoding.ASCII.GetBytes(value); }

        }

        /// <summary>
        /// Gets or sets the key used in this RC4 instance
        /// </summary>
        public byte[] KeyBinary
        {
            get { return xKeyRC4_Bb; }
            set
            {
                if (xKeyRC4_Bb == value)
                    return;
                xKeyRC4_Bb = value;
                xKeyRC4_Ba = new byte[0x100];
                int j = 0;
                for (short i = 0; i < 0x100; i++)
                    xKeyRC4_Ba[i] = (byte)i;
                for (short i = 0; i < 0x100; i++)
                {
                    j = (j + xKeyRC4_Ba[i] + value[i % value.Length]) % 0x100;
                    swap(ref xKeyRC4_Ba, i, j);
                }
            }
        }

        void swap(ref byte[] xIn, int i, int j)
        {
            byte temp = xIn[i];
            xIn[i] = xIn[j];
            xIn[j] = temp;
        }

        /// <summary>
        /// Encrypts or decrypts the data
        /// </summary>
        /// <param name="xData"></param>
        /// <returns></returns>
        public byte[] RunAlgorithm(byte[] xData)
        {
            int i = 0, j = 0;
            byte[] xReturn = new byte[xData.Length];
            byte[] n_LocBox = new byte[0x100];
            xKeyRC4_Ba.CopyTo(n_LocBox, 0);
            for (int offset = 0; offset < xData.Length; offset++)
            {
                i = (i + 1) % 0x100;
                j = (j + n_LocBox[i]) % 0x100;
                swap(ref n_LocBox, i, j);
                xReturn[offset] = (byte)(xData[offset] ^ n_LocBox[(n_LocBox[i] + n_LocBox[j]) % 0x100]);
            }
            return xReturn;
        }
    }

    /// <summary>
    /// Used for Quick HMACSHA1 Hashes
    /// </summary>
    [DebuggerStepThrough]
    public static class HMACSHAQuick
    {
        /// <summary>
        /// Runs an HMACSHA1 hash on specified data using specified key
        /// </summary>
        /// <param name="xKey"></param>
        /// <param name="xData"></param>
        /// <returns></returns>
        public static byte[] ComputeHash(byte[] xKey, byte[] xData)
        {
            HMACSHA1 xhs = new HMACSHA1(xKey);
            return xhs.ComputeHash(xData);
        }
    }

    /// <summary>
    /// Get a decryption from just a key and a byte
    /// </summary>
    [DebuggerStepThrough]
    public static class RC4Quick
    {
        /// <summary>
        /// Runs an RC4 on the specified data using the specified key
        /// </summary>
        /// <param name="xKey"></param>
        /// <param name="xData"></param>
        /// <returns></returns>
        public static byte[] RunAlgorithm(byte[] xKey, byte[] xData)
        {
            RC4 xrc4 = new RC4(xKey);
            return xrc4.RunAlgorithm(xData);
        }
        
        /// <summary>
        /// Runs a Kerberos RC4 encryption on the specified data
        /// </summary>
        /// <param name="xKey">Key input</param>
        /// <param name="xConfounder">outputs the confounder</param>
        /// <param name="xPayload">Outputs the payload</param>
        /// <param name="xConLen">Confounder Length</param>
        /// <param name="xData">Outputs the decrypted data</param>
        /// <returns></returns>
        public static bool RunKerberosDecrypt(byte[] xKey, byte[] xData, out byte[] xConfounder, int xConLen, out byte[] xPayload)
        {
            RC4 xrc4 = new RC4(xKey);
            return xrc4.KerberosDecrypt(xData, out xConfounder, xConLen, out xPayload);
        }

        /// <summary>
        /// Runs an RC4 encryption with a specified key
        /// </summary>
        /// <param name="xKey"></param>
        /// <param name="xConfounder"></param>
        /// <param name="xPayload"></param>
        /// <returns></returns>
        public static byte[] RunKerberosEncrypt(byte[] xKey, ref byte[] xConfounder, ref byte[] xPayload)
        {
            RC4 xrc4 = new RC4(xKey);
            return xrc4.KerberosEncrypt(ref xConfounder, ref xPayload);
        }
    }

    /// <summary>
    /// CRC32 Checksums
    /// </summary>
    [DebuggerStepThrough]
    public sealed class CRC32
    {
        [CompilerGenerated]
        const uint bitsfilled = 0xFFFFFFFF;
        [CompilerGenerated]
        int[] crctable = new int[256];
        [CompilerGenerated]
        uint crc = 0;

        /// <summary>Current Checksum</summary>
        public uint CRC { get { return (crc ^ bitsfilled); } }

        /// <summary>
        /// Initializes a default CRC checksum instance
        /// </summary>
        public CRC32() : this(0xEDB88320) { }

        /// <summary>
        /// Initializes a custom CRC checksum instance
        /// </summary>
        /// <param name="Polynomial"></param>
        public CRC32(uint Polynomial)
        {
            for (uint i = 0; i < crctable.Length; i++)
            {
                crc = i;
                for (int j = 0; j < 8; j++)
                {
                    if ((crc & 1) == 1)
                        crc = (crc >> 1) ^ Polynomial;
                    else crc >>= 1;
                }
                crctable[i] = (int)crc;
            }
            crc = bitsfilled;
        }

        /// <summary>
        /// Adds a byte to the checksum
        /// </summary>
        /// <param name="xAdd"></param>
        public void AddToCRC(byte xAdd) { crc = (uint)(((crc >> 8) & bitsfilled) ^ crctable[(crc ^ xAdd) & 0xFF]); }

        /// <summary>
        /// Adds a byte array to the checksum
        /// </summary>
        /// <param name="xAdd"></param>
        public void AddToCRC(byte[] xAdd)
        {
            foreach (byte x in xAdd)
                crc = (uint)(((crc >> 8) & bitsfilled) ^ crctable[(crc ^ x) & 0xFF]);
        }
    }

    /// <summary>
    /// Scrambling methods used in this .DLL
    /// </summary>
    [DebuggerStepThrough]
    public static class ScrambleMethods
    {
        /// <summary>
        /// Swaps bytes in 8 byte blocks, xReverse true if reverse each 8 byte blocks
        /// </summary>
        /// <param name="xPiece"></param>
        /// <param name="xReverse"></param>
        /// <returns></returns>
        public static byte[] StockScramble(byte[] xPiece, bool xReverse)
        {
            if ((xPiece.Length % 8) != 0)
                throw new Exception("Input not divisible by 8");
            DJsIO xStream = new DJsIO(xPiece, true);
            for (int i = 0; i < (xPiece.Length / 2); i += 8)
            {
                xStream.Position = i;
                byte[] xPart1 = xStream.ReadBytes(8);
                xStream.Position = (xPiece.Length - i - 8);
                byte[] xPart2 = xStream.ReadBytes(8);
                xStream.Position = i;
                xStream.Write(xPart2);
                xStream.Position = (xPiece.Length - i - 8);
                xStream.Write(xPart1);
                xStream.Flush();
            }
            if (xReverse)
                for (int i = 0; i < xPiece.Length; i += 8)
                    Array.Reverse(xPiece, i, 8);
            return xPiece;
        }

        /// <summary>
        /// Reverses all bytes
        /// </summary>
        /// <param name="xPiece"></param>
        /// <returns></returns>
        public static byte[] DevScramble(byte[] xPiece)
        {
            Array.Reverse(xPiece);
            return xPiece;
        }

        /// <summary>
        /// Special scramble method
        /// </summary>
        /// <param name="xPiece"></param>
        /// <returns></returns>
        public static byte[] DJScramble(byte[] xPiece)
        {
            if ((xPiece.Length % 8) != 0)
                throw new Exception("Input not divisible by 8");
            int xSpot = 5, xCurrent = 5;
            DJsIO xStream = new DJsIO(xPiece, true);
            xStream.Position = 0;
            byte[] xHalf1 = xStream.ReadBytes((xPiece.Length / 2));
            byte[] xHalf2 = xStream.ReadBytes((xPiece.Length / 2));
            xStream.Position = 0;
            xStream.Write(xHalf2);
            xStream.Write(xHalf1);
            xStream.Flush();
            xStream.Close();
            for (int i = 0; i < xPiece.Length; i += xCurrent)
            {
                Array.Reverse(xPiece, i, xSpot);
                xCurrent = xSpot;
                if (xSpot == 5)
                    xSpot = 3;
                else xSpot = 5;
            }
            return xPiece;
        }
    }

    /// <summary>
    /// Create and verify PKS1 Signatures of SHA1 digest
    /// </summary>
    [DebuggerStepThrough]
    public static class RSAQuick
    {
        /// <summary>
        /// Generate a PKS1 Signature of SHA1 digest
        /// </summary>
        /// <param name="xParam"></param>
        /// <param name="xHash"></param>
        /// <returns></returns>
        public static byte[] SignatureGenerate(RSAParameters xParam, byte[] xHash)
        {
            RSACryptoServiceProvider xRSACrypto = new RSACryptoServiceProvider();
            RSAPKCS1SignatureFormatter xRSASigFormat = new RSAPKCS1SignatureFormatter();
            try { xRSACrypto.ImportParameters(xParam); }
            catch (Exception xerror) { throw xerror; }
            xRSASigFormat.SetHashAlgorithm("SHA1");
            xRSASigFormat.SetKey(xRSACrypto);
            try { return xRSASigFormat.CreateSignature(xHash); }
            catch { throw CryptoExcepts.CryptoSign; }
        }

        /// <summary>
        /// Verifies a PKS1 signature of SHA1 digest
        /// </summary>
        /// <param name="xParam">Keys</param>
        /// <param name="xHash">Hash to sign</param>
        /// <param name="xSignature">Outputs the signature</param>
        /// <returns></returns>
        public static bool SignatureVerify(RSAParameters xParam, byte[] xHash, byte[] xSignature)
        {
            RSACryptoServiceProvider xRSACrypto = new RSACryptoServiceProvider();
            RSAPKCS1SignatureDeformatter xRSASigDeformat = new RSAPKCS1SignatureDeformatter();
            try { xRSACrypto.ImportParameters(xParam); }
            catch (Exception xerror) { throw xerror; }
            xRSASigDeformat.SetHashAlgorithm("SHA1");
            xRSASigDeformat.SetKey(xRSACrypto);
            try { return xRSASigDeformat.VerifySignature(xHash, xSignature); }
            catch { throw CryptoExcepts.CryptoVeri; }
        }

        /// <summary>
        /// Generates a new set of keys
        /// </summary>
        /// <returns></returns>
        public static RSAParameters CreateKeys()
        {
            CspParameters xCSP = new CspParameters();
            RSACryptoServiceProvider xRSA = new RSACryptoServiceProvider(xCSP);
            return xRSA.ExportParameters(true);
        }
    }

    /// <summary>
    /// Initializes a new instance of a SHA1 hash
    /// </summary>
    [DebuggerStepThrough]
    public static class SHA1Quick
    {
        /// <summary>
        /// Computes a SHA1 hash on specified data
        /// </summary>
        /// <param name="xData"></param>
        /// <returns></returns>
        public static byte[] ComputeHash(byte[] xData)
        {
            return new SHA1CryptoServiceProvider().ComputeHash(xData);
        }
    }
}