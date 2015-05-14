using System;
using X360.STFS;
using NDesk.Options;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Drawing;
using X360.Other;

namespace rb3pkg
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            string game_title = "Rock Band 3";
            uint game_title_id = 0x45410914;
            string package_title = "Untitled Package";
            string source_dir = ".";
            string description = "Package built with rb3pkg.";
            bool unlock = true;
            string file_out = "rb3pkg_out";
            bool show_help = false;
            PackageType pkg_type = PackageType.SavedGame;

            var p = new OptionSet () {
                { "g|game=", "name of the package's game",
                    v => game_title = v },
                { "i|id=", "title ID of the package's game",
                    (uint v) => game_title_id = v },
                { "p|package=", "name of the package",
                    v => package_title = v },
                { "f|files=", "directory of package files",
                    v => source_dir = v },
                { "u|unlock", "unlock the package",
                    v => unlock = v != null },
                { "d|description=", "package description",
                    v => description = v },
                { "h|?|help", "show options",
                    v => show_help = v != null },
            };
            List<string> extra;
            try {
                extra = p.Parse (args);
            } catch (OptionException e) {
                Console.WriteLine ("rb3pkg: " + e.Message);
                return;
            }
            if (extra.Count >= 1) {
                file_out = extra [0];
            }

            if (show_help) {
                Console.WriteLine ("rb3pkg, command-line custom package builder for Rock Band 3");
                Console.WriteLine ("Basically a stripped-down version of RB3Maker by Technicolor:");
                Console.WriteLine ("<http://rockband.scorehero.com/forum/viewtopic.php?t=34542>");
                Console.WriteLine ("Functionality by DJ SkunkieButt's X360 .NET library <http://skunkiebutt.com/>");
                Console.WriteLine ("Command-line program by Michael Tolly <onyxite@gmail.com>");
                Console.WriteLine ("");
                Console.WriteLine ("Usage: rb3pkg [options] [file_out.con]");
                p.WriteOptionDescriptions(Console.Out);
                return;
            }

            CreateSTFS xsession = new CreateSTFS ();
            xsession.HeaderData.TitleID = game_title_id;
            xsession.HeaderData.SetLanguage (Languages.English);
            xsession.HeaderData.Publisher = "";
            xsession.HeaderData.Title_Package = game_title;
            xsession.STFSType = STFSType.Type0;
            xsession.HeaderData.ThisType = pkg_type;
            xsession.HeaderData.Title_Display = package_title;
            xsession.HeaderData.Description = description;

            string pngfilename = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location) + "/rb3.png";
            Bitmap pngbitmap = new Bitmap(pngfilename);
            byte[] pngbytes = Imaging.ImageToBytes(pngbitmap, System.Drawing.Imaging.ImageFormat.Png);
            xsession.HeaderData.PackageImageBinary = pngbytes;
            xsession.HeaderData.ContentImageBinary = pngbytes;

            addFiles (xsession, source_dir, "");

            RSAParams firstParams = new RSAParams(StrongSigned.LIVE);
            var rec = new X360.Other.LogRecord ();
            STFSPackage pkg = new STFSPackage(xsession, firstParams, file_out, rec);
            foreach (string s in rec.Log) {
                Console.WriteLine (s);
            }
            pkg.FlushPackage(firstParams);
            pkg.CloseIO ();

            if (unlock) {
                using (FileStream stream = new FileStream(file_out, FileMode.Open)) {
                    using (BinaryWriter writer = new BinaryWriter(stream)) {
                        long pos = writer.Seek (0x237, SeekOrigin.Begin);
                        if (pos != 0x237) {
                            Console.WriteLine ("rb3pkg: Error unlocking " + file_out);
                        }
                        else {
                            writer.Write (1);
                            writer.Close ();
                            stream.Close ();
                            Console.WriteLine ("File unlocked succesfully.");
                        }
                    }
                }
            }

            rec = new X360.Other.LogRecord();
            pkg = new STFSPackage(file_out, rec);
            if (!pkg.ParseSuccess)
            {
                Console.WriteLine("Error reading back the package to fix.");
            }
            else
            {
                RSAParams secondParams = new RSAParams(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location) + "/KV.bin");
                if (secondParams.Valid)
                {
                    pkg.FlushPackage(secondParams);
                }
                pkg.CloseIO();
            }
            foreach (string s in rec.Log)
            {
                Console.WriteLine(s);
            }

            Console.WriteLine ("Done!");
        }

        /**
         * Recursively adds all the files/folders in source_dir to the package
         * at the location destination_dir.
         */
        public static void addFiles (CreateSTFS xsession, string source_dir, string destination_dir)
        {
            string file_prefix = destination_dir == "" ? "" : destination_dir + "/";

            foreach (string file in Directory.GetFiles (source_dir)) {
                string package_file = file_prefix + Path.GetFileName (file);
                Console.WriteLine ("Adding file " + package_file);
                xsession.AddFile (file, package_file);
            }

            foreach (string dir in Directory.GetDirectories (source_dir)) {
                string new_destination_dir = file_prefix + Path.GetFileName (dir);
                xsession.AddFolder (new_destination_dir);
                addFiles(xsession, dir, new_destination_dir);
            }
        }
    }
}
