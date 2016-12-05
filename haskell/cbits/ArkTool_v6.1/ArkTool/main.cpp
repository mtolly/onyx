// 
// This tool was made as an example of how to easily use my ark/hdr code.
// While it does do most of the available ark/hdr operations,
// it would be better to be able to string together a bunch of editing
// operations together and then save the changes in a GUI application.
// 
// Please credit me when using this source or decryption routines
// as it took my time and effort to work out the decryption :)
// 
// xorloser 2008
// 

#include "types.h"
#include <string.h>
#include "ArkHdrPair.h"

const char G_TITLE[] =	"ArkTool v6.1  -  xorloser 2008";
const char G_USAGE[] =	"Usage:    ArkTool <option> <parameters>\n"
						"Get:      ArkTool -g ark_dir somedir/somefile.ext externalfile.bin\n"
						"Add:      ArkTool -a ark_dir somedir/somefile.ext externalfile.bin\n"
						"Delete:   ArkTool -d ark_dir somedir/somefile.ext\n"
						"Replace:  ArkTool -r ark_dir somedir/somefile.ext externalfile.bin\n"
						"Print:    ArkTool -p ark_dir somedir/somefile.ext\n"
						"Extract:  ArkTool -e ark_dir somedir/somefile.ext external_dir\n"
						"NoCrypt:  ArkTool -x ark_dir somedir/somefile.ext external_dir\n"
						"\n"
						"* \"ark_dir\" is the dir that contains the ark and hdr files with their\n"
						"  original filenames (on your pc)."
						"* \"somedir/somefile.ext\" is the name of the file inside the ark.\n"
						"* \"externalfile.bin\" is the name of the file outside the ark (on your pc).\n"
						"* \"external_dir\" is the dir that files will be extracted to (on your pc.)\n"
						"\n"
						"Note: Print/Extract/NoCrypt work on multiple files using wildcards for the ark\n"
						"      filepath. You can use \"?\" or \"*\" to replace 1 or multiple letters."
						"Note: Extract extracts files and decrypts those that are encrypted.\n"
						"      NoCrypt extracts files without decrypting any.\n"
						"\n"
						"Note: \".\" can be used to specify the current directory for \"external_dir\".\n"
						"Note: \"*\" can be used to specify all files.";


int main(int argc, const char* argv[])
{
	printf("%s\n", G_TITLE);
	if(	argc < 3 )
	{
		printf("%s\n", G_USAGE);
		return 1;
	}
	
	// get which option to handle
	bool get_file		= (STRICMP("-g", argv[1]) == 0);
	bool add_file		= (STRICMP("-a", argv[1]) == 0);
	bool delete_file	= (STRICMP("-d", argv[1]) == 0);
	bool replace_file	= (STRICMP("-r", argv[1]) == 0);
	bool print_files	= (STRICMP("-p", argv[1]) == 0);
	bool extract_files	= (STRICMP("-e", argv[1]) == 0);
	bool extract_nocrypt= (STRICMP("-x", argv[1]) == 0);
	bool do_test		= (STRICMP("-t", argv[1]) == 0);
	char ark_filename[260];
	char ark_dirname[260];
	char ext_filename[260];
	char ext_dirname[260];
	strcpy(ark_dirname, argv[2]);
	strcpy(ark_filename, ((argc > 3) ? argv[3] : "*"));
	strcpy(ext_filename, ((argc > 4) ? argv[4] : "file.bin"));
	strcpy(ext_dirname,  ((argc > 4) ? argv[4] : "."));
	
	// check each option has the correct number of args
	if( (get_file		&& argc != 5) ||
		(add_file		&& argc != 5) ||
		(delete_file	&& argc != 4) ||
		(replace_file	&& argc != 5) ||
		(print_files	&& (argc != 3 && argc != 4)) ||
		(extract_files	&& argc != 5) ||
		(extract_nocrypt&& argc != 5) )
	{
		printf("%s\n", G_USAGE);
		return 1;
	}
	
	
	// *************************************
	// 
	// This shows how to extract a file from the ark/hdr to an external
	// file on your pc. The extracted file could then be edited before
	// being used to replace the existing version of that file in the ark/hdr.
	// 
	// *************************************
	if( get_file )
	{
		// first open the ark/hdr file
		ArkHdrPair arkhdr;
		if( !arkhdr.Open(ark_dirname) )
		{
			printf("Error opening ark/hdr files\n");
			return 2;
		}
		
		// get the file from the ark/hdr file
		bool perform_decrypts = true;
		if( !arkhdr.GetFile(ext_filename, ark_filename, perform_decrypts) )
		{
			printf("Error getting file\n");
			return 3;
		}
		
		// (ark/hdr file not altered, so no need to save any changes)
		// close the ark/hdr file
		arkhdr.Close();
		printf("Success getting %s into %s\n", ark_filename, ext_filename);
	}
	
	
	// *************************************
	// 
	// This shows how to add a new external file on your pc into the
	// ark/hdr files. Adding a new file won't work if a file already exists
	// with the given filename in the ark/hdr.
	// 
	// *************************************
	else if( add_file )
	{
		// first open the ark/hdr file
		ArkHdrPair arkhdr;
		if( !arkhdr.Open(ark_dirname) )
		{
			printf("Error opening ark/hdr files\n");
			return 2;
		}
		
		// add a file to the ark/hdr file
		bool perform_encrypts = true;
		if( !arkhdr.AddFile(ext_filename, ark_filename, perform_encrypts) )
		{
			printf("Error adding file\n");
			return 3;
		}
		
		// save the changes to the ark/hdr file
		if( !arkhdr.Save() )
		{
			printf("Error saving changes to ark/hdr files\n");
			return 4;
		}
		
		// close the ark/hdr file
		arkhdr.Close();
		printf("Success adding %s as %s\n", ext_filename, ark_filename);
	}
	
	
	// *************************************
	// 
	// This shows how to delete a file from the ark/hdr files.
	// 
	// *************************************
	else if( delete_file )
	{
		// first open the ark/hdr file
		ArkHdrPair arkhdr;
		if( !arkhdr.Open(ark_dirname) )
		{
			printf("Error opening ark/hdr files\n");
			return 2;
		}
		
		// delete a file from the ark/hdr file
		if( !arkhdr.RemoveFile(ark_filename) )
		{
			printf("Error deleting file\n");
			return 3;
		}
		
		// save the changes to the ark/hdr file
		if( !arkhdr.Save() )
		{
			printf("Error saving changes to ark/hdr files\n");
			return 4;
		}
		
		// close the ark/hdr file
		arkhdr.Close();
		printf("Success deleting %s from ark/hdr\n", ark_filename);
	}
	
	
	// *************************************
	// 
	// This shows how to replace an existing file in the ark/hdr files with
	// a new external file on your pc. In order to replace a file it must
	// exist in the ark/hdr files when you attempt to replace it.
	// 
	// *************************************
	else if( replace_file )
	{
		// first open the ark/hdr file
		ArkHdrPair arkhdr;
		if( !arkhdr.Open(ark_dirname) )
		{
			printf("Error opening ark/hdr files\n");
			return 2;
		}
		
		// replace a file in the ark/hdr file
		bool perform_encrypts = true;
		if( !arkhdr.ReplaceAFile(ext_filename, ark_filename, perform_encrypts) )
		{
			printf("Error replacing file\n");
			return 3;
		}
		
		// save the changes to the ark/hdr file
		if( !arkhdr.Save() )
		{
			printf("Error saving changes to ark/hdr files\n");
			return 4;
		}
		
		// close the ark/hdr file
		arkhdr.Close();
		printf("Success replacing %s as %s\n", ark_filename, ext_filename);
	}
	
	
	// *************************************
	// 
	// While this just prints out information about the files in the
	// ark/hdr file, it is a good example of how to iterate through all
	// the files contained in the ark/hdr files.
	// Note: The ArkHdrPair::Print function works this way internally,
	// i just copied the code from it to here as an iteration example.
	// 
	// *************************************
	else if( print_files )
	{
		// first open the ark/hdr file
		ArkHdrPair arkhdr;
		if( !arkhdr.Open(ark_dirname) )
		{
			printf("Error opening ark/hdr files\n");
			return 2;
		}
		
		// this stream could be pointed to an open file descriptor
		// in order to print into that file
		FILE* p_stream = stdout;
		int file_count = 0;
		fprintf(p_stream, "\n  Offset      Size  Path\n\n");
		
		// the same iterator object must be used during an iteration over file entries.
		// the same search path (in this case "ark_filename") must be used during an iteration too.
		FileEntrySetIter iter;
		for(const FileEntry* p_entry=arkhdr.First(iter, ark_filename);
			p_entry!=NULL;
			p_entry=arkhdr.Next(iter, ark_filename))
		{
			// now you can access all info about "p_entry" (your current file entry)
			fprintf(p_stream, "%8llX  %8llX  %s\n",
				p_entry->Offset(), p_entry->Arksize(),
				p_entry->Arkname() );
			file_count++;
		}
		fprintf(p_stream, "\n  %d files found\n", file_count);
		
		// close the ark/hdr file
		arkhdr.Close();
	}
	
	
	// *************************************
	// 
	// Extract multiple files from hdr/ark.
	// When this function is used it doesn't really allow you to
	// show the progress of the extraction to a user.
	// I would suggest to copy the code from ArkHdrpair::ExtractFiles
	// into your own code in order to show progress.
	// 
	// *************************************
	else if( extract_files || extract_nocrypt )
	{
		// first open the ark/hdr file
		ArkHdrPair arkhdr;
		if( !arkhdr.Open(ark_dirname) )
		{
			printf("Error accessing hdr/ark files\n");
			return 3;
		}
		
		// easily extract all files that fit the searchpath (ark_filename).
		// the searchpath can have "*" and "?" as parts of if.
		int file_count;
		bool perform_decrypts = !extract_nocrypt;
		FILE* p_stream = stdout;
		if( (file_count=arkhdr.ExtractFiles(ext_dirname, ark_filename, perform_decrypts, p_stream)) < 0 )
		{
			printf("Error extracting files\n");
			return 3;
		}
		
		// close the ark/hdr file
		arkhdr.Close();
		printf("Success extracting %d files\n", file_count);
	}
	
	
	// *************************************
	// 
	// ignore this: this bit is just used when for testing functionality
	// 
	// *************************************
	else if( do_test )
	{
/*
		// test of "save as" to ensure it works with and without "remove gaps"
		ArkHdrPair arkhdr;
		if( !arkhdr.Open(ark_dirname) )
		{
			printf("Error accessing hdr/ark files\n");
			return 3;
		}
		
		bool remove_gaps = false;
		if( !arkhdr.SaveAs(ext_dirname, remove_gaps) )
		{
			printf("Error removing gaps\n");
			return 3;
		}

		arkhdr.Close();
		printf("Success saving as\n");
*/
	}
	
	
	// done
	return 0;
}

