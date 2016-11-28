/*
 _________
/         \ hello.c v2.7.2 [November 23, 2016] zlib licence
|tiny file| Hello World file of "tiny file dialogs" created [November 9, 2014]
| dialogs | Copyright (c) 2014 - 2016 Guillaume Vareille http://ysengrin.com
\____  ___/ http://tinyfiledialogs.sourceforge.net
     \|           	                     mailto:tinyfiledialogs@ysengrin.com

            git://git.code.sf.net/p/tinyfiledialogs/code

Please
	1) let me know
	- if you are including tiny file dialogs,
	  I'll be happy to add your link to the list of projects using it.
	- If you are using it on different hardware / OS / compiler.
	2) Be the first to leave a review on Sourceforge. Thanks.

tiny file dialogs (cross-platform C C++)
InputBox PasswordBox MessageBox ColorPicker
OpenFileDialog SaveFileDialog SelectFolderDialog
Native dialog library for WINDOWS MAC OSX GTK+ QT CONSOLE & more

A single C file (add it to your C or C++ project) with 6 boxes:
- message / question
- input / password
- save file
- open file & multiple files
- select folder
- color picker.

Complements OpenGL GLFW GLUT GLUI VTK SFML SDL Ogre Unity ION
CEGUI MathGL CPW GLOW IMGUI GLT NGL STB & GUI less programs

NO INIT
NO MAIN LOOP

The dialogs can be forced into console mode

Windows (XP to 10) [ASCII + MBCS + UTF-8 + UTF-16]
- native code & some vbs create the graphic dialogs
- enhanced console mode can use dialog.exe from
http://andrear.altervista.org/home/cdialog.php
- basic console input

Unix (command line call attempts) [ASCII + UTF-8]
- applescript
- zenity / matedialog
- kdialog
- Xdialog
- python2 tkinter
- dialog (opens a console if needed)
- basic console input
The same executable can run across desktops & distributions

tested with C & C++ compilers
on VisualStudio MinGW Mac Linux Bsd Solaris Minix Raspbian C# fortran (iso_c)
using Gnome Kde Enlightenment Mate Cinnamon Unity
Lxde Lxqt Xfce WindowMaker IceWm Cde Jds OpenBox

bindings for LUA and C# dll

- License -

This software is provided 'as-is', without any express or implied
warranty.  In no event will the authors be held liable for any damages
arising from the use of this software.

including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
claim that you wrote the original software.  If you use this software
in a product, an acknowledgment in the product documentation would be
appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
*/


/*
- Here is the Hello World:
    if a console is missing, it will use graphic dialogs
    if a graphical display is absent, it will use console dialogs
		(on windows the input box may take some time to open the first time)
*/


#include <stdio.h>
#include <string.h>
#include "tinyfiledialogs.h"
int main(void)
{
	char const * lTmp;
	char const * lTheSaveFileName;
	char const * lTheOpenFileName;
	char const * lTheSelectFolderName;
	char const * lTheHexColor;
	char const * lWillBeGraphicMode;
	unsigned char lRgbColor[3];
	FILE * lIn;
	char lBuffer[1024];
	char lThePassword[1024];
	char const * lFilterPatterns[2] = { "*.txt", "*.text" };

	lWillBeGraphicMode = tinyfd_inputBox("tinyfd_query", NULL, NULL);

#ifdef _MSC_VER
#pragma warning(disable:4996) /* silences warning about strcpy strcat fopen*/
#endif

	if (lWillBeGraphicMode)
	{
		strcpy(lBuffer, "graphic mode: ");
	}
	else
	{
		strcpy(lBuffer, "console mode: ");
	}

	strcat(lBuffer, tinyfd_response);
	strcpy(lThePassword, "tinyfiledialogs v");
	strcat(lThePassword, tinyfd_version);
	tinyfd_messageBox(lThePassword, lBuffer, "ok", "info", 0);

	if ( lWillBeGraphicMode && ! tinyfd_forceConsole )
	{
		tinyfd_forceConsole = tinyfd_messageBox("Hello World",
			"force dialogs into console mode?\
				\n\t(it is better if dialog is installed)",
				"yesno", "question", 0);
	}

	lTmp = tinyfd_inputBox(
		"a password box", "your password will be revealed", NULL);

	if (!lTmp) return 1 ;

	/* copy lTmp because saveDialog would overwrites
	inputBox static buffer in basicinput mode */

	strcpy(lThePassword, lTmp);

	lTheSaveFileName = tinyfd_saveFileDialog(
		"let us save this password",
		"passwordFile.txt",
		2,
		lFilterPatterns,
		NULL);

	if (! lTheSaveFileName)
	{
		tinyfd_messageBox(
			"Error",
			"Save file name is NULL",
			"ok",
			"error",
			1);
		return 1 ;
	}

	lIn = fopen(lTheSaveFileName, "w");
	if (!lIn)
	{
		tinyfd_messageBox(
			"Error",
			"Can not open this file in write mode",
			"ok",
			"error",
			1);
		return 1 ;
	}
	fputs(lThePassword, lIn);
	fclose(lIn);

	lTheOpenFileName = tinyfd_openFileDialog(
		"let us read the password back",
		"",
		2,
		lFilterPatterns,
		NULL,
		0);

	if (! lTheOpenFileName)
	{
		tinyfd_messageBox(
			"Error",
			"Open file name is NULL",
			"ok",
			"error",
			1);
		return 1 ;
	}

	lIn = fopen(lTheOpenFileName, "r");

#ifdef _MSC_VER
#pragma warning(default:4996)
#endif

	if (!lIn)
	{
		tinyfd_messageBox(
			"Error",
			"Can not open this file in read mode",
			"ok",
			"error",
			1);
		return(1);
	}
	lBuffer[0] = '\0';
	fgets(lBuffer, sizeof(lBuffer), lIn);
	fclose(lIn);

	tinyfd_messageBox("your password is",
			lBuffer, "ok", "info", 1);

	lTheSelectFolderName = tinyfd_selectFolderDialog(
		"let us just select a directory", NULL);

	if (!lTheSelectFolderName)
	{
		tinyfd_messageBox(
			"Error",
			"Select folder name is NULL",
			"ok",
			"error",
			1);
		return 1;
	}

	tinyfd_messageBox("The selected folder is",
		lTheSelectFolderName, "ok", "info", 1);

	lTheHexColor = tinyfd_colorChooser(
		"choose a nice color",
		"#FF0077",
		lRgbColor,
		lRgbColor);

	if (!lTheHexColor)
	{
		tinyfd_messageBox(
			"Error",
			"hexcolor is NULL",
			"ok",
			"error",
			1);
		return 1;
	}

	tinyfd_messageBox("The selected hexcolor is",
		lTheHexColor, "ok", "info", 1);

	return 0;
}

/*
OSX :
$ gcc -o hello.app hello.c tinyfiledialogs.c

UNIX :
$ gcc -o hello hello.c tinyfiledialogs.c
( or clang tcc cc CC )

MinGW (needs gcc >= v4.9 otherwise some headers are incomplete):
> gcc -o hello.exe hello.c tinyfiledialogs.c -LC:/mingw/lib -lcomdlg32 -lole32
(unfortunately some headers are missing with tcc)

VisualStudio :
	Create a console application project,
	it links against Comdlg32.lib & Ole32.lib.
*/
