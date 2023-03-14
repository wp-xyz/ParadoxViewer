# ParadoxViewer

This little application is a viewer for ancient PARADOX database files. 
Using the directory tree navigate to the folder with the PARADOX file to be viewed, and select the file in the file list. 
The records will be displayed in the tab "Data" in a DBGrid, and the field structure is listed in the tab Fields.
The table can be exported as an SQLite3 file for usage in modern programs.

NOTE: This program does not allow editing the PARADOX data files.

ParadoxViewer works on Windows both as a 32-bit or 64-bit application. Other operating systems were not tested, but should work, too. An external DLL is not required.

Compile the program with a recent Lazarus (2.08, FPC 3.04, or newer). 

The reader unit for the Paradox files is copied from the Lazarus Component and Code Repository (https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/tparadoxdataset/). It is no longer required to install the package because the component is created at runtime.
