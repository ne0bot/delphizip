
			Welcome to the Delphi Zip v1.9.1
                   This is the Delphi Edition for version 4 and later only
					April 18, 2012 (updated 5 March 2013)
						 
Major Changes

Delphi versions supported
 	not before 4

Directory Structure
  ZipMaster files
  Delphi - design- and run-time page files
  Demos  - Demos and SortGrid
  Dll - dll
  DLL\Source - Source code for dll (Available separately
  Docs 		- where this file resides
  Help 		- help files
  Res		- resource files for connecting to the application or SFX
  Res\Lang	- Language files
  Tools - updaters for language resource strings

Configuration
  In Source/ZipConfig191.inc 
   { $DEFINE USE_COMPRESSED_STRINGS}  // define to use compressed strings instead of 'ResourceString's. 
   { $DEFINE STATIC_LOAD_DELZIP_DLL}  // define to statically bind the dll
  

The required Delphi source files (files marked with '+' are written by ZipResMaker.exe)
    ...\
            ZipMstr.pas		 - main component source
            ZipMstr.res		 - components version resource 
            ZipVers.inc		 - required defines for Delphi versions (only 4..10 supported)
            ZipConfig191.inc - sets compile options 
			ZMArgSplit		 - used by MergeZippedFiles to 'read' command arguments
			ZMCenDir.pas	 - allows external connections to the internal central entries
			ZMCentral.pas	 - represents the 'Central Directory' of a zip file
			ZMCompat.pas	 - support for older compilers
            ZMCtx.pas		 - Dialog box help context values
            ZMCore.pas		 - basic support functions and event triggering
+           ZMDefMsgs.pas	 - default message strings and tables
            ZMDelZip.pas	 - dll interface definitions
            ZMDlg.pas		 - dialog box support
            ZMDllLoad.pas	 - dynamically loads and binds the dll 
            ZMDllOpr.pas	 - operations that require the dll
            ZMDrv.pas		 - Handles drive level parameters and methods
            ZMEOC.pas		 - represents and finds, reads, writes the zip end of central structures
            ZMHash.pas	 	 - hash table functions
			ZMInflt.pas		 - used internally to 'inflate' deflated resources
            ZMIRec.pas		 - representation and functions on zip central entries
			ZMLister.pas	 - loads and controls the zip file specified
            ZMMatch.pas		 - wildcard file spec matching
			ZMModOpr.pas	 - operations that modify the zip file
+           ZMMsg.pas		 - message values
            ZMMsgStr.pas	 - handles message string storage and language selection
			ZMReg.pas		 - registers design package (XE* only)
            ZMSFXInt.pas	 - SFX stub interface structures and definitions
            ZMStructs.pas	 - definition of internal zip structures 
            ZMUTF8.pas		 - functions for handling UTF8/UTF16
            ZMUtils.pas		 - some functions to make life easier
            ZMWFuncs.pas	 - support 'wide' file names and paths for non-Unicode Delphi
            ZMWorkFile.pas	 - primitive support for files
            ZMWZip.pas		 - basic file support for zip files
            ZMXcpt.pas		 - EZipMaster definitions
            ZMZipFile.pas	 - handles read and writing a zip file
			ZMZippedOpr.pas	 - operations copying or merging zipped files

    ...\RES\
+          	ZMRes191_str.rc	 - resource script for compressed languages and dll
+          	ZMRes191_str.res	 - compiled resource for applications using ZipMaster (link to application)
			ZMRes191_sfx.rc	 - resource script for including sfx stub
			ZMRes191_sfx.res	 - compiled resource for including sfx stub (link to application) 
			ZMRes191_dll.rc	 - resource script for including compressed dll
			ZMRes191_dll.res	 - compiled resource for including compressed dll (link to application)(optional)
			ZMSFX191.bin		 - Ansi sfx stub
			ZMSFXU191.bin		 - Unicode sfx stub (requires XP or later)DefStr.bin
			
	...\SFX\
			DefStr.bin			- Default (US) strings (compressed)
			ZMSFX191.dpr		- Pre D2009 stub project
			ZMSFX191.exe		- prebuilt 'raw' stub (use ZipResMaker to prepare)
			ZMSFXDefs.pas		- defines, constants
			ZMSFXDialogs.pas	- code handling for the various dialogs
			ZMSFXDLG.rc			- resource script for dialogs
			ZMSFXDLG.res		- compiled resources for dialogs
			ZMSFXInflate.pas	- inflate code for use within the stub
			ZMSFXProcs.pas		- the main stub code
			ZMSFXStrings.pas	- strings and string handling
			ZMSFXStructs.pas	- various structures used
			ZMSFXU191.dpr		- D2009+ stub project (XP+ only)
			ZMSFXU191.exe		- prebuilt stub
			ZMSFXU_ver.rc		- resource script
			ZMSFXU_ver.res		- compiled resources
			ZMSFXVars.pas		- variables
			ZMSFXWinTrust.pas	- checks signed exe
			ZMSFX_ver.rc		- resource script
			ZMSFX_ver.res		- resources			

    ...\Packages\ 
            ZMstr190D?.bpk   - design and run-time package (? is compiler version)
    ...\Packages\XE*
			ZipMaster.groupproj - project group file
			ZipMasterR.dpk		- run-time package file
			ZipMasterR.dproj	- run-time package file
			ZipMasterR.res		- run-time package resources
			ZipMasterD.dpk		- design package file
			ZipMasterD.dproj	- design package file
			ZipMasterD.res		- design package resources

    ...\DLL\
            DelZip190.dll	 - required dll
			DelZip64.dll	 - dll for x64

    ...\DOCS\
            licence.txt		 - a copy of the licence
			ReadMe.txt
			Install.txt
			Debug.txt
            ZipMaster.chm	 - compile html file
            dzsfx.chm		 - SFX help file
			
    ...\LANGS\
            ZipMsg.h		 - master message identifier header file
            ZipMsgUS.rc		 - master message script
            ZipMsg??.rc		 - resource language script files
            ZipMsg??.res	 - compiled language resource file
            SFX??.txt		 - language files for sfx

    ...\HELP\
            ZipMaster.hlp	 - compiled help file (Delphi 7)

    ...\HELP\SOURCE\		 - source files for help

    ...\DEMOS\DEMO1\		 - zip adder/extractor

    ...\DEMOS\DEMO2\		 - quick add/extract and dll test

    ...\DEMOS\DEMO3\		 - another add/extract example

    ...\DEMOS\DEMO4\		 - simple self installer

    ...\DEMOS\DEMO5\		 - make exe file (sfx)

    ...\DEMOS\DEMO6\		 - span multiple disks

    ...\DEMOS\DEMO7\		 - extract from stream

    ...\DEMOS\DEMO9\		 - use in thread

    ...\DEMOS\SortGrid\		 - (optional) sort grid component (used in some Demos)
            SortGrid.pas	 - 
            SortGrid.res	 - 
            SortGrid.dcr	 - 
            SortGridreg.pas	 - 
            SortGridPreview.pas	 - 
            SortGridPreview.dfm	 - 

      
     
                      License
                               
	This component is subject to a variation of the BSD 3-Clause license
	 (http://www.opensource.org/licenses/BSD-3-Clause)
     as explained in full in the Help file and in licence.txt.

 
                     DLL Source Code in C 
            
        The DLL source code is distributed separately due to it's
     size, and the fact that most Delphi users of this package
     probably don't want the C source for the DLL's.  The DLL 
     source is also freeware, and will remain that way. 
     The DLL source code needs Borland C++ Builder v5 or later.
     
     
                     Problem Reports or Suggestions
     
     We DO want to hear your ideas!  If you find a problem with
     any part of this project, or if you just have an idea for
     us to consider, send us e-mail!
     
     But, please make sure that your bug has not already been
     reported.  Check the "official" web site often:
     
     Latest Versions and changes available at
     http://www.delphizip.org/index.html
     
     Problems
     please report any to 
     problems@delphizip.org
     
     Amended and updated by
     R.Peters 
     
