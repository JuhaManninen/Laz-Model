This is a first cut to port ESS-Model to Lazarus and fpc.


Primary goals met on this iteration.

1: Produces Diagrams on screen with common mouse handling across Windows
Gnome and QT.
2: Has no other package dependancies than LCL.
3: Has no code which requires {$mode delphi} all files have {$mode objfpc}{$H+}
   to have a common a base as possible for cross platform.
4: Capable of parsing itself.
   ( parser is old, not expected to parse later dialects in first port)

Casualties .. functionality removed

html generation (violates 2: requires other units/libraries )
Delphi integration module ( not required )
WMF generation  (violates 2: requires other units/libraries )

Casualties ..

The Good:
1: Still as fast as delphi binary (at least on windows.)
2: Runs in Linux (only one {$ifdef LINUX} was required.)
3: Internal workings would suit event emitting style parser.
4: Code position is stored with model elements.
5: Suited to single file parsing.
6: It has scratched my itch. (To have something to view large packages,
   primarily GLScene)
7: Does not crash parsing modern pascal variants
   (throws lots of errors but does not crash)
8: Nothing else added at this point so a base for possibilities.
9: Reads .lpr for project (use read directory for packages).
10: Does what the windows binary does ( casualaties excepted ).
11: Does not leak mem.

The Bad.
1: Parser is old.
2: xmi is old, not found anything modern that will see the xmi as valid.
3: Missing key model features (Enums, Aggregation, Composition to name a few)
4: Crashes through dangling pointers to badly parsed entities.
   (Solution FIX THE PARSER don't add exception handling)
5: Mouse handling may not be optimal in Gnome as lots of calls to get parents
   of control under mouse, original program relied on mouse events not being
   caputured by owner drawn labels but used windows specific mechanisms which
   are not portable. I run my Linux test boxes in VMs so may be ok on bare
   metal.
6: Dragging when view is scrolled, original suffered this also.





ORIGINAL README BELOW

"ESS-Model is a powerful, reverse engine, UML-tool for
Delphi/Kylix and Java-files."



ESS-Model used to be an commercial product but is now
released as an Open Source under the GPL.

Note about compiling the source:


The source is written in Delphi 6.

We started developing an Kylix version, about 90% of the code
is platform independent.

The following conditional defines can be set:

PNG_SUPPORT - Define this to use the free PNG-component which
can be downloaded from: http://pngdelphi.sourceforge.net/

DRAG_SUPPORT - Define this to use the free Drag and drop component
which can be downloaded from: http://www.melander.dk/delphi/dragdrop/

Drag and drop components are written by Anders Melander. Unfortunately
his homepage has been down for a while now, so here are some alternative
links:

   http://www.peertropolis.de/download/delphi.htm
   http://www.torry.net/gif.htm
   http://www.torry.net/draganddrop.htm


ARGO_XMI - Define this to use Argo UML compatible xmi-format.






What's new
----------

2.2

  + Changed diagram bitmap format from .GIF to .PNG
  + Now supports Microsoft XML Parser 4.
  + 'Open folder' function for parsing large java-projects.
  + Fixed bug in delphi parser for projects with many subdirectories
  + Fixed memory leak when saving bitmaps
  + Changed scrollbehaviour when dragging items to the edge of the diagram window.


2.1  

  + Fixed bug in .java-parser, attributes have correct visibility.
  + Fixed bug in .class-parser to correctly identify abstract methods.
  + .Java / .class parsers now handles interface attributes
  + Translated more source code comments to english
  + New IFDEF: ARGO_XMI for using Argo UML compatible xmi-format
  + Settings for default diagram options
  + Command-line control of diagram options
  + Abstract classes now have class name shown in italics


2.0 
  
  + Initial Open Source release.
  





ESS-Model
---------

o  Introduction
o  Installation
o  Command line options
o  Explorer and Delphi integration



Introduction
------------
ESS-Model is a powerful, reverse engine, UML-tool for Delphi/Kylix and Java-files.

The easiest way to get class diagrams from source code.

Easy to use, lightning fast and with automatic documentation
possibilities. 

Features:

- Automatic generation of UML-standard class diagrams from your source.
- Lightning fast installation and execution, only 1 exe-file needed (approx.700Kb)
- Delphi/Kylix (.dpr, .pas) and Java (.class, .java) parser.
- Easy Drag-n-Drop user interface.
- Easy integration into Delphi/Kylix.
- Full control from command-line.
- JavaDoc-style HTML-documentation. 
- XMI-export features. 
 


Installation
------------
You do not need to run an separate installation program.

Simply extract ESSMODEL.EXE from the zip file to the directory 
you want to put it in. 

Then doubleclick on the programicon to start ESS-Model.

No other files are needed for the free version. 

To use the HTML documentation feature in the you need the library 
MSXML.DLL, normally installed  with Microsoft's Internet Explorer 
5 (IE5) or later.

Please note that you need version 4.0 of this library.

You can obtain the file as part of the XML Parser package from 
the Microsoft Download Center at:

   http://www.microsoft.com/downloads/search.asp

Choose the "Keyword Search" option, then search for "MSXML".

Or try this link:

  http://msdn.microsoft.com/xml



Command line options
--------------------
ESS-Model can be controlled from the command line.

Syntax: essmodel [options] [@list] [files...]

The following options are available:
  -help       Show help for the options.
  -d[path]    Generate documentation to the path specified.
  -a[+/-]     Show associations on generated diagrams on/off.
  -v[0-3]     Visibilty filter for generated diagram. 0=Show all, 3=Hide all.
  -x[file]    Export model to xmi file.

Files can be specified with wildcards.

Examples:
  essmodel *.java subdir\*.java
  essmodel *.dpr
  essmodel myproj.dpr -dc:\myproj\doc         Parse and generate documentation

Examples of how to use in batchfiles:
  doDoc.bat
    rem Parse and generate documentation.
    essmodel MyProject.dpr -dC:\MyProject\doc
  
  doModel.bat
    rem Open essmodel with all java-files from current path and downwards.
    dir *.java /S /B > files.tmp
    essmodel @files.tmp
        

All the examples above assumes that essmodel.exe is in the current
search path. Otherwise you need to type the full path to essmodel, 
for instance c:\utils\esmodel.exe.




Diagram indicators for operations and attributes
------------------------------------------------
Italic font = Abstract
Green = Constructor
Red = Destructor
Black = Procedure  
Gray = Function
Plus-sign = Public
Minus-sign = Private
Hash-sign = Protected



Explorer and Delphi integration
-------------------------------
ESS-Model can be configured to run from the Explorer
contextmenu and from the Delphi IDE Tools menu.

From the main menu select File - Change settings.
Use the checkboxes to choose integration options.

When the shortcuts are active, ESS-Model will appear
as 'View as model diagram' in the menues.




Other info can be found at ESS-Model homepage: 


  http://www.essmodel.com
  http://www.eldean.se/essmodel
  http://www.sourceforge.net/projects/essmodel



