# xle-glueworkbench-interface
Illustrates how the Glue semantics workbench (GSWB) can be called from XLE. 
The project contains a sample xle grammar that encodes Glue premises in the f-structure.
The folder /src contains prolog procedures that translate the Glue premises in an f-structure to strings
that are formatted such that they can be read by the Glue semantics workbench. 
The file glue.tcl adds a command to the XLE GUI that allows you to run the PROLOG procedures and the Glue semantics workbench and return the result.

Confirmed functional* for:

- MacOS 10.13.6 (High Sierra) 
- Ubuntu 16.04
- Ubuntu 14.04 
- Windows 7



*Given appropriate SWI-Prolog installation and either Oracle Java 1.8 (or higher) or OpenJDK 13 (or OpenJDK > 8) 

# WARNING
- The current version is simply a toy example to illustrate how it can be done. 
- This system is developed on MacOS. Please report compatibility issues with other platforms 

# Requirements
- This project requires an installation of XLE. It is designed to be version independent, but feedback is welcome.
- SWI-Prolog is required for translating f-structure premises to premise strings (The latest stable release is recommended; SWI-Prolog 6.x and older are not compatible with the present system. Currently, there is an issue with the SWI-Prolog website where the download site is marked as dangerous by some browsers).
- Glue semantics workbench .jar file. The best way to get this is by cloning https://github.com/Mmaz1988/GlueSemWorkbench_v2 and 
producing your own .jar file. Since the project has been created with IntelliJ, I recommend to use that editor to build the project.
Alternatively, the .jar file can be downloaded [HERE](https://ling.sprachwiss.uni-konstanz.de/pages/home/zymla/glueSemWorkbench2.jar) (Send a reminder if it's not up to date). 
- The GSWB jar file requires Java. [OpenJDK](https://jdk.java.net/13/) for Mac works with the jar file made available above. I appreciate feedback for other platforms.

# Running the system
- Download the project to your computer. No specific path is necessary, as long as you have properly set up XLE and SWI-Prolog. This means, you should have added the relevant binaries to your $PATH file, for example, in .bash_profile. For further assistance refer to the respective documentations.

- Put the glueSemWorkbench2.jar file in the top-level project folder, or specify the path in src/glue.tcl, where "Run Java Glue prover" is written.
Replace glueSemWorkbench2.jar with some/directory/glueSemWorkbench2.jar 

```
	#Run Java Glue prover; jar file relative to execution as above 
	eval exec java [list -jar glueSemWorkbench2.jar \
			-i $outputfile -o $displayfile]
```

The next step is to navigate to the top-level folder of the project via the command line (the top level folder contains the xlerc file). 
Then simply run XLE. The result should looke something like the following: 

```
lap0987:gluegrammar red_queen$ xle
XLE loaded from xle.
XLEPATH = /Applications/xle-2009-09-18.
Copyright (c) 1993-2001 by the Xerox Corporation and
Copyright (c) 2002-2009 by the Palo Alto Research Center.
All rights reserved. This software is made available AS IS,
and PARC and the Xerox Corporation make no warranty about
the software, its performance or its conformity to any specification.
XLE version 2.6.5 (built Sep 16, 2009 16:07 -0700)
Type 'help' for more information.
loading /Users/red_queen/Projects/gluegrammar/glue-basic.lfg...
Grammar has 8 rules with 22 states, 21 arcs, and 21 disjuncts (21 DNF).

(re)Indexing lexicon section ( ENGLISH BASIC ) for  parse 
MORPHOLOGYCONFIGFILE = /Applications/xle-2009-09-18/bin/default-morph-config.

Morph transducer files relative to /Applications/xle-2009-09-18/bin/

0.010 CPU seconds
/Users/red_queen/Projects/gluegrammar/glue-basic.lfg loaded
Grammar last modified on Aug 28, 2019 09:30.
/Users/red_queen/Projects/gluegrammar/xlerc loaded.
% 
```
If this suceeds you can simply parse a sentence as usual in XLE.

```
% parse "a man yawned"
parsing {a man yawned}
2 solutions, 0.000 CPU seconds, 0.000MB max mem, 26 subtrees unified
```
The f-structure window "Commands" menu should now contain an entry called "Semantics". Clicking on this button should generate a window which contains the result of the Glue derivation including the underlying premises and their compiled counterpart, the agenda. Note that the example below uses pseudo-semantics to illustrate the functionality of the workbench.

# Command 

![alt text](fstructure.png)

# Expected output

![alt text](semantics.png)
