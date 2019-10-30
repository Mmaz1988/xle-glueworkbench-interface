# xle-glueworkbench-interface
Illustrates how the Glue semantics workbench can be called from XLE. 
The project contains a sample xle grammar that encodes Glue premises in the f-structure.
The folder /src contains prolog procedures that translate the Glue premises in an f-structure to strings
that are formatted such that they can be read by the Glue semantics workbench. 
The file glue.tcl adds a command to the XLE GUI that allows you to run the PROLOG procedures and the Glue semantics workbench and return the result.

# WARNING
The current version is simply a toy example to illustrate how it can be done. 
The system has been developed with an OS based on UNIX in mind. 

# Requirements
- This project requires an installation of XLE. It is designed to be version independent, but feedback is welcome.
- SWI-Prolog (latest stable release should do the trick) is required for translating f-structure premises to premise strings
- Glue semantics workbench .jar file. The best way to get this is by cloning https://github.com/Mmaz1988/GlueSemWorkbench_v2 and 
producing your own .jar file. Since the project has been created with IntelliJ, I recommend to use that editor to build the project.
Alternatively, [here](https://ling.sprachwiss.uni-konstanz.de/pages/home/zymla/glueSemWorkbench2.jar) (Send a reminder if it's not up to date). 

# Running the system
- Download the project to your computer. No specific path is necessary, as long as you have properly set up XLE and SWI-Prolog. This means, you should have added the relevant binaries to your $PATH file, for example, in .bash_profile. For further assistance refer to the respective documentations.

- Put the GSWB .jar file in the top-level project folder, or specify the path in src/glue.tcl, where "Run Java Glue prover" is written.
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
% parse "Tracy yawned"
parsing {Tracy yawned}
2 solutions, 0.000 CPU seconds, 0.000MB max mem, 24 subtrees unified
```
The f-structure window "Commands" menu should now contain an entry called "Semantics". Clicking on this button should generate a window which contains the result of the Glue derivation. 

![alt text](fstructure.png)


