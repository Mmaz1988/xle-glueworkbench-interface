# xle-glueworkbench-interface
Illustrates how the Glue semantics workbench can be called from XLE. 
The project contains a sample xle grammar that encodes Glue premises in the f-structure
The folder /src contains prolog procedures that translate the Glue premises in an f-structure to Strings
that are formatted such that they can be read by the Glue semantics workbench. 

# Requirements
- This project requires an installation of XLE. It is designed to be version independent, but feedback is welcome.
- SWI-Prolog (latest stable release should do the trick) is required for translating f-structure premises to premise strings
- Glue semantics workbench .jar file. The best way to get this is by cloning https://github.com/Mmaz1988/GlueSemWorkbench_v2 and 
producing your own .jar file. Since the project has been created with IntelliJ, I recommend to use that editor to build the project.
Alternatively, contact me for a .jar file of the most recent version of the GSWB.

# Running the system
- Put the GSWB .jar file in the top-level project folder, or specify the path in src/glue.tcl, where "Run Java Glue prover" is written.

'''
    eval exec java [list -jar glueSemWorkbench2.jar \
			-i $outputfile -o $displayfile]
'''

