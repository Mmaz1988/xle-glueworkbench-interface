#.tcl file -- sets up XLE/Glue interface
#Author: Mark-Matthias Zymla
#Date: 07.11.2019

####################################################################
# Set up Glue menue
####################################################################

proc init-glue {{compiled 0}} {
    global defaultGlueParser
    #global defaultPrologFiles 

    #Create parser
    create-parser $defaultGlueParser
    #Add Glue menues to XLE GUI. Specified below. 
    create-glue-menus

    #COmment in when working with Sicstus-Prolog; not recommended    
    #foreach prologFile $defaultPrologFiles {
    #prolog "load_files('$prologFile')."}
    
}

####################################################################
# Set up menu items on f-structure windows 
####################################################################

#This simply adds a button to the XLE command menu in the GUI
#Calls the tcl function window-to-sem specified below
proc create-glue-menus {} {
    global fsCommands fsViews fsChartCommands fsChartViews 

    add-item-to-xle-menu \
	{command -label "Semantics" \
	     -command "window-to-sem 0 $self" \
	     -doc "Derives Glue semantics premises."} \
	fsCommands
}

    # Convert contents of window to semantics; display
    proc window-to-sem {packed window} {
	
	global semwindowtop
	global semDisplay

    set fsData [get-window-prop $window data]
   fsdata-to-premises ".semantics" $window  \
	          $semDisplay 522+$semwindowtop
}

#------------------------------------
#This function runs the pipeline from XLE to Glue representation
#It operates on an f-structure window $window
#displaywindow is simply the name of the window
#displaymode and position is for arranging the new window relative to the XLE GUI.

proc fsdata-to-premises {displaywindow window displaymode position} {
   
    global defaultTmpDir
    #For Sicstus Prolog
    #global defaultPrologFiles


    file mkdir tmp
    
    if {$displaymode == "window"} {
	set displayfile tmp/out_[pid].pl
    } else {
	set displayfile stdout
    }

    set prologfile tmp/default_[pid].pl
    set outputfile tmp/output_[pid].pl

    print-fs-as-prolog $prologfile $window

    puts "Generating $prologfile"

    #Put path of prolog file relative to where tcl command is executed
    #Nnot where tcl command is stored. 
    exec swipl -q -f  src/premises.pl -t "main." -- \
	"$prologfile" "$outputfile"

    puts "Generating $outputfile"


    #Run Java Glue prover; jar file relative to execution as above 
    eval exec java [list -jar glueSemWorkbench2.jar \
			-i $outputfile -o $displayfile]

    puts "Generating $displayfile"
    
    if {$displaymode == "window"} {
	display-file $displayfile $displaywindow $position {Courir 18}
       #Delete temporary files 
       file delete $prologfile
       file delete $outputfile
       file delete $displayfile
       file delete tmp
       puts "Temporary files are deleted after procedure is completed."
    }
}


