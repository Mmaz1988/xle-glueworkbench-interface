#    Copyright (C) 2019–2020 Mark-Matthias Zymla
#
#    This file is part of XLE+Glue (https://github.com/Mmaz1988/xle-glueworkbench-interface).
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

####################################################################
# Set up Glue menu
####################################################################

proc init-glue {{compiled 0}} {
    global defaultGlueParser semParser transferDebug
    #global defaultPrologFiles 

    #Create parser
    create-parser $defaultGlueParser
    #Add Glue menus to XLE GUI. Specified below. 
    create-glue-menus

    #COmment in when working with Sicstus-Prolog; not recommended    
    #foreach prologFile $defaultPrologFiles {
    #prolog "load_files('$prologFile')."}


    if {$semParser == 1} {
	puts "Semantic parser is active."
    }
    if {$transferDebug == 1} {
	puts "Debug mode is active." 
    }
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
   
    global defaultTmpDir semParser transferDebug processDRT solutionOnly \
	outputfont fontsize
	
    
    #For Sicstus Prolog
    #global defaultPrologFiles


    file delete -force tmp
   
    file mkdir tmp

    set gswbfile tmp/gswb_[pid].pl	
    
    if {$displaymode == "window"} {
	set displayfile tmp/display_[pid].pl
    } else {
	set displayfile stdout
    }

    #Currently processed f-structure
    set prologfile tmp/default_[pid].pl
    #Output of the transfer component
    set outputfile tmp/transferOutput_[pid].pl

    print-fs-as-prolog $prologfile $window

  
        if {$transferDebug == 1} {
	    puts "#### Prolog F-structure: $prologfile ####"
	    puts \n 
	set fp [open $prologfile]
	set data [read $fp]

	puts $data
	puts "+------------------------------------------+"
        close $fp
        } else {
	   puts "Generating $prologfile"
	}

    #Put path of prolog file relative to where tcl command is executed
    #Not where tcl command is stored. 
    exec swipl -q -f  src/premises.pl -t "main." -- \
	"$prologfile" "$outputfile"

    
    if {$transferDebug == 1} {
	puts "#### Output of the Prolog Transfer System: $outputfile ####"
	puts \n
	set fp [open $outputfile]
	set data [read $fp]

	puts $data
	puts "+------------------------------------------+"
	close $fp
    } else {
	puts "Generating $outputfile"
    }


    #Set path of gswb
    set gswbpath "glueSemWorkbench2.jar"
    
    #Changing this line is not recommended 
    set inout "-i $outputfile -o $gswbfile"

    #Set up the command to call the GSWB in accordance with the parameters set in xlerc
    set gswb exec
    lappend gswb java
    lappend gswb -jar
    lappend gswb $gswbpath
    set gswb [concat $gswb $inout]
    if {$semParser == 1} {
	lappend gswb "-parseSem"
	lappend gswb "-inputStyle"
	lappend gswb  "0"
    } elseif {$semParser == 2} {
	lappend gswb "-inputStyle" 
	lappend gswb "1" 
    }
    if {$solutionOnly == 1} {
	lappend gswb "-s"
    }


    #Evaluates the command defined above
    eval $gswb 

    #Run Java Glue prover; jar file relative to execution as above
   # if {$semParser == 0} {
#	eval exec java [list -jar glueSemWorkbench2.jar \
#			    -i $outputfile -o $gswbfile -s]
#    } elseif {$semParser == 1} {
#	eval exec java [list -jar glueSemWorkbench2.jar \
#			    -i $outputfile -o $gswbfile -parseSem -s]
#    } elseif {$semParser == 2} {
#	eval exec java [list -jar glueSemWorkbench2.jar \
#			    -i $outputfile -o $gswbfile -prolog -s]
#   }

    
    if {$transferDebug == 1} {
	puts "#### Output of the Glue Semantics Workbench: $gswbfile ####"
	puts \n
	set fp [open $gswbfile]
	set data [read $fp]
	puts $data
	puts "+------------------------------------------+"
	close $fp
    } else {
	puts "Generating $gswbfile"
    }

    


    #Calls the DRT software to betareduce and pretty print the output by the gswb
     #   exec swipl -q -f  src/lambdaDRT.pl -t "main." -- \
     #"$gswbfile" "$displayfile"


    if {$processDRT == 1} {
	set prettydrtfile tmp/prettydrt_[pid].pl 
	set prettydrt [open $prettydrtfile "w"]
	set drtOutputFile tmp/drt_[pid].pl	
	
	set pipe [open |[list swipl -q -f  src/lambdaDRT.pl -t "main." -- \
			     "$gswbfile" "$drtOutputFile" ] "r"]
	while {[gets $pipe line] >= 0} {
	    puts -nonewline $prettydrt "$line\n"
	}

    #For some reason, the pipe fails to close, when lambdaDRT.pl fails
	if [catch {close $pipe} msg ] {
	    puts "\nFailed to read output from lambdaDRT.pl!\n" 
	}

	close $prettydrt

        if {$transferDebug == 1} {
	    puts "#### Output of the DRT software: $prettydrtfile ####"
	    puts \n
	    set fp [open $prettydrtfile]
	    set data [read $fp]
	    puts $data
	    puts "+------------------------------------------+"
	    close $fp
	} else {
	    puts "Generating $prettydrtfile"
	}
	file delete $drtOutputFile
	set displayfile $prettydrtfile
    } else {
	set displayfile $gswbfile
    }

    
    
    if {$displaymode == "window"} {
	display-file $displayfile $displaywindow $position "$outputfont $fontsize"
	#Delete temporary files

	if {$transferDebug == 0} {
	    file delete $prologfile
	    file delete $outputfile
	    file delete $displayfile
#	    file delete $drtOutputFile
	    file delete $prettydrtfile
	    file delete $gswbfile
	    file delete -force tmp
	    puts "Temporary files are deleted after procedure is completed."
	} else {
	    puts "Temporary files are stored in the /tmp folder."
	}
    }
}


