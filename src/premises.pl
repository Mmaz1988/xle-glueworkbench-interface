/* ------------------------------------------------------------------------ */
/* Project:   Transfer premises from f-structure to prover format.          */
/* Author:    Mary Dalrymple                                                */
/* File:      premises.pl                                                   */
/* Language:  SWI Prolog                                                    */
/* Top-level module.  Unpack the f-structures, extract the premises, and    */
/* transfer them to the input format for the prover.          */
/* ------------------------------------------------------------------------ */

:- ensure_loaded(transfer_glue_premises).

main :- 
 current_prolog_flag(argv,Argv),
 Argv = [X,Y|_],
 premises(X,Y).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Top-level function.
% Extract the premises from each f-structure in the packed structure
% in File, assemble them in a list, and write the result to PremiseFile.
% The name of PremiseFile is constructed from the input file name: input
% file X.pl has output premise file X_premises.txt.
premises(FStrFile,PremiseFile) :-
    open(FStrFile,read,Stream),
    read(Stream,FStr),
    extract_premises(FStr,PremiseSets),
%    atom_concat(Filename,'.pl',FStrFile),
%    atom_concat(Filename,'_premises.txt',PremiseFile),
    write_premise_sets(PremiseSets,PremiseFile).

write_premise_sets(PremiseSets,PremiseFile) :-
    open(PremiseFile,write,Stream),
    set_output(Stream),
    write_premise_sets(PremiseSets),
    close(Stream).

% Each premise set is enclosed in curly brackets.
write_premise_sets([Premises0|Premises]) :-
    write("{\n"),
    write_premises(Premises0),
    write("}\n"),
    !,
    write_premise_sets(Premises).
write_premise_sets([]).

write_premises([P|Ps]) :-
    write(P),nl,
    write_premises(Ps).
write_premises([]).




