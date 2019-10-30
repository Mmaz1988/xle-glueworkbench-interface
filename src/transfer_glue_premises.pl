/* ------------------------------------------------------------------------ */
/* Project:   Transfer premises from f-structure to prover format.          */
/* Author:    Mary Dalrymple                                                */
/* File:      transfer_glue_premises.pl                                     */
/* Language:  SWI Prolog                                                    */
/* Unpack the f-structures, extract the premises from each f-structure,     */
/* and transfer them to the input format for the prover.                    */
/* ------------------------------------------------------------------------ */

:- ensure_loaded(extract_analysis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unpack the f-structures in Fstructure, extract the premises from each one,
% and transfer them to the input format for the prover.
extract_premises(Fstructure,Premises) :-
    % Extract all of the f-structures.
    bagof(Fstr, Cor^CVars^String^Props^Cstr^extract_analysis(Fstructure, f, Cor, CVars, String, Props, Fstr, Cstr), Fstrs),
    % Gather up the premises from this f-structure.  This will be
    % the members of the set value of the GLUE attribute throughout. 
    % Throw the rest of the f-structure away. Transfer the f-structure
    % representation of the premises to the input format for the prover.
    extract_premises_from_fstrs(Fstrs, Premises0),
    % Remove any duplicate premise sets.
    remove_duplicate_premise_sets(Premises0, Premises).

extract_premises_from_fstrs([FStr|FStrs], [P|Ps]) :-
    extract_premises_from_fstr(FStr, P),
    extract_premises_from_fstrs(FStrs, Ps).
extract_premises_from_fstrs([], []).

extract_premises_from_fstr(FStr, Premises) :-
    % Search the f-structure FStr and find all of the premises encoded as f-structures;
    % then turn them into the appropriate input format for the prover.
    find_premises(FStr, PremiseIDs),
    extract_premises(FStr, PremiseIDs, Premises).

find_premises(FStr, PremiseIDs) :-
    % Find the ID for the set value of the attribute 'GLUE'
    find_glue_attributes(FStr, GlueIDs),
    % Find the IDs for the members of the glue set
    collect_premise_ids(FStr, GlueIDs, PremiseIDs).

find_glue_attributes(FStr, GlueIDs) :-
    % Find all values of the attribute 'GLUE'.  
    bagof(GlueID, PremiseID^find_attr(FStr, PremiseID, 'GLUE', GlueID), GlueIDs).

collect_premise_ids(FStr, [GlueID|GlueIDs], PremiseIDs) :-
    ( % Collect the IDs of each member of the set values of the attribute 'GLUE'
	bagof(Premise, find_attr(FStr, GlueID, member, Premise), IDs1) ;
	% If the user has not made 'GLUE' a set-valued attribute,
	% assume that the value of 'GLUE' is a single premise, and 
	% return the value of the 'GLUE' attribute as a list
	IDs1 = [GlueID] ),
    collect_premise_ids(FStr, GlueIDs, IDs2),
    append(IDs1, IDs2, PremiseIDs).
collect_premise_ids(_FStr, [], []).

extract_premises(FStr, [PremiseID|PremiseIDs],[Premise|Premises]) :-
    convert_fstr_to_premise(FStr, PremiseID, Premise),
    extract_premises( FStr, PremiseIDs, Premises).
extract_premises(_FStr, [], []).

convert_fstr_to_premise(FStr, PremiseID, Premise) :-
    % Find the meaning side 
    find_attr(FStr, PremiseID, 'REL', Meaning),
    % Find the glue side
    find_glue(FStr, PremiseID, Glue),
    atomics_to_string([Meaning, " : ", Glue], Premise).

find_glue(FStr, PremiseID, Glue) :-
    find_glue_atom(FStr, PremiseID, Result),
    find_glue_args(FStr, PremiseID, Args),
    assemble_glue_expression(Args, Result, Glue).

% NOT FINISHED YET
find_glue_args(FStr, PremiseID, [Arg]) :-
    find_attr(FStr, PremiseID, 'ARG1', var(V)), !,
    find_glue(FStr, var(V), Arg).
find_glue_args(_FStr, _PremiseID, []).

assemble_glue_expression([], G, G).
assemble_glue_expression([Arg], G, Premise) :-
    atomics_to_string(["(", Arg, " -o ", G, ")"], Premise).
    
find_glue_atom(FStr, PremiseID, Term) :-
    % find a glue atom and its type corresponding to PremiseID, create term
    find_attr(FStr, PremiseID, 'SEMSTR', var(V)),
    find_attr(FStr, PremiseID, 'TYPE', Type),
    atomics_to_string([V, '_', Type], Term).

% F-structure FID has attributes Attr with value Val. 
find_attr([cf(_,eq(attr(FID,Attr),Val))|_FStr], FID, Attr, Val).
find_attr([cf(_,in_set(Val,FID))|_FStr], FID, member, Val).
find_attr([_|FStr], FID, Attr, Val) :-
    find_attr(FStr, FID, Attr, Val).

% Clean up the premises by removing duplicate premise sets, which could arise from syntactic
% ambiguity in the f-structure with no semantic effect.

remove_duplicate_premise_sets(PremiseSets0, PremiseSets) :-
    % Sort the individual sets of premises so that duplicates can be reliably detected
    sort_premises(PremiseSets0, PremiseSetsSorted),
    % Sorting the full list removes duplicate premise sets.
    sort(PremiseSetsSorted, PremiseSets).

sort_premises([PS0|PremiseSets0],[PS|PremiseSets]) :-
    % Sort the set of premises according to "standard order of terms" so that we can detect and
    % delete duplicate premise sets.  msort/2 does not remove duplicates.
    msort(PS0, PS),
    sort_premises(PremiseSets0, PremiseSets).
sort_premises([], []).



    
    

	



