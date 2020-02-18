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
    % Transfer the premises encoded in each f-structure.
    extract_premises_from_fstrs(Fstrs, Premises0),
    % Remove any duplicate premise sets.
    remove_duplicate_premise_sets(Premises0, Premises).

extract_premises_from_fstrs([FStr|FStrs], [P|Ps]) :-
    extract_premises_from_fstr(FStr, P),
    extract_premises_from_fstrs(FStrs, Ps).
extract_premises_from_fstrs([], []).

extract_premises_from_fstr(FStr, Premises) :-
    % Search the f-structure FStr and find all of the premises encoded as f-structures.
    % These will be value of the GLUE attribute throughout, or the members of the set
    % value of the attribute.  Turn them into the appropriate input format for the prover.   
    % Throw the rest of the f-structure away.  
    find_premises(FStr, PremiseIDs),
    extract_premises(FStr, PremiseIDs, Premises).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find the f-structure IDs for the values of the attribute 'GLUE', which may be a set.
find_premises(FStr, PremiseIDs) :-
    % Find the ID for the value of the attribute 'GLUE'
    find_glue_attributes(FStr, GlueIDs),
    % Find the IDs for the f-structure premises
    collect_premise_ids(FStr, GlueIDs, PremiseIDs).

find_glue_attributes(FStr, GlueIDs) :-
    % Find all values of the attribute 'GLUE'.  
    bagof(GlueID, PremiseID^find_attr(FStr, PremiseID, 'GLUE', GlueID), GlueIDs).

collect_premise_ids(FStr, [GlueID|GlueIDs], PremiseIDs) :-
    ( % Collect the IDs of each member of the set values of the attribute 'GLUE'
	bagof(Premise, find_attr(FStr, GlueID, member, Premise), IDs1) ;
	% If the user has not made 'GLUE' a set-valued attribute,
	% assume that the value of 'GLUE' is a single premise, and 
	% return the value of the 'GLUE' attribute as a single-membered list
	IDs1 = [GlueID] ),
    collect_premise_ids(FStr, GlueIDs, IDs2),
    append(IDs1, IDs2, PremiseIDs).
collect_premise_ids(_FStr, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert each of the premises identified by PremiseID to a premise of the form
% expected by the prover.
extract_premises(FStr, [PremiseID|PremiseIDs],[Premise|Premises]) :-
    convert_fstr_to_premise(FStr, PremiseID, Premise),
    extract_premises( FStr, PremiseIDs, Premises).
extract_premises(_FStr, [], []).

convert_fstr_to_premise(FStr, PremiseID, Premise) :-
    % Find all of the attributes and values of the f-structure PremiseID.
    bagof([Attr,Val], find_attr(FStr, PremiseID, Attr, Val), AttrVals),
    % Find the meaning side.  This will be the value of the REL attribute.
    member(['REL',Meaning], AttrVals),
    % Find the glue side. 
    find_glue([], FStr, AttrVals, Glue),
    % Assemble the premise in the form that the prover expects.
    atomics_to_string([Meaning, " : ", Glue], Premise).

% Assemble the glue side of the meaning constructor.  First argument is either the empty set of variables
% (we are not in the scope of a quantifier binding a variable) or a variable bound by a quantifier,
% which has to be prefixed with "F".
find_glue(BoundVars0, FStr, AttrVals, Glue) :-
    check_for_quant(AttrVals, BoundVar),
    append(BoundVar, BoundVars0, BoundVars),
    % Assemble the result term from the attributes SEMSTR and TYPE.
    assemble_term(BoundVars, AttrVals, Result),
    % Check for quantification via the FORALL attribute
    % Construct the argument terms.
    msort(AttrVals, AttrValsSorted),
    find_glue_args(BoundVars, FStr, AttrValsSorted, Args),
    % Assemble the result.
    assemble_glue_expression(Args, Result, Glue0),
    % Add quantification over the s-structure as a prefix.  Quant is the empty
    % list if check_for_quant found no quantifier.  If there is a quantified variable, prefix
    % all occurrences of the variable in the string with "F".
    add_quant(BoundVar, Glue0, Glue).

% Check for a quantifier over s-structures. If the scope is a semantic form, prefix "s"
% to distinguish it from f-structures with the same numeric identifier.
check_for_quant(AttrVals, [var(V)]) :-
    ( member(['FORALL',var(V)], AttrVals);
      member(['FORALL',semform(_F,SV,_A,_NA)], AttrVals),
      atom_concat('s', SV, V)), !.
check_for_quant(_AttrVals, []).

% Add the quantifier prefix.
add_quant([], Glue, Glue). % There is no quantifier prefix.
add_quant([var(V)], Glue0, Glue) :-
   atomics_to_string(["AF", V, "_t.", Glue0], Glue).

find_glue_args(BoundVars, FStr, [[Attr,_]|ArgAttrVals], Args):-
    % Ignore the REL, SEMSTR, TYPE, and FORALL attributes.  
    member(Attr, ['REL','SEMSTR','TYPE','FORALL']), !,
    find_glue_args(BoundVars, FStr, ArgAttrVals, Args).
find_glue_args(BoundVars, FStr, [[_Arg,PremiseID]|ArgAttrVals], [Arg|Args]):-
    % These are the arguments ARG1 through ARGN, already sorted by msort above.
    % Find all of the attributes and values of the f-structure PremiseID.
    bagof([Attr,Val], find_attr(FStr, PremiseID, Attr, Val), AttrVals),
    find_glue(BoundVars, FStr, AttrVals, Arg), !,
    find_glue_args(BoundVars, FStr, ArgAttrVals, Args).
find_glue_args(_BoundVars, _FStr, [], []).

% G is the result, and Arg1...ArgN are its arguments.  Put these together into the glue
% side of a premise.
assemble_glue_expression([], G, G).
assemble_glue_expression([Arg|Args], G, Premise) :-
    assemble_glue_expression(Args, G, Premise0),
    atomics_to_string(["(", Arg, " -o ", Premise0, ")"], Premise).

% Assemble a term: the f-structure ID stands in for the semantic structure, and the
% type is the value of TYPE.  If V is an identifier for a semantic form, prefix "s".
% If V is a bound variable, prefix "F".
assemble_term(BoundVars, AttrVals, Term) :-
    % find a glue atom and its type corresponding to PremiseID, create term
    ( member(['SEMSTR',var(V)], AttrVals);
      member(['SEMSTR',semform(_F,SV,_A,_NA)], AttrVals),
      atom_concat('s', SV, V) ),
    member(['TYPE',Type], AttrVals),
    ( member(var(V), BoundVars), !,  atomics_to_string(['F', V, '_', Type], Term);
      atomics_to_string([V, '_', Type], Term) ).
      

% F-structure FID has attributes Attr with value Val. 
find_attr([cf(_,eq(attr(FID,Attr),Val))|_FStr], FID, Attr, Val).
find_attr([cf(_,in_set(Val,FID))|_FStr], FID, member, Val).
find_attr([_|FStr], FID, Attr, Val) :-
    find_attr(FStr, FID, Attr, Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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



    
    

	



