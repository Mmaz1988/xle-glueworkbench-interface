%:- consult('glue_prover_amended.pl').       % glue/2
:- consult('boxer/betaConversionDRT.pl').   % betaConvert/2
:- consult('boxer/printDrs.pl').            % printDrs/1


main :- 
 current_prolog_flag(argv,Argv),
 Argv = [X,Y|_],
  convert(X,Y).

convert(X,Y) :- consult(X),
  findall(S,solution(_,S),L),
  drt2file(L,Y). 

drt2file(L,F) :- betaConvertList(L,L2),
  open(F,write,Stream),
  write(Stream,L2),
  close(Stream).
 
betaConvertList([],[]).
  betaConvertList([H1|T1],[H2|T2]) :- betaConvert(H1,H2),
  printDrs(H2),
  betaConvertList(T1,T2). 

