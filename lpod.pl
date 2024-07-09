#!/usr/bin/env swipl


:- use_module(library(readutil)).
:- use_module(library(ugraphs)).
:- op(100, xfx, {}).
:- op(100, fx, #).
:- op(100, fx, ~).
:- op(200, fy, @).
:- op(200, fy, not).
:- op(1110, xfy, *).
:- op(1100, xfx, const).
:- op(1100, xfx, show).
:- op(1100, xfx, external).


headsym('h').
bodysym('b').
fstarsym('_isF').
fstar_or_true_sym('_isForT').

translate_lpod(Rules, CompiledRules) :-
	normalize_rules(Rules, NormalRules),
	classify_rules(NormalRules, Prefs, Regs),
	translate_prefs(Prefs, CompilePrefs),
	append(CompilePrefs, Regs, CompileRules0),
	gen_fstar_rules(NormalRules, CompileFstars),
	append(CompileRules0, CompileFstars, CompiledRules).

% Classifies rules to regulars and with preferences (ordered disjunction)
classify_rules([],[],[]).
classify_rules([R|Rs], [R|Ps], Os) :- odisj_rule(R), !, classify_rules(Rs, Ps, Os).
classify_rules([R|Rs], Ps, [R|Os]) :- classify_rules(Rs, Ps, Os).

normalize_rules([],[]).
normalize_rules([R|Rs], NormalizedRules) :-
	normalize_rule(R, NR),
	normalize_rules(Rs, NRs),
	append(NR, NRs, NormalizedRules).

normalize_rule((:- Body),    [(:- Body)]) :- !.
normalize_rule((H :- B), [(H :- B)]) :- lit(H), !.
normalize_rule(Rule, [Rule1 | Aux ]) :-
	gensym('r', R), 
	head(Rule, Head),
	body(Rule, Body),
	normalize_body(R, Body, Body1, Aux1),
	normalize_head(R, Head, Head1, Aux2),
	rule(Head1, Body1, Rule1),
	append(Aux1, Aux2, Aux).

normalize_body(_R, [], [], []) :- !.
normalize_body(_R, [L], [L],  []) :- lit(L), !.
normalize_body(R, Body, [Body1], [Rule1]) :- bodysym(S), Body1 =.. [S, R], rule(Body1, Body, Rule1).

normalize_head(_R, Head, Head, [])  :- lit(Head), !.
normalize_head(_R, Head, Head, [])  :- disj(Head), !.
normalize_head(R, Head, Head1, Aux) :- odisj(Head), !, 
	bin_to_list('*', Head, HeadList),
	normalize_head_aux(R, HeadList, NormalHeadList, Aux),
	binop('*', NormalHeadList, Head1).

normalize_head_aux(_R, [],    [], []).
normalize_head_aux(R, [H|L], [H|NL], Aux) :- lit(H), !, 
	normalize_head_aux(R, L, NL, Aux).
normalize_head_aux(R, [H|L], [NH|NL], [ (H :- NH) |Aux]) :-
	gensym('o', O),
	headsym(S),
	NH =.. [ S, R, O ],
	normalize_head_aux(R, L, NL, Aux).

translate_prefs([],     []). 
translate_prefs([P|Ps], Rs) :- 
	translate_pref_rule(P, Rs0), 
	translate_prefs(Ps,Rs1),
	append(Rs0, Rs1, Rs).

translate_pref_rule(Rule, Rules) :-
	head(Rule, Head),
	body(Rule, Body),
	bin_to_list('*', Head, Options),
	translate_options(Body, Options, Rules).

translate_options(Body, Options, Rules) :-
	translate_options(Body, Options, [], Rules).

translate_options(_,    [],  _,    []).
translate_options(Body, [O], Prev, [R]) :-
	append(Body, Prev, Body1),
	rule(O, Body1, R), !.
translate_options(Body, [O|Os], Prev, Rs) :-
	append(Body, Prev, Body1),
	rule(({O}), Body1, R),
	translate_options(Body, Os, [(not O)|Prev], RestR),
	append([R], RestR, Rs).

%
% Give a list of rules creates the corresponding a set of fstar (and fstar_or_true) rules
%
gen_fstar_rules(R, FR) :-
	build_hasF_clauses(R),
	gen_fstar_rules1(R,FR),
	destroy_hasF.

gen_fstar_rules1([], []).
gen_fstar_rules1([R|Rs], FRR) :-
	gen_fstar_rule(R, FR),
	gen_fstar_rules(Rs,FRs),
	append(FR, FRs, FRR).

%
% Rule is a ordered disjunction
% C1 * ... * Cn :- Body
%
gen_fstar_rule(Rule, FstarRules) :- odisj_rule(Rule), !,
	head(Rule, Head),
	body(Rule, Body),
	bin_to_list('*', Head, Options),
	gen_fstars_options(Body, Options, [], FstarRules).

%
% Rule is a disjunction
% C1 ; ...; Cn :- Body.
%
gen_fstar_rule((Head:-Body), FstarRules) :- disj(Head), !,
	gen_fstars_head(Body, Head, FstarRules).

%
% Rule is a regular rule
% H :- A1, ..., Am, not Am+1, ..., Ak
%
gen_fstar_rule((Head:-Body), FstarRules) :- 
	(hasF(Head) ->  gen_fstars_body(Head, Body, FstarRules) ; true).

%
% Rule is a fact
%
gen_fstar_rule(Rule, []) :- fact(Rule), !.

%
% Translates a given option inside a ordered disjunctive rule
% Body is the body of the rule. Since the rule is normalized this is either [] or [BodySymbol]
% O = [O1, ..., On] is a list of options
% Prev is a list of options that precede. If O1, O2, O3 are the options then Prev contains _isF(01), _isF(O2), ...
% Rs is a list of rules
%
% 	_isF(O1) :- _isFT(Body), Prev
%   _isF(O2) :- _isFT(Body), _isF(O1), Prev
%    ...
%   _isF(On) :- _isFT(Body), _isF(O1), _isF(O2), _isF(On-1), Prev
%
gen_fstars_options(_, [], _, []).
gen_fstars_options(Body, [O|Os], Prev, Rs) :-
	fstarsym(FStarSym),
	FStarSymO    =.. [ FStarSym, O ],
	% append([(not O) | Body], Prev, Body1),
	% rule(FStarSymO, Body1, R0), 
	gen_fstars_options2(FStarSymO, O, Prev, Body, Rs0),
	% append([R0], R1, Rs0),
	gen_fstars_options(Body, Os, [FStarSymO | Prev], Rs1),
	append(Rs0, Rs1, Rs).


% Prev is the _isF list of previous options 
% Prev = _isF(C1), _isF()
%
% _isF(0) :- not 0, _isForT(Body), Prev.
%
gen_fstars_options2(FStarSymO, O, Prev, [], [R0]) :- 
	append([(not O)], Prev, Body1),
	rule(FStarSymO, Body1, R0), !.
gen_fstars_options2(FStarSymO, O, Prev, [Body], [R1]) :-
	fstar_or_true_sym(FStarSym),
	(hasF(Body) -> FStarSymBody =.. [ FStarSym, Body ] ; FStarSymBody = Body),
	% FStarSymBody =.. [ FStarSym, Body ],
	append([(not O), FStarSymBody], Prev, Body2),
	rule(FStarSymO, Body2, R1).
	% gen_fstars_or_trues([Body],Rs1).


% if ReadBody is the BodySymbol is this rule redundant?
gen_fstars_body(BodySymbol, RealBody, [R]) :-
	bin_to_list(',', RealBody, BodyList),
	body_to_posneg(BodyList, PosList, NegList),
	fstar_or_true_sym(FStarTrueSym),
	mapop(FStarTrueSym, PosList, ForTList),
	% gen_fstars_or_trues(PosList,PosListRules),
	append(ForTList, NegList, L),
	fstarsym(FStarSym),
	FStarBodySymbol =.. [ FStarSym, BodySymbol ],
	rule(FStarBodySymbol, [(not BodySymbol)| L], R).
	% append([R], PosListRules, Rs).

gen_fstars_head(HeadSymbol, RealHead, Rs) :-
	bin_to_list(';', RealHead, HeadList),
	fstarsym(FStarSym),
	FStarHeadSymbol =.. [ FStarSym, HeadSymbol ],
	mapop(FStarSym, HeadList, IsFList),
	binop(';', IsFList, IsFHead),
	Rs = [(IsFHead :- FStarHeadSymbol)].

%
% Given a list of atoms creates the rules
%  _isForT(A) :- A
%  _isForT(A) :- _isF(A)
%
gen_fstars_or_trues([],[]).
gen_fstars_or_trues([L|Ls],Rs) :-
	fstarsym(FStarSym),
	FStarL =.. [ FStarSym, L ],
	fstar_or_true_sym(FStarTrueSym),
	FStarTrueSymL =.. [ FStarTrueSym, L ],
	Rs0 = [ (FStarTrueSymL :- FStarL),  (FStarTrueSymL :- L) ],
	gen_fstars_or_trues(Ls,Rs1),
	append(Rs0, Rs1, Rs).


%  hasF(A): true if A is in a head of an ordered disjunction
%  hasF(A): if it is in a head of a rule where some of the positive atoms B, hasF(B).
%
%  hasTF(A) if it hasF(A).
% if it does not hasTF(A) then we can substitute with A whenever we need hasTF(A).

:- dynamic hasF/1.
:- table hasF/1.

add_hasF(F) :- add_hasF(F,F).
add_hasF(F, R) :-  (call(F) -> true ;  assertz(R)).

build_hasF_clauses([]).

build_hasF_clauses([Rule|Rules]) :- 
	odisj_rule(Rule), !,
	head(Rule, Head),
	bin_to_list('*', Head, Options), !,
	((member(O, Options),
	add_hasF(hasF(O)), 
	fail) ; true),
	build_hasF_clauses(Rules).

build_hasF_clauses([Rule|Rules]) :- 
	head(Rule, Head),
	lit(Head), !,
	body(Rule, BodyList),
	body_to_posneg(BodyList, Pos, _Neg),
	(member(O, Pos), 
	 add_hasF(hasF(Head), (hasF(Head) :- hasF(O))), 
	 fail ; true),
	build_hasF_clauses(Rules).

destroy_hasF :- retractall(hasF(_)).

% auxiliary predicates 

bin_to_list(F,Term,L) :- Term =.. [F,A,B], !, bin_to_list(F,A,LA), bin_to_list(F,B,LB), append(LA,LB,L).
bin_to_list(_F,A,[A]).

binop(_F,[X],X):-!.
binop(F,[X|L],T):-binop(F,L,T0),T =.. [F,X,T0].

mapop(_F,[],[]).
mapop(F,[X|L],[T|R]) :- T =.. [F,X], mapop(F,L,R).

body_to_posneg([], [], []).
body_to_posneg([L|Ls], Pos, [L|Neg]) :- neg(L), !, body_to_posneg(Ls, Pos, Neg).
body_to_posneg([L|Ls], [L|Pos], Neg) :- body_to_posneg(Ls, Pos, Neg).

body((_H :- B), BodyList) :- !, bin_to_list(',', B, BodyList).
body(_H, []).
head((H :- _B), H) :- !.
head(H, H).
rule(H, [], H) :- !.
rule(H, L, (H:-B)) :- binop(',', L, B). 

conj((_ , _)).
disj((_ ; _)).
odisj((_ * _)).
neg((not _)).
lit(L) :- atom(L), !.
lit(L) :- compound(L), functor(L, F, _), F \= ';', F \= ',', F \= '*', F \= 'not', !.

disj_rule(Head :- _) :- disj(Head), !.
disj_rule(Head) :- disj(Head).
odisj_rule(Head :- _) :- odisj(Head), !.
odisj_rule(Head) :- odisj(Head).
rule((_ :- _)).
fact(F) :- \+ rule(F).

% read LPODS

load_file(F,T) :- see(F), read_rules([],T), seen.

read_rules(L1,L) :- read(R),(
        R = end_of_file, !, L = L1
        ; append(L1,[R],L2),
          read_rules(L2,L)
        ).

write_rules([]) :- !.
write_rules([R|Rs]) :- write(R), write('.'), nl, write_rules(Rs). 

compile_lpod(File, CompiledRules) :-
	load_file(File, Rules),
	translate_lpod(Rules, CompiledRules).

:- initialization(main, main).

main(Argv) :-
	argv_options(Argv, Positional, _Options),
	Positional = [File | _],
	compile_lpod(File, Rules),
	write_rules(Rules).