#!/usr/bin/env swipl


:- use_module(library(readutil)).
:- op(100, xfx, {}).
:- op(100, fx, #).
:- op(100, fx, ~).
:- op(200, fy, @).
:- op(210, fy, not).
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

gen_fstar_rules([], []).
gen_fstar_rules([R|Rs], FRR) :-
	gen_fstar_rule(R, FR),
	gen_fstar_rules(Rs,FRs),
	append(FR, FRs, FRR).

gen_fstar_rule(Rule, FstarRules) :- odisj_rule(Rule), !,
	head(Rule, Head),
	body(Rule, Body),
	bin_to_list('*', Head, Options),
	gen_fstars_options(Body, Options, [], FstarRules).

gen_fstar_rule((Head:-Body), FstarRules) :- disj(Head), !,
	gen_fstars_head(Body, Head, FstarRules).

gen_fstar_rule((Head:-Body), FstarRules) :- !,
	gen_fstars_body(Head, Body, FstarRules).

gen_fstar_rule(Rule, []) :- fact(Rule), !.

gen_fstars_options(_, [], _, []).
%gen_fstars_options([], [O|Os], Prev, Rs) :- !,
%	fstarsym(FStarSym),
%	FStarSymO    =.. [ FStarSym, O ],
%	append([(not O)], Prev, Body1),
%	rule(FStarSymO, Body1, R0),
%	gen_fstars_options([], Os, [FStarSymO | Prev], Rs1),
%	append([R0], Rs1, Rs).

gen_fstars_options(Body, [O|Os], Prev, Rs) :-
	fstarsym(FStarSym),
	FStarSymO    =.. [ FStarSym, O ],
	append([(not O) | Body], Prev, Body1),
	rule(FStarSymO, Body1, R0), 
	gen_fstars_options2(FStarSymO, O, Prev, Body, R1),
	append([R0], R1, Rs0),
	gen_fstars_options(Body, Os, [FStarSymO | Prev], Rs1),
	append(Rs0, Rs1, Rs).


% the following inline code is not working for some reason
%
% (Body = [], Rs0 = [R0]
% ; FStarSymBody =.. [ FStarSym, Body ],
%   append([(not O) | FStarSymBody], Prev, Body2),
%   rule(FStarSymO, Body2, R1),
%   Rs0 = [R0,R1]),
%
% so it is outsourced in the following predicate.
gen_fstars_options2(_FStarSymO, _O, _Prev, [], []) :- !.
gen_fstars_options2(FStarSymO, O, Prev, [Body], [R1]) :-
	fstarsym(FStarSym),
	FStarSymBody =.. [ FStarSym, Body ],
	append([(not O), FStarSymBody], Prev, Body2),
	rule(FStarSymO, Body2, R1).

% if ReadBody is the BodySymbol is this rule redundant?
gen_fstars_body(BodySymbol, RealBody, Rs) :-
	bin_to_list(',', RealBody, BodyList),
	body_to_posneg(BodyList, PosList, NegList),
	fstar_or_true_sym(FStarTrueSym),
	mapop(FStarTrueSym, PosList, ForTList),
	gen_fstars_or_trues(PosList,PosListRules),
	append(ForTList, NegList, L),
	fstarsym(FStarSym),
	FStarBodySymbol =.. [ FStarSym, BodySymbol ],
	rule(FStarBodySymbol, [(not BodySymbol)| L], R),
	append([R], PosListRules, Rs).

gen_fstars_or_trues([],[]).
gen_fstars_or_trues([L|Ls],Rs) :-
	fstarsym(FStarSym),
	FStarL =.. [ FStarSym, L ],
	fstar_or_true_sym(FStarTrueSym),
	FStarTrueSymL =.. [ FStarTrueSym, L ],
	Rs0 = [ FStarTrueSymL :- (FStarL ; L) ],
	gen_fstars_or_trues(Ls,Rs1),
	append(Rs0, Rs1, Rs).

gen_fstars_head(HeadSymbol, RealHead, Rs) :-
	bin_to_list(';', RealHead, HeadList),
	fstarsym(FStarSym),
	FStarHeadSymbol =.. [ FStarSym, HeadSymbol ],
	mapop(FStarSym, HeadList, IsFList),
	binop(';', IsFList, IsFHead),
	Rs = [(IsFHead :- FStarHeadSymbol)].

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