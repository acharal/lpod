#!/usr/bin/env swipl


:- use_module(library(readutil)).
:- op(100, xfx, {}).
:- op(100, fx, #).
:- op(100, fx, ~).
:- op(200, fy, @).
:- op(200, fy, not).
:- op(220,xfy,x).
:- op(1100, xfx, const).
:- op(1100, xfx, show).
:- op(1100, xfx, external).


headsym('h').
bodysym('b').
fstarsym('isF').
fstar_or_true_sym('isForT').

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

normalize_rule((:- Body),    [(:- Body)]).
normalize_rule((Head:-Body), [(Head :- Body)]) :- lit(Head), !.
normalize_rule((Head:-Body), [(Head :- Body1) | Aux ]) :- disj(Head), !, 
	gensym('r', R), 
	normalize_body(R, Body, Body1, Aux).
normalize_rule((Head:-Body), [(Head1 :- Body1) | Aux ]) :- odisj(Head), !,
	gensym('r', R), 
	normalize_body(R, Body, Body1, Aux1),
	normalize_head(R, Head, Head1, Aux2),
	append(Aux1, Aux2, Aux).

normalize_body(_, Body, Body,  []) :- lit(Body), !.
normalize_body(R, Body, Body1, [Body1 :- Body]) :- conj(Body), !, bodysym(S), Body1 =.. [S, R].

normalize_head(_, Head, Head, [])  :- lit(Head), !.
normalize_head(_, Head, Head, [])  :- disj(Head), !.
normalize_head(R, Head, Head1, Aux) :- odisj(Head), !, 
	bin_to_list('x', Head, HeadList),
	normalize_head_aux(R, HeadList, NormalHeadList, Aux),
	binop('x', NormalHeadList, Head1).

normalize_head_aux(_, [],    [], []).
normalize_head_aux(R, [H|L], [H|NL], Aux) :- lit(H), !, 
	normalize_head_aux(R, L, NL, Aux).
normalize_head_aux(R, [H|L], [NH|NL], [ (H :- NH) |Aux]) :-
	gensym('o', O),
	headsym(S),
	NH =.. [ S, R, O ],
	normalize_head_aux(R, L, NL, Aux).

conj(_ , _).
disj(_ ; _).
odisj(_ x _).
neg(not _).
lit(L) :- atom(L).
lit(L) :- compound(L), functor(L, F, A), L \= ';', L \= ','.

disj_rule(Head :- _) :- disj(Head).
odisj_rule(Head :- _) :- odisj(Head).

translate_prefs([],     []). 
translate_prefs([P|Ps], Rs) :- 
	translate_pref_rule(P, Rs0), 
	translate_prefs(Ps,Rs1),
	append(Rs0, Rs1, Rs).

translate_pref_rule((Head:-Body), Rules) :-
	bin_to_list('x', Head, Options),
	translate_options(Body, Options, Rules).

translate_options(Body, Options, Rules) :-
	translate_options(Body, Options, [], Rules).

translate_options(_,    [],  _,    []).
translate_options(Body, [O], Prev, [R]) :-
	append_to_rule((O :- Body), Prev, R), !.
translate_options(Body, [O|Os], Prev, Rs) :-
	append_to_rule(({ O } :- Body), Prev, R),
	translate_options(Body, Os, [(not O)|Prev], RestR),
	append([R], RestR, Rs).

gen_fstar_rules([], []).
gen_fstar_rules([R|Rs], FRR) :-
	gen_fstar_rule(R, FR),
	gen_fstar_rules(Rs,FRs),
	append(FR, FRs, FRR).

gen_fstar_rule((Head:-Body), FstarRules) :-
	Head = (_ x _), !,
	bin_to_list('x', Head, Options),
	gen_fstars_options(Body, Options, [], FstarRules).

gen_fstar_rule((Head:-Body), FstarRules) :-
	Head = (_ ; _), !,
	gen_fstars_head(Body, Head, FstarRules).

gen_fstar_rule((Head:-Body), FstarRules) :- !,
	gen_fstars_body(Head, Body, FstarRules).

gen_fstars_options(_, [], _, []).
gen_fstars_options(Body, [O|Os], Prev, Rs) :-
	% neg_list(Prev, NPrev), 
	append_to_rule((isF(O)    :- (not O), Body),      Prev, R0),
	append_to_rule((isF(O)    :- (not O), isF(Body)), Prev, R1),
	append([R0],[R1],Rs0),
	gen_fstars_options(Body, Os, [isF(O)|Prev], Rs1),
	append(Rs0, Rs1, Rs).

% if ReadBody is the BodySymbol is this rule redundant?
gen_fstars_body(BodySymbol, RealBody, Rs) :-
	bin_to_list(',', RealBody, BodyList),
	body_to_posneg(BodyList, PosList, NegList),
	mapop(isForT, PosList, ForTList),
	append(ForTList, NegList, L),
	gen_fstars_or_trues(PosList,PosListRules),
	append_to_rule((isF(BodySymbol) :- not BodySymbol), L, R),
	append([R], PosListRules, Rs).

gen_fstars_or_trues([],[]).
gen_fstars_or_trues([L|Ls],Rs) :-
	Rs0 = [ isForT(L) :- (isF(L) ; L) ],
	gen_fstars_or_trues(Ls,Rs1),
	append(Rs0, Rs1, Rs).

gen_fstars_head(HeadSymbol, RealHead, Rs) :-
	bin_to_list(';', RealHead, HeadList),
	mapop(isF, HeadList, IsFList),
	binop(';', IsFList, IsFHead),
	Rs = [(IsFHead :- isF(HeadSymbol))].

% auxiliary predicates 

bin_to_list(F,Term,L) :- Term =.. [F,A,B], !, bin_to_list(F,A,LA), bin_to_list(F,B,LB), append(LA,LB,L).
bin_to_list(_F,A,[A]).

binop(_F,[X],X):-!.
binop(F,[X|L],T):-binop(F,L,T0),T =.. [F,X,T0].

mapop(_F,[],[]).
mapop(F,[X|L],[T|R]) :- T =.. [F,X], mapop(F,L,R).

append_to_rule(R, [], R) :- !.
append_to_rule((H :- B), L, (H :- B1)):-
	bin_to_list(',', B, Bs0),
	append(Bs0, L, Bs),
	binop(',', Bs, B1).


% unused
neg_list(X,Y) :- mapop(not, X, Y). 

body_to_posneg([], [], []).
body_to_posneg([L|Ls], Pos, [L|Neg]) :- neg(L), !, body_to_posneg(Ls, Pos, Neg).
body_to_posneg([L|Ls], [L|Pos], Neg) :- body_to_posneg(Ls, Pos, Neg).


% read LPODS

load_file(F,T) :- see(F), read_rules([],T), seen.

read_rules(L1,L) :- read(C),(
        C = end_of_file, !, L = L1
        ; append(L1,[C],L2),
          read_rules(L2,L)
        ).

write_rules([]) :- !.
write_rules([R|Rs]) :- write(R), write('.'), nl, write_rules(Rs). 

compile_lpod(File, CompiledRules) :-
	load_file(File, Rules),
	translate_lpod(Rules, CompiledRules).

main(Argv).