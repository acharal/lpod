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

% read LPODS

load_file(F,T) :- see(F), read_clauses([],T), seen.

read_clauses(L1,L) :- read(C),(
        C = end_of_file, !, L = L1
        ; append(L1,[C],L2),
          read_clauses(L2,L)
        ).

compile_lpod(File, CompiledRules) :-
	load_file(File, Rules),
	translate_lpod(Rules, CompiledRules).


translate_lpod(Rules, CompiledRules) :-
	% classify_rules(Rules, Prefs, _),
	normalize_rules(Rules, NormalRules),
	classify_rules(NormalRules, Prefs, _),
	translate_prefs(Prefs, CompiledPrefs),
	% gen_fstars(NormalRules, CompiledFstars),
	CompiledFstars = [],
	append(CompiledPrefs, CompiledFstars, CompiledRules).

% Classifies rules to regulars and with preferences (ordered disjunction)
classify_rules([],[],[]).
classify_rules([R|Rs], [R|Ps], Os) :- is_pref(R), !, classify_rules(Rs, Ps, Os).
classify_rules([R|Rs], Ps, [R|Os]) :- classify_rules(Rs, Ps, Os).

normalize_rules([],[]).
normalize_rules([R|Rs], NormalizedRules) :-
	normalize_rule(R, NR),
	normalize_rules(Rs, NRs),
	append(NR, NRs, NormalizedRules).

normalize_rule((:- Body),    [(:- Body)]).
normalize_rule((Head:-Body), [(Head :- Body)]) :- atom(Head), !.
normalize_rule((Head:-Body), [(Head :- Body1) | Aux ]) :- 
	Head = (_ ; _), 
	gensym('r', R), 
	normalize_body(R, Body, Body1, Aux).
normalize_rule((Head:-Body), [(Head1 :- Body1) | Aux ]) :- 
	Head = (_ x _),  
	gensym('r', R), 
	normalize_body(R, Body, Body1, Aux1),
	normalize_head(R, Head, Head1, Aux2),
	append(Aux1, Aux2, Aux).

normalize_body(_, Body, Body,  []) :- atom(Body), !.
normalize_body(R, Body, Body1, [Body1 :- Body]) :- Body1 =.. ['b', R].

normalize_head(_, Head, Head, [])  :- atom(Head), !.
normalize_head(_, Head, Head, [])  :- Head = (_ ; _), !.
normalize_head(R, Head, Head1, Aux) :- Head = (_ x _), 
	bin_to_list('x', Head, HeadList),
	normalize_head_aux(R, HeadList, NormalHeadList, Aux),
	binop('x', NormalHeadList, Head1).

normalize_head_aux(_, [],    [], []).
normalize_head_aux(R, [H|L], [H|NL], Aux) :- 
	atom(H), !, 
	normalize_head_aux(R, L, NL, Aux).
normalize_head_aux(R, [H|L], [NH|NL], [ (H :- NH) |Aux]) :-
	gensym('o', O),
	NH =.. [ 'h', R, O ],
	normalize_head_aux(R, L, NL, Aux).

is_disj((_ ; _) :- _).
is_pref((_ x _) :- _).
is_not((not _)).

translate_prefs([],     []). 
translate_prefs([P|Ps], Rs) :- 
	translate_pref_rule(P, Rs0), 
	translate_prefs(Ps,Rs1),
	append(Rs0, Rs1, Rs).

translate_pref_rule((Head:-Body), Rules) :-
	bin_to_list('x', Head, Options),
	translate_body(Body, Body1, BodyRules),
	translate_options(Body1, Options, OptionRules),
	append(OptionRules, BodyRules, Rules).

% translate_body(Body, Body, []) :- atom(Body), !.
% translate_body(Body, B, [ (B :- Body) ]) :- gensym(r, RSymbol), B =.. [body, RSymbol].

% translate_head(O, O, [])  :- atom(O), !. % or a single predicate.
% translate_head(O, S, [ (O :- S) ]) :- gensym(head, S).

translate_options(Body, Options, Rules) :-
	translate_options(Body, Options, [], Rules).

translate_options(_,    [],  _,    []).
translate_options(Body, [O], Prev, RR) :-
	translate_head(O, O1, OR), 
	append_to_rule((O1 :- Body), Prev, R), 
	append([R], OR, RR),
	!.
translate_options(Body, [O|Os], Prev, Rs) :-
	translate_head(O, O1, OR), 
	append_to_rule(({ O1 } :- Body), Prev, R),
	translate_options(Body, Os, [(not O)|Prev], RestR),
	append([R], OR, RR),
	append(RR, RestR, Rs).

gen_fstars((Head:-Body), FstarRules) :-
	bin_to_list('x', Head, Options),
	gen_fstars_options(Body, Options, [], FstarRules0),
	gen_fstars_body(Body, RealBody, FstarRules1),
	gen_fstars_head(Head, RealHead, FstarRules2),
	append(FstarRules0, FstarRules1, FstarRules3),
	append(FstarRules3, FstarRules2, FstarRules).

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
	mapop(isForT, PosList, isForTList),
	append(isForTList, NegList, L),
	gen_fstars_or_trues(PosList,PosListRules),
	append_to_rule((isF(BodySymbol) :- not BodySymbol), L, R),
	append([R], PosListRules, Rs).

gen_fstars_or_trues([],[]).
gen_fstars_or_trues([L|Ls],Rs) :-
	Rs0 = [ isForT(L) :- (isF(L) ; L) ],
	gen_fstars_or_trues(Ls,Rs1),
	append(Rs0, Rs1, Rs).

gen_fstars_head(RealHead, RealHead, []) :- atom(RealHead), !.
gen_fstars_head(HeadSymbol, RealHead, Rs) :-
	bin_to_list(';', ReadHead, HeadList),
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

neg_list([],[]).
neg_list([X|Xs], [(not X) | NotXs]) :- neg_list(Xs, NotXs).

body_to_posneg([L|Ls], Pos, [L|Neg]) :- is_not(L), !, body_to_posneg(Ls, Pos, Neg).
body_to_posneg([L|Ls], [L|Pos], Neg) :- body_to_posneg(Ls, Pos, Neg).