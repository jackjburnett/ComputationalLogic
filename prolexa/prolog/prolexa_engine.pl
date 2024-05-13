%:- module(prolexa_engine,
%	[
%		prove_question/3,		% main question-answering engine
%		explain_question/3,		% extended version that constructs a proof tree
%		known_rule/2,			% test if a rule can be deduced from stored rules
%		all_rules/1,			% collect all stored rules 
%		all_answers/2,			% everything that can be proved about a particular Proper Noun
%	]).

:- consult(library).

:-op(900,fy,not). % defined for use of not X

%%% Main question-answering engine adapted from nl_shell.pl %%%

prove_question(Query, SessionId, Answer):-
    findall(R, prolexa:stored_rule(SessionId, R), Rulebase),
    ( handle_negation(Query, Rulebase, RulebaseNegated, Negated) -> % Check if the query is negated
        ( explain_rb(Negated, RulebaseNegated) -> % If negated query can be proven
            transform(Negated, Clauses),
            phrase(sentence(Clauses), AnswerAtomList),
            atomics_to_string(AnswerAtomList, " ", Answer)
        ; % If negated query cannot be proven
            Answer = 'Sorry, I don\'t think this is the case'
        )
    ; % If query is not negated
        ( explain_rb(Query, Rulebase) -> % If query can be proven
            transform(Query, Clauses),
            phrase(sentence(Clauses), AnswerAtomList),
            atomics_to_string(AnswerAtomList, " ", Answer)
        ; % If query cannot be proven
            Answer = 'Sorry, I don\'t think this is the case'
        )
    ).
prove_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( explain_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).	

% two-argument version that can be used in maplist/3 (see all_answers/2)
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( explain_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; explain_rb(not Query,Rulebase) -> % Adapted for negation
		transform(not Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).	

%%% Extended version of prove_question/3 that constructs a proof tree %%%
explain_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( explain_rb(Query,Rulebase,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		phrase(sentence1([(Query:-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; explain_rb(not Query,Rulebase,[],Proof) -> % Adapted for negation
		maplist(pstep2message,Proof,Msg),
		phrase(sentence1([(not Query:-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages,"; ",Answer)
	; Answer = 'Sorry, I don\'t think this is the case'
	).


% Predicate to handle negated queries
handle_negation(not Query, Rulebase, RulebaseNegated, Negated) :-
    transform(not Query, Clauses),
    phrase(sentence(Clauses), Negated),
    RulebaseNegated = Rulebase.

handle_negation(Query, Rulebase, Rulebase, Query).

% convert proof step to message
pstep2message(p(_,Rule),Message):-
	rule2message(Rule,Message).
pstep2message(n(Fact),Message):-
	rule2message([(Fact:-true)],FM),
	atomic_list_concat(['It is not known that',FM]," ",Message).

%%% test if a rule can be deduced from stored rules %%%
known_rule([Rule],SessionId):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	(try((numbervars(Rule,0,_),
	     Rule=(H:-B), % this checks for the basic rules
	     add_body_to_rulebase(B,Rulebase,RB2),
	     prove_rb(H,RB2)
	   )) -> true
	; try((numbervars(Rule,0,_),
		     Rule=default(H:-B), % this checks for the default rules
		     add_body_to_rulebase(B,Rulebase,RB2),
		     prove_rb(H,RB2)
		))
	).

add_body_to_rulebase((A,B),Rs0,Rs):-!,
	add_body_to_rulebase(A,Rs0,Rs1),
	add_body_to_rulebase(B,Rs1,Rs).
add_body_to_rulebase(A,Rs0,[[(A:-true)]|Rs0]).

% meta-interpreter for rules and defaults from Section 8.1 of Peter Flach's simply logical book
explain_rb(true,_Rulebase,P, P):-!.
explain_rb((A,B),Rulebase,P0,P):-!,
  explain_rb(A,Rulebase,P0,P1),
  explain_rb(B,Rulebase,P1,P).
 explain_rb([A,B],Rulebase,P0,P):-!,
  explain_rb(A,Rulebase,P0,P1),
  explain_rb(B,Rulebase,P1,P).
explain_rb(A,Rulebase,P0,P):-
  prove_rb(A,Rulebase,P0,P). % explain by rules only
explain_rb(A,Rulebase,P0,P):-
  find_clause(default(A:-B),Rule,Rulebase),
  explain_rb(B,Rulebase,[p(A,Rule)|P0],P),
  not contradiction(A,Rulebase,P). % A consistent with P

% 3rd argument is accumulator for proofs
prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).

prove_rb(not B,Rulebase,P0,P):- % Added for negation
	find_clause((A:-B),Rule,Rulebase),
	prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P).

prove_rb((exists(V, A)), Rulebase, P0, P) :- !, % Handling existential quantification
    fresh_variable(V, FreshV),
    substitute(A, V, FreshV, NewA),
    prove_rb(NewA, Rulebase, P0, P).

% top-level versions that ignore proof
explain_rb(Q,RB):-
	explain_rb(Q,RB,[],_P).

prove_rb(Q,RB):-
	prove_rb(Q,RB,[],_P).

% Meta-interpreter for rules
prove_e(true,_Rulebase,P,P):-!.
prove_e((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
    prove_e(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_e(A,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
  prove_e(B,Rulebase,[p(A,Rule)|P0],P).

% Check contradiction against rules
contradiction(not A,Rulebase,P):-!,
	prove_e(A,Rulebase,P,_P1).
contradiction(A,Rulebase,P):-
	prove_e(not A,Rulebase,P,_P1).

% Meta-interpreter for abduction
abduce(true, _Rulebase, P, P) :- !.
abduce((A, B), Rulebase, P0, P) :- !,
    find_explanation((A:-C), Rule, Rulebase),
    conj_append(C, B, D),
    abduce(D, Rulebase, [p((A,B), Rule)|P0], P).
abduce(A, Rulebase, P0, P) :-
    find_explanation((A:-B), Rule, Rulebase),
    abduce(B, Rulebase, [p(A, Rule)|P0], P).
find_explanation((A:-B), Rule, Rulebase) :-
    find_clause((A:-B), Rule, Rulebase).


% Utility predicate for generating fresh variables
fresh_variable(V, FreshV) :-
    atomic_concat('_', V, FreshV).

% Utility predicate for substituting variables
substitute(Term, Variable, NewValue, Result) :-
    Term =.. [Func | Args],
    maplist(substitute(Variable, NewValue), Args, NewArgs),
    Result =.. [Func | NewArgs].

substitute(Variable, NewValue, Term, NewTerm) :-
    Term == Variable,
    !,
    NewTerm = NewValue.
substitute(_, _, Term, Term).


%%% Utilities from nl_shell.pl %%%

% Shouldn't need this as should be defined in library, but may throw up a warning 
% copy_element(X,Ys):-
% 	element(X1,Ys), copy_term(X1,X).

find_clause(Clause,Rule,[Rule|_Rules]):-
	copy_term(Rule,[Clause]).	% do not instantiate Rule

find_clause(Clause,Rule,[Rule|_Rules]):- % for existential quantification 
	copy_element(Clause,Rule).

find_clause(Clause,Rule,[_Rule|Rules]):-
	find_clause(Clause,Rule,Rules).

% transform instantiated, possibly conjunctive, query to list of clauses
transform((A,B),[(A:-true)|Rest]):-!,
    transform(B,Rest).
transform(A,[(A:-true)]).

%%% Two more commands: all_rules/1 and all_answers/2

% collect all stored rules 
all_rules(Answer):-
	findall(R,prolexa:stored_rule(_ID,R),Rules),
	maplist(rule2message,Rules,Messages),
	( Messages=[] -> Answer = "I know nothing"
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).

% convert rule to sentence (string)
rule2message(Rule,Message):-
	phrase(sentence1(Rule),Sentence),
	atomics_to_string(Sentence," ",Message).

% collect everything that can be proved about a particular Proper Noun
all_answers(PN,Answer):-
	findall(Q,(pred(P,1,_),Q=..[P,PN]),Queries), % collect known predicates from grammar
	maplist(prove_question,Queries,Msg),
	delete(Msg,"",Messages),
	( Messages=[] -> atomic_list_concat(['I know nothing about',PN],' ',Answer)
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).
