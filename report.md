# Computational Logic Assignment
## Introduction
The objective of this coursework assignment was to implement additional functionality to the Prolexa code that it cannot currently handle. In our case, we implemented negation, existential quantification, abduction and default rules. This report will discuss and explain how the functionalities were implemented, the tests that we used to check they work and the suggestions for further work to improve Prolexa.

## Negation
Negation was one of the first functionalities implemented along with default rules. Initially, Prolexa was unable to handle logical negation. This is a more fundamental issue with prolog, which uses negation as failure which is separate to logical negation. An example can be used to illustrate the difference between the two: "All lions are carnivorous. Ben is not carnivorous, therefore Ben is not a lion", is an example of logical negation. Negation as failure on the other hand would be "lions do not eat crocodiles". This does not explicitly state that lions eat animals which are not crocodiles; it implies that there is no evidence/rule in our knowledge base which states that lions eat crocodiles. Hence, in the absence of evidence to the contrary, the assumption is that lions do not eat crocodiles.

### Implementation
In order to implement logical negation, the following stored rules were added to the prolexa.pl file:

```
stored_rule(1,[(enthusiastic(X):-volunteer(X))]).
stored_rule(1,[(not enthusiastic(bob):-true)]).
stored_rule(1,[(adorable(X):-capybara(X))]).
stored_rule(1,[(not adorable(bob):-true)]).
```
The first of these stored rules is logically defined as every volunteer is enthusiastic. It does not mean that every enthusiastic person is a volunteer. This can be checked by trying to prove the predicate "prove_rb" both ways. It does not. 

In addition to the two stored rules added to prolexa.pl, the following operators and unary predicates were added to prolexa_grammar.pl.

Operator

```
:- op(900, fy, not).
not(X) :- \+ X.
```

Unary Predicates

```
pred(enthusiastic, 1, [a/enthusiastic]).
pred(volunteer, 1, [n/volunteer]).
pred(adorable, 1, [a/adorable]).
pred(capybara, 1, [n/capybara]).
```
 Two sentence structures and four verb phrases were also added to the grammar to handle negation.

 ```
 % Added the following for negations of terms
sentence1([(not L:-true)]) --> proper_noun(N,X),negative_verb_phrase(N,not X=>L).
sentence1([(not H:-B)]) --> determiner(N,M1,M2,[(not H:-B)]),noun(N,M1),negative_verb_phrase(N,not M2). 
 ```

 ```
negative_verb_phrase(s,not M) --> [is],[not],property(s,M).
negative_verb_phrase(p,not M) --> [are],[not],property(p,M). 
negative_verb_phrase(p,not M) --> [do],[not], iverb(p,M).
negative_verb_phrase(s,not M) --> [does],[not], iverb(p,M). 
 ```
The purpose of both of these were to allow for handling for a diverse range of sentence types.

Prolexa also needed method to enable negation proofs, which motivated us to make changes to prove_question and explain_question. The prove_question portion of the engine would check if the query is negated and if not, it would check if it can be proven. The predicate specifically written to handle negated queries was called handle_negation.

```
% Predicate to handle negated queries
handle_negation(not Query, Rulebase, RulebaseNegated, Negated) :-
    transform(not Query, Clauses),
    phrase(sentence(Clauses), Negated),
    RulebaseNegated = Rulebase.

handle_negation(Query, Rulebase, Rulebase, Query).
```
Corresponding to these changes in the main question answering engine, a compact addition was made to the meta-interpreter to handle negation.

```
prove_rb(not B,Rulebase,P0,P):- 
	find_clause((A:-B),Rule,Rulebase),
	prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P).
```

I will find a rule such as 'enthusiastic if volunteer' and prove that someone is not a volunteer by calling the equivalent predicate with 'not enthusiastic' in a recursive manner.

### Testing

We present a test of logical negation in prolog below.

```
prolexa> "Every volunteer is enthusiastic".
*** utterance(Every volunteer is enthusiastic)
*** rule([(enthusiastic(_59646):-volunteer(_59646))])
*** answer(I already knew that Every volunteer is enthusiastic)
I already knew that Every volunteer is enthusiastic
prolexa> "bob is not enthusiastic".
*** utterance(bob is not enthusiastic)
*** rule([(not(enthusiastic(bob)):-true)])
*** answer(I already knew that bob is not enthusiastic)
I already knew that bob is not enthusiastic
prolexa> "Is bob a volunteer".
*** utterance(Is bob a volunteer)
*** query(volunteer(bob))
*** answer(Sorry, I don't think this is the case)
Sorry, I don't think this is the case
prolexa> "Explain why bob is not a volunteer".
*** utterance(Explain why bob is not a volunteer)
*** goal(explain_question(not(volunteer(bob)),_62444,_62170))
*** answer(bob is not enthusiastic; every volunteer is enthusiastic; therefore bob is not a volunteer)
bob is not enthusiastic; every volunteer is enthusiastic; therefore bob is not a volunteer
```
Whilst this proves that logical negation has been implemented, negation in defined rules cannot be handled, as can be seen by the following example.

```
prolexa> "Every capybara is adorable".
*** utterance(Every capybara is adorable)
*** rule([(adorable(_63744):-capybara(_63744))])
*** answer(I already knew that Every capybara is adorable)
I already knew that Every capybara is adorable
prolexa> "bob is adorable".
*** utterance(bob is adorable)
*** rule([(adorable(bob):-true)])
*** answer(I will remember that bob is adorable)
I will remember that bob is adorable
prolexa> "Is bob a capybara".
*** utterance(Is bob a capybara)
*** query(capybara(bob))
*** answer(Sorry, I don't think this is the case)
Sorry, I don't think this is the case
```

## Existential Quantification
Existential quantification is a method for making statements which assert the existence of something without specifying what the thing is. In prolog, the typical way of achieving existential quantification is implicitly through variables. When a prolog program is queried, the variables may be used to ask if there exists a value which satisfies specific conditions. Prolog will then search for the values which satisfy those conditions, binding the variables. An example is given below.

```
parent(john, mary).
parent(john, peter).
parent(anne, mary).
parent(anne, peter).

?- parent(Parent, Child).

Parent = john,
Child = mary ;
Parent = john,
Child = peter ;
Parent = anne,
Child = mary ;
Parent = anne,
Child = peter.
```

### Implementation
To test the implementation of existential quantification, the following stored rules were used.

```
stored_rule(1,[(human(sk):-true), (genius(sk):-true)]).
stored_rule(1,[(win(X):-genius(X))]).
stored_rule(1,[(genius(donald):-true)]).
```
The words 'human', 'genius' and 'win' are defined in the grammar. A plural form of genius was added in noun_s2p because this does not follow the general rule of simply adding an s for plural (e.g. 'geniuses' compared with 'wins'). The following sentence structures were added to handle existential quantification.

```
sentence1((Q1,Q2)) --> [there,is],noun(s,sk=>Q1),verb_phrase(s,sk=>Q2). 
sentence1((Q1,Q2)) --> [there,are],noun(p,sk=>Q1),verb_phrase(p,sk=>Q2).
```
They cover two separate cases; individual and plural quantities. Corresponding determiners were uncommented from the provided code to handle existential quantification such as 'some animals are carnivores'.

```
determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].
determiner(p, sk=>H1, sk=>H2, [(H2:-true),(H1:-true)]) -->[some].

```

The following two questions were added to the grammar as well to allow for handling of questions such as 'do some animals eat meat' and 'are some animals carnivores'.

```
question1((Q1,Q2)) --> [do],[some],noun(p,sk=>Q1),verb_phrase(p,sk=>Q2).
question1((Q1,Q2)) --> [are],[some],noun(p,sk=>Q1),property(p,sk=>Q2).
```
The last addition to the grammar was a command which allowed for explanations of existential quantification such as 'explain why some animals eat meat'.

The following were added to the meta-interpreter to allow proofs of the different components which make up existential quantification. 

```
prove_rb((exists(V, A)), Rulebase, P0, P) :- !, % Handling existential quantification
    fresh_variable(V, FreshV),
    substitute(A, V, FreshV, NewA),
    prove_rb(NewA, Rulebase, P0, P).

```
It deals with existential quantification, by generating a new variable, substituting it into the formula, then continues the proof process with the modified formula in the rule base. The first line state that to prove the existence of a variable 'V' such that 'A' holds, for a given rulebase 'Rulebase', starting probability 'P0' and the result 'P', the predicate should follow the steps provided. A cut is also added, which commits to the clause to prevent backtracking, only if it matches. The second line generates the fresh variable which is not present in the system already. The reason this was added was to avoid variable capture which was not intended and to prevent clashes with existing variables. The third line substitutes all occurrences of the variable 'V' within the formula 'A' with the fresh variable 'FreshV' resulting in the 'NewA' formula. This ensures that existential quantification is handled properly by replacing the variable 'V' with a fresh one. The last line recursively calls 'prove_rb' with the modified formula which now does not contain the existential quantification. Thus, the proof continues with the modified formula.

### Testing
Below we present a test of existential quantification.

```
prolexa> "Some humans are geniuses".
*** utterance(Some humans are geniuses)
*** rule([(human(sk):-true),(genius(sk):-true)])
*** answer(I will remember that Some humans are geniuses)
I will remember that Some humans are geniuses
prolexa> "every genius wins". 
*** utterance(every genius wins)
*** rule([(win(_62702):-genius(_62702))])
*** answer(I already knew that every genius wins)
I already knew that every genius wins
prolexa> "Do some humans win".
*** utterance(Do some humans win)
*** query((human(sk),win(sk)))
*** answer(some humans win)
some humans win
```
The code below can be used for explaining existential quantification; however, whilst this has sound logic and gives us the outcome that we desire, the explanation of this is confusing. This is primarily due to the loop introduced, which repeats the existential quantification rule that 'some humans are geniuses'.
```
prolexa> "Explain why some humans win".
*** utterance(Explain why some humans win)
*** goal(explain_question([human(sk),win(sk)],_64606,_64396))
*** answer(some humans are geniuses; every genius wins; some humans are geniuses; therefore some humans win)
some humans are geniuses; every genius wins; some humans are geniuses; therefore some humans win
```

## Default Rules
To test default rules, the birds and flying example from Simply Logical 8.1 were implemented. The grammar did not need to be changed for these stored rules. The following code was used to implement the rules from Simply Logical:
```
stored_rule(1,[(default(flies(X):-bird(X)))]).
stored_rule(1,[(not flies(X):-penguin(X))]). % exception to the default rule
stored_rule(1,[(bird(X):-penguin(X))]).
stored_rule(1,[(penguin(tweety):-true)]).
stored_rule(1,[(bird(opus):-true)]).
```
The following was added to prolexa's grammar to identify the proper nouns used:
```
proper_noun(s,tweety) --> [tweety].
proper_noun(s,opus) --> [opus].
```
The following predicates were added to identify the nouns and verbs used:
```
pred(bird,     1,[n/bird]).
pred(penguin,     1,[n/penguin]).
pred(flies,     1,[v/flies]).
```
### Implementation
A determiner was required in the grammar. The term 'most' wass used to align with the examples provided in assignment.md. The determiner translates default rules to 'most'. 
```
determiner(p,X=>B,X=>H,[(default(H:-B))]) --> [most]. 
```
The meta-interpreter from Simply Logical 8.1 was implemented, with minor changes. explain_rb replaced explain, and variables mirroring prove_rb. First the top level explanation was implemented:
```
explain_rb(Q,RB):-
	explain_rb(Q,RB,[],_P).
```
Then the meta-interpreter for rules and defaults was implemented:
```
explain_rb(true,_Rulebase,P, P):-!.
explain_rb((A,B),Rulebase,P0,P):-!,
  explain_rb(A,Rulebase,P0,P1),
  explain_rb(B,Rulebase,P1,P).
explain_rb(A,Rulebase,P0,P):-
  prove_rb(A,Rulebase,P0,P). % explain by rules only
explain_rb(A,Rulebase,P0,P):-
  find_clause(default(A:-B),Rule,Rulebase),
  explain_rb(B,Rulebase,[p(A,Rule)|P0],P),
  not contradiction(A,Rulebase,P). % A consistent with P
```
Next the meta-interpreter for rules was implemented, this was done through copying prove_rb and changing it to prove_e to mirror Simply Logical 8.1:
```
prove_e(true,_Rulebase,P,P):-!.
prove_e((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
  prove_e(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_e(A,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_e(B,Rulebase,[p(A,Rule)|P0],P).
```
Finally, the contradiction against rules from Simply Logical 8.1 was implemented:
```
% Check contradiction against rules
contradiction(not A,Rulebase,P):-!,
	prove_e(A,Rulebase,P,_P1).
contradiction(A,Rulebase,P):-
	prove_e(not A,Rulebase,P,_P1).
```
After initial testing, the names attachment from 8.1 was identified to be already implemented through the initial prolexa_plus commit.
### Testing
As the rules were already stored for testing, default rules can be tested through asking prolexa while opus flies.
Below is the conversation with Prolexa, proving that default rules work:
```
prolexa> "opus is a bird".
*** utterance(opus is a bird)
*** rule([(bird(opus):-true)])
*** answer(I already knew that opus is a bird)
I already knew that opus is a bird
prolexa> "Does opus fly".
*** utterance(Does opus fly)
*** query(fly(opus))
*** answer(opus flies)
opus flies
prolexa> "Explain why opus flies".
*** utterance(Explain why opus flies)
*** goal(explain_question(fly(opus),_61814,_61634))
*** answer(opus is a bird; most birds fly; therefore opus flies)
opus is a bird; most birds fly; therefore opus flies
```
Default rules can be implemented by saying most noun verb:
```
prolexa> "most birds fly".
*** utterance(most birds fly)
*** rule([default((fly(_4100):-bird(_4100)))])
*** answer(I will remember that most birds fly)
I will remember that most birds fly
```
A full example of implemtation and testing is:
```
prolexa> "bob is human".
*** utterance(bob is human)
*** rule([(human(bob):-true)])
*** answer(I will remember that bob is human)
I will remember that bob is human
prolexa> "most humans fly".
*** utterance(most humans fly)
*** rule([default((fly(_1912):-human(_1912)))])
*** answer(I will remember that most humans fly)
I will remember that most humans fly
prolexa> "does bob fly".
*** utterance(does bob fly)
*** query(fly(bob))
*** answer(bob flies)
bob flies
prolexa> "explain why bob flies".
*** utterance(explain why bob flies)
*** goal(explain_question(fly(bob),_3318,_3138))
*** answer(bob is human; most humans fly; therefore bob flies)
bob is human; most humans fly; therefore bob flies
```
## Abduction
The implementation of abduction used for testing builds upon default rules; it implements a rule that can fly, to identify if it will be explained through being a bird.
```
stored_rule(1,[(flies(abe):-true)]).
```
The following was added to prolexa's grammar to identify the nouns used:
```
proper_noun(s,abe) --> [abe].
```

### Implementation
Abduction is implemented through replicating default rules, but combining explanations rather than proving each component. Abduction forms an explanation rather than proving components, the below code is used to implement abduction:
```
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
```
find_explanation was used to give clarity to the code's functionality.

Many attempts for implementing the grammar were attempted, with the final being:
```
question1(Q) --> [could], proper_noun(N,X), [be], noun(N,M).
```
A method within the grammar that allows for abduction to be called is necessary, akin to:
```
{abduce((X could be M), Rulebase, [], Q)}
```
Abduction was only implemented through the prolexa engine, as suitable grammar implementations could not be found.
 
## Further Work
Default rules can be enhanced by enabling the question 'do most x y' and the program responding with true of false, based on if a default rule exists. 
The necessary code that needs to be manipulated is:
```
question1((Q1,Q2)) --> [do],[most],noun(_,query),verb_phrase(_,query).
```
but after extended trial and error, the necessary queries could not be found.  

This assignment did not implement disjuction within prolexa plus; however, this can be implemented through building upon Simply Logical chapter 8.3 by modifying the below code for prolexa:
```
% abduce_not(O,E0,E) <- E is abductive expl. of not(O)
abduce_not((A,B),E0,E):-!,
    abduce_not(A,E0,E);       % disjunction
    abduce_not(B,E0,E).
abduce_not(A,E0,E):-
    setof(B,clause(A,B),L),
    abduce_not_l(L,E0,E).
abduce_not(A,E,E):-
    element(not(A),E).        % not(A) already assumed
abduce_not(A,E,[not(A)|E]):-  % not(A) can be added to E
    not element(not(A),E),    % if it's not already there,
    abducible(A),             % if A is abducible
    not abduce(A,E,E).        % and E doesn't explain A
abduce_not(not(A),E0,E):-     % find explanation for A
    not element(not(A),E0),   % should be consistent
    abduce(A,E0,E).

abduce_not_l([],E,E).
abduce_not_l([B|Bs],E0,E):-
    abduce_not(B,E0,E1),
    abduce_not_l(Bs,E1,E).
```
 
