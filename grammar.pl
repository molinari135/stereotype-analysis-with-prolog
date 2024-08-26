:- module(grammar, [
    is_hyponym_of_female/1,
    is_hyponym_of_male/1,
    find_gender/2,
    sentence/3,
    sentence_to_pos_tags/2,
    generate_and_print_sentences_with_tags/0
]).

:- consult(stereotypes).

% Initialization to confirm the file has been loaded
:- initialization(format('Loading grammar.pl~n')).

% Check if a term is a hyponym of female (direct or indirect)
is_hyponym_of_female(Word) :-
    (   hypernym(female, Word)
    ;   synonym(Synonym, Word),
        hypernym(female, Synonym)
    ;   hypernym(Intermediate, Word),
        is_hyponym_of_female(Intermediate)
    ;   synonym(Synonym, Word),
        is_hyponym_of_female(Synonym)
    ).

% Check if a term is a hyponym of male (direct or indirect)
is_hyponym_of_male(Word) :-
    (   hypernym(male, Word)
    ;   synonym(Synonym, Word),
        hypernym(male, Synonym)
    ;   hypernym(Intermediate, Word),
        is_hyponym_of_male(Intermediate)
    ;   synonym(Synonym, Word),
        is_hyponym_of_male(Synonym)
    ).

% Find the gender of a term starting from the word or its synonyms
find_gender(Word, Gender) :-
    (   is_hyponym_of_female(Word)
    ->  Gender = female
    ;   is_hyponym_of_male(Word)
    ->  Gender = male
    ;   synonym(Synonym, Word)
    ->  find_gender(Synonym, Gender)
    ;   Gender = unknown
    ).

% DCG for "subject be adjective"
sentence(subject_be_adjective(Sentence)) -->
    subject(Subject),
    [is],
    adjective_for_subject(Subject, Adjective),
    { Sentence = [Subject, is, Adjective] }.

% DCG for "subject be too adjective be role"
sentence(subject_be_too_adjective_be_role(Sentence)) -->
    subject(Subject),
    [is],
    [too],
    adjective_for_subject(Subject, Adjective),
    [to_be],
    role_for_subject(Subject, Role),
    { Sentence = [Subject, is, too, Adjective, to_be, Role] }.

% DCG for "subject is not suited for role"
sentence(subject_is_not_suited_for_role(Sentence)) -->
    subject(Subject),
    [is],
    [not],
    [suited],
    [for],
    role_opposite_for_subject(Subject, Role),
    { Sentence = [Subject, is, not, suited, for, Role] }.

% DCG for subject (hyponyms of male or female)
subject(Subject) -->
    [Subject],
    { (is_hyponym_of_male(Subject); is_hyponym_of_female(Subject)) }.

% DCG for adjective (traits) - only traits valid for the subject's gender
adjective_for_subject(Subject, Adjective) -->
    [Adjective],
    { find_gender(Subject, Gender), trait(Gender, Adjective) }.

% DCG for role - only roles valid for the opposite gender
role_for_subject(Subject, Role) -->
    [Role],
    { (is_hyponym_of_female(Subject) -> role(male, Role) ; role(female, Role)) }.

% DCG for opposite role - only roles valid for the opposite gender
role_opposite_for_subject(Subject, Role) -->
    [Role],
    { (is_hyponym_of_female(Subject) -> role(male, Role) ; role(female, Role)) }.

% Convert a sentence into POS tag format
sentence_to_pos_tags(Sentence, POSList) :-
    maplist(pos_tag, Sentence, POSList).

% POS tag predicates
pos_tag(Subject, (Subject, n)) :-
    is_hyponym_of_male(Subject) ; is_hyponym_of_female(Subject).
pos_tag(be, (be, v)).
pos_tag(to_be, (be, v)).
pos_tag(too, (too, r)).
pos_tag(Adjective, (Adjective, a)) :-
    find_gender(_, Gender), trait(Gender, Adjective).
pos_tag(Role, (Role, n)) :-
    role(_, Role).
pos_tag(is, (be, v)).
pos_tag(not, (not, r)).
pos_tag(suited, (suited, v)).
pos_tag(for, (for, r)).

% Generate and print sentences with POS tags
generate_and_print_sentences_with_tags :-
    % Generate sentences of type "subject be adjective"
    findall(Sentence1, phrase(sentence(subject_be_adjective(Sentence1)), _), Sentences1),
    (   Sentences1 \= []
    ->  random_member(Sentence1, Sentences1),
        sentence_to_pos_tags(Sentence1, PosTags1),
        format('~nGenerated Sentence 1: ~w~nPOS Tags: ~w~n', [Sentence1, PosTags1])
    ;   format('~nNo valid sentences found for Sentence 1~n')
    ),

    % Generate sentences of type "subject be too adjective be role"
    findall(Sentence2, phrase(sentence(subject_be_too_adjective_be_role(Sentence2)), _), Sentences2),
    (   Sentences2 \= []
    ->  random_member(Sentence2, Sentences2),
        sentence_to_pos_tags(Sentence2, PosTags2),
        format('~nGenerated Sentence 2: ~w~nPOS Tags: ~w~n', [Sentence2, PosTags2])
    ;   format('~nNo valid sentences found for Sentence 2~n')
    ),

    % Generate sentences of type "subject is not suited for role"
    findall(Sentence3, phrase(sentence(subject_is_not_suited_for_role(Sentence3)), _), Sentences3),
    (   Sentences3 \= []
    ->  random_member(Sentence3, Sentences3),
        sentence_to_pos_tags(Sentence3, PosTags3),
        format('~nGenerated Sentence 3: ~w~nPOS Tags: ~w~n', [Sentence3, PosTags3])
    ;   format('~nNo valid sentences found for Sentence 3~n')
    ).
