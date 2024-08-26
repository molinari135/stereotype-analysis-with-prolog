:- module(extract, [extract_and_print/1]).
:- consult(stereotypes).

% Initialization to confirm the file has been loaded
:- initialization(format('Loading extract.pl~n')).

% Check if a word is a hyponym of "female" (either directly or indirectly)
is_hyponym_of_female(Word) :-
    find_word_gender_bfs(Word, female).

% Check if a word is a hyponym of "male" (either directly or indirectly)
is_hyponym_of_male(Word) :-
    find_word_gender_bfs(Word, male).

% Check if the term is a stereotypical trait or role for the given subject
matches_trait_or_role(Subject, Object) :-
    (   is_hyponym_of_male(Subject)
    ->  ( trait(male, Object)
        -> format('Stereotypical trait found! Males are ~w.~n', [Object])
        ;   role(male, Object)
        -> format('Stereotypical role found! Males are ~w.~n', [Object])
        ;   trait(female, Object)
        -> format('Negative stereotypical trait found! Females are ~w, not males.~n', [Object])
        ;   role(female, Object)
        -> format('Negative stereotypical role found! Females are ~w, not males.~n', [Object])
        ;   format('No trait or role found for the word ~w.~n', [Object])
        )
    ;   is_hyponym_of_female(Subject)
    ->  ( trait(female, Object)
        -> format('Stereotypical trait found! Females are ~w.~n', [Object])
        ;   role(female, Object)
        -> format('Stereotypical role found! Females are ~w.~n', [Object])
        ;   trait(male, Object)
        -> format('Negative stereotypical trait found! Males are ~w, not females.~n', [Object])
        ;   role(male, Object)
        -> format('Negative stereotypical role found! Males are ~w, not females.~n', [Object])
        ;   format('No trait or role found for the word ~w.~n', [Object])
        )
    ;   format('No correspondence found for ~w.~n', [Subject])
    ).

% Extract subject-adjective pairs, checking if the adjective matches a stereotypical trait using matches_trait_or_role
extract_subject_adjective_pairs(Tokens, SubjectAdjectivePairs) :-
    findall((Subject, Adjective),
            ( member((Subject, n), Tokens),          % Find subjects (nouns)
              member((Adjective, a), Tokens),        % Find adjectives
              nth0(SubjectPos, Tokens, (Subject, n)),
              nth0(AdjectivePos, Tokens, (Adjective, a)),
              SubjectPos < AdjectivePos,             % Ensure the adjective comes after the subject
              matches_trait_or_role(Subject, Adjective) % Verify if the adjective is a valid trait
            ),
            SubjectAdjectivePairs).

% Extract subject-object pairs, checking if the object matches a stereotypical role or trait using matches_trait_or_role
extract_subject_object_pairs(Tokens, SubjectObjectPairs) :-
    findall((Subject, Object),
            ( member((Subject, n), Tokens),
              member((Object, n), Tokens),
              Subject \= Object,
              nth0(VerbPos, Tokens, (_, v)),
              nth0(SubjectPos, Tokens, (Subject, n)),
              nth0(ObjectPos, Tokens, (Object, n)),
              SubjectPos < VerbPos,
              VerbPos < ObjectPos,
              matches_trait_or_role(Subject, Object) % Verify if the object is a valid role or trait
            ),
            SubjectObjectPairs).

% Main function to extract and print subject-object and subject-adjective pairs
extract_and_print(Tokens) :-
    extract_subject_object_pairs(Tokens, SubjectObjectPairs),
    extract_subject_adjective_pairs(Tokens, SubjectAdjectivePairs),

    % Remove duplicates from the pairs
    list_to_set(SubjectObjectPairs, UniqueSubjectObjectPairs),
    list_to_set(SubjectAdjectivePairs, UniqueSubjectAdjectivePairs),

    % Print the results
    format('~nSubject-Object Pairs: ~w~n', [UniqueSubjectObjectPairs]),
    format('Subject-Adjective Pairs: ~w~n', [UniqueSubjectAdjectivePairs]).