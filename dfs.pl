:- module(dfs, [find_word_gender_dfs/2, show_operations_dfs/0]).
:- use_module(stereotypes).

% Dynamic variable to count the number of calls
:- dynamic call_count_dfs/1.
call_count_dfs(0).

% Initialization to confirm the file has been loaded
:- initialization(format('Loading dfs.pl~n')).

% Predicate to increment the call count
increment_call_count_dfs :-
    retract(call_count_dfs(N)),
    N1 is N + 1,
    assert(call_count_dfs(N1)).

% Predicate to display the number of calls
show_operations_dfs :-
    call_count_dfs(N),
    format('Number of operations: ~w~n', [N]).

% Check if a word is a hyponym of "female" (direct or indirect)
is_hyponym_of_female(Word) :-
    increment_call_count_dfs,
    hypernym(female, Word).
is_hyponym_of_female(Word) :-
    increment_call_count_dfs,
    hypernym(Intermediate, Word),
    is_hyponym_of_female(Intermediate).

% Check if a word is a hyponym of "male" (direct or indirect)
is_hyponym_of_male(Word) :-
    increment_call_count_dfs,
    hypernym(male, Word).
is_hyponym_of_male(Word) :-
    increment_call_count_dfs,
    hypernym(Intermediate, Word),
    is_hyponym_of_male(Intermediate).

% Find the gender of a word, starting from the word or its synonyms
find_gender(Word, Gender) :-
    increment_call_count_dfs,
    (   is_hyponym_of_female(Word)
    ->  Gender = female
    ;   is_hyponym_of_male(Word)
    ->  Gender = male
    ;   synonym(Synonym, Word)
    ->  find_gender(Synonym, Gender)
    ;   Gender = unknown
    ).

% Example usage: Reset the call count, find the gender of "dude", and show the number of operations
% retractall(call_count_dfs(_)), assert(call_count_dfs(0)), find_gender(dude, Gender), show_operations.

% Find the gender of a word
find_word_gender_dfs(Word, Gender) :-
    retractall(call_count_dfs(_)),
    assert(call_count_dfs(0)),

    increment_call_count_dfs,
    (   is_hyponym_of_female(Word)
    ->  Gender = female
    ;   is_hyponym_of_male(Word)
    ->  Gender = male
    ;   synonym(Synonym, Word)
    ->  find_gender(Synonym, Gender)
    ;   Gender = unknown
    ).