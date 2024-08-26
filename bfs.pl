:- module(bfs, [find_word_gender_bfs/2, show_operations_bfs/0]).
:- use_module(stereotypes).

% Initialization to confirm the file has been loaded
:- initialization(format('Loading bfs.pl~n')).

% Dynamic variable to count the number of calls
:- dynamic call_count_bfs/1.
call_count_bfs(0).

% Predicate to increment the call count
increment_count :-
    retract(call_count_bfs(N)),
    N1 is N + 1,
    assert(call_count_bfs(N1)).

% Tracked predicates for hypernyms and synonyms
tracked_hypernym(A, B) :-
    increment_count,
    hypernym(A, B).

tracked_synonym(A, B) :-
    increment_count,
    synonym(A, B).

% Use the tracked predicates in your BFS search
bfs_hyponym_of_gender_tracked(Word, Gender, Path) :-
    bfs_tracked([[Word]], Gender, Path).

% Tracked BFS search
bfs_tracked([[Gender | Path] | _], Gender, [Gender | Path]) :- 
    !.

bfs_tracked([[Current | Path] | Rest], Gender, Result) :-
    findall([Next, Current | Path],
            (tracked_hypernym(Next, Current); tracked_synonym(Next, Current)),
            Neighbors),
    append(Rest, Neighbors, NewPaths),
    bfs_tracked(NewPaths, Gender, Result).

bfs_tracked([], _, []) :- 
    !.

% Display the number of operations
show_operations_bfs :-
    call_count_bfs(N),
    format('Number of operations: ~w~n', [N]).

% Find the gender of the word
find_word_gender_bfs(Word, Gender) :-
    retractall(call_count_bfs(_)),
    assert(call_count_bfs(0)),

    % Perform the search for "female"
    bfs_hyponym_of_gender_tracked(Word, female, FemalePath),
    (   FemalePath \= []
    -> Gender = female
    ;   % Perform the search for "male"
        bfs_hyponym_of_gender_tracked(Word, male, MalePath),
           MalePath \= []
        ->    Gender = male
        ;   % No gender found
            Gender = unknown
    ).