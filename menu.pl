:- consult([extract, grammar, bfs, dfs, stereotypes, stereotypes]).
:- use_module([extract, grammar, bfs, dfs, stereotypes]).

:- initialization(menu).

% Show the menu and handle options
menu :-
    format('~nMenu'), nl,
    format('1. Find word gender with DFS'), nl,
    format('2. Find word gender with BFS'), nl,
    format('3. Find traits and roles for female and male'), nl,
    format('4. Generate a sentence and show POS tags'), nl,
    format('100. Add a trait (dev)'), nl,
    format('200. Add a role (dev)'), nl,
    format('0. Exit'), nl,
    prompt_user.

% Prompt user for their choice and handle it
prompt_user :-
    format('~nEnter your choice: '),
    read(Choice),
    handle_choice(Choice).

% Handle user choices
handle_choice(1) :-
    format('~nEnter the word to find its gender: '),
    read(Word),
    find_word_gender_dfs(Word, Gender),
    format('The gender of ~w is ~w using DFS.~n', [Word, Gender]),
    show_operations_dfs,
    menu.  % Re-display the menu after showing the result

handle_choice(2) :-
    format('~nEnter the word to find its gender: '),
    read(Word),
    find_word_gender_bfs(Word, Gender),
    format('The gender of ~w is ~w using BFS.~n', [Word, Gender]),
    show_operations_bfs,
    menu.  % Re-display the menu after showing the result

handle_choice(3) :-
    format('~nFinding traits and roles for female and male...~n'),
    find_traits_and_roles,
    menu.  % Re-display the menu after showing the result

handle_choice(4) :-
    format('~nGenerate a sentence based on the following frames:'), nl,
    format('1. Subject be adjective'), nl,
    format('2. Subject be too adjective be role'), nl,
    format('3. Subject is not suited for role'), nl,
    format('Select a frame (1-3): '),
    read(FrameOption),
    generate_sentence(FrameOption),
    menu.

handle_choice(100) :-
    prompt_gender_trait(Gender, Trait),
    assert_trait(Gender, Trait),
    format('Added trait: (~w, ~w)~n', [Gender, Trait]),
    menu.

handle_choice(200) :-
    prompt_gender_role(Gender, Role),
    assert_role(Gender, Role),
    format('Added role: (~w, ~w)~n', [Gender, Role]),
    menu.

handle_choice(0) :-
    format('Exiting...~n').

handle_choice(_) :-
    format('Invalid choice. Please try again.~n'),
    menu.

% Find and display traits and roles for both genders
find_traits_and_roles :-
    format('~nTraits and roles for female:~n'),
    findall(Trait, trait(female, Trait), FemaleTraits),
    findall(Role, role(female, Role), FemaleRoles),
    ( FemaleTraits \= [] -> format('~nTrait for female: ~w~n', [FemaleTraits]) ; format('No traits found for female.~n') ),
    ( FemaleRoles \= [] -> format('Role for female: ~w~n', [FemaleRoles]) ; format('No roles found for female.~n') ),
    
    format('~nTraits and roles for male:~n'),
    findall(Trait, trait(male, Trait), MaleTraits),
    findall(Role, role(male, Role), MaleRoles),
    ( MaleTraits \= [] -> format('~nTrait for male: ~w~n', [MaleTraits]) ; format('No traits found for male.~n') ),
    ( MaleRoles \= [] -> format('Role for male: ~w~n', [MaleRoles]) ; format('No roles found for male.~n') ).

% Add a trait
assert_trait(Gender, Trait) :-
    assertz(trait(Gender, Trait)).

% Add a role
assert_role(Gender, Role) :-
    assertz(role(Gender, Role)).

% Prompt and validate gender and trait
prompt_gender_trait(Gender, Trait) :-
    repeat,
    format('Enter the gender (female or male): '),
    read(Gender),
    ( Gender = female ; Gender = male ), !,
    format('Enter the trait: '),
    read(Trait).

% Prompt and validate gender and role
prompt_gender_role(Gender, Role) :-
    repeat,
    format('Enter the gender (female or male): '),
    read(Gender),
    ( Gender = female ; Gender = male ), !,
    format('Enter the role: '),
    read(Role).

% Function to generate a sentence based on the selected frame
generate_sentence(1) :-
    findall(Sentence, phrase(sentence(subject_be_adjective(Sentence)), _), Sentences),
    (   Sentences \= []
    ->  random_member(Sentence, Sentences),
        sentence_to_pos_tags(Sentence, PosTags),
        format('~nGenerated Sentence: ~w~nPOS Tags: ~w~n', [Sentence, PosTags]),
        extract_and_print(PosTags) % Analyze the sentence with find_stereotype
    ;   format('~nPlease retry, something went wrong~n')
    ).

generate_sentence(2) :-
    findall(Sentence, phrase(sentence(subject_be_too_adjective_be_role(Sentence)), _), Sentences),
    (   Sentences \= []
    ->  random_member(Sentence, Sentences),
        sentence_to_pos_tags(Sentence, PosTags),
        format('~nGenerated Sentence: ~w~n~nPOS Tags: ~w~n', [Sentence, PosTags]),
        extract_and_print(PosTags) % Analyze the sentence with find_stereotype
    ;   format('~nPlease retry, something went wrong~n')
    ).

generate_sentence(3) :-
    findall(Sentence, phrase(sentence(subject_is_not_suited_for_role(Sentence)), _), Sentences),
    (   Sentences \= []
    ->  random_member(Sentence, Sentences),
        sentence_to_pos_tags(Sentence, PosTags),
        format('~nGenerated Sentence: ~w~n~nPOS Tags: ~w~n', [Sentence, PosTags]),
        extract_and_print(PosTags) % Analyze the sentence with find_stereotype
    ;   format('~nPlease retry, something went wrong~n')
    ).
