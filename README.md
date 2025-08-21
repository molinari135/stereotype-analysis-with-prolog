# Gender stereotype recognition with Prolog

[![CC BY-NC 4.0][cc-by-nc-shield]][cc-by-nc]

[cc-by-nc]: https://creativecommons.org/licenses/by-nc/4.0/
[cc-by-nc-image]: https://licensebuttons.net/l/by-nc/4.0/88x31.png
[cc-by-nc-shield]: https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg

## Abstract

Stereotypes are generalized belief about a particular category of people and are used as expectation that people might have about every person of a particular group. They are often overgeneralized, inaccurate, and resistant to new information and they can be positive, neutral, or negative. The main idea of this project is to analyze and extract classic explicit stereotypes from everyday language through logic programming. The full documentation can be found on this [document](https://github.com/burraco135/stereotype-analysis-with-prolog/blob/master/Stereotype_analysis_with_Prolog.pdf).

## How to use

Download all `.pl` files in your Prolog environment and consult `[menu].` to use the CLI interface and test the system. If the module has been loaded correctly, the prompt will be the following:

```
1 ?- [menu].
Loading stereotypes.pl
Loading extract.pl
Loading stereotypes.pl
Loading grammar.pl
Loading bfs.pl
Loading dfs.pl
Loading stereotypes.pl
Loading stereotypes.pl

Menu
1. Find word gender with DFS
2. Find word gender with BFS
3. Find traits and roles for female and male
4. Generate a sentence and show POS tags
100. Add a trait (dev)
200. Add a role (dev)
0. Exit

Enter your choice:
```

## Modules

1. `stereotypes.pl`
    - Defines roles, traits, hypernyms and synonyms related to genders
        - `trait(+Gender, +Trait)`: defines a trait related to a gender and it's dynamic
        - `role(+Gender, +Role)`: defines a role related to a gender and it's dynamic
        - `hypernym(+Hypernym, +Hyponym)`: defines a hypernym and its hyponym, and vice versa
        - `synonym(+Word1, +Word2)`: defines a synonym for a word
3. `extract.pl`
    - Provides predicates to extract information from a sentence, as traits and roles
        - `extract_subject_object_pair(+Tokens, -SubjectObjectPairs)`: extract (Subject, Object) pairs using POS tagging
        - `extract_subject_adjective_pair(+Tokens, -SubjectAdjectivePairs)`: extract (Subject, Adjective) pairs using POS tagging
        - `matches_trait_or_role(+Subject, +Object)`: check the knowledge base for traits and roles related to the extracted pairs
        - `extract_and_print(+Tokens)`: prints stereotypes found and word pairs
4. `grammar.pl`
    - Defines DCG grammars for sentence generation based on stereotypes
        - `sentence/1`: includes DCG predicates to generate structured sentences
        - `subject/1`: choose the subject of the sentence (female or male-related)
        - `adjective_for_subject/1`: choose the appropriate adjective (trait) for a certain subject
        - `role_for_subject/1`: choose the appropriate noun (role) for a certain subject
5. `bfs.pl`
    - Best-First Search implementation in Prolog for hyponym search
        - `find_word_gender_bfs(+Word, -Gender)`: find the gender related to a word
        - `show_operations_bfs/0`: shows the number of operations needed to find a solution
6. `dfs.pl`
    - Depth-First Search implementation in Prolog for hyponym search
        - `find_word_gender_dfs(+Word, -Gender)`: find the gender related to a word

## License

This work is licensed under the [Creative Commons Attribution-NonCommercial 4.0 International License][cc-by-nc].

For a copy of the license, please visit https://creativecommons.org/licenses/by-nc/4.0/

[![CC BY-NC 4.0][cc-by-nc-image]][cc-by-nc]
        - `show_operations_dfs/0`: shows the number of operations needed to find a solution
7. `menu.pl`
    - A simple CLI menu to test the system
        - `menu/0`: shows the main menu with all the available options
