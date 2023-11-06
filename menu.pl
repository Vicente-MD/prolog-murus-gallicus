print_menu :-
    nl,
    write('|#################### Murus Gallicus Menu ################|'), nl,
    write('|                                                         |'), nl,
    write('| 1. Play the game                                        |'), nl,
    write('| 2. Quit                                                 |'), nl,
    write('|                                                         |'), nl,
    write('|#########################################################|'), nl.


% Resolve game choice and start the game.
resolve_game_choice(0) :- 
    murus_gallicus.
resolve_game_choice(1) :- 
    start_game.
resolve_game_choice(2) :- 
    start_game_with_bot.

resolve_choice(1) :-
    print_game_options,
    read(Choice),
    resolve_game_choice(Choice).
resolve_choice(2) :-
    halt.
resolve_choice(_) :-
    print_menu.

print_game_options :- 
    write('|###################### Game Options #####################|'), nl,
    write('|                                                         |'), nl,
    write('| 1. Player vs Player                                     |'), nl,
    write('|    - Play against another player                        |'), nl,
    write('|                                                         |'), nl,
    write('| 2. Player vs Bot                                        |'), nl,
    write('|    - Challenge the computer opponent                    |'), nl,
    write('|                                                         |'), nl,
    write('| 0. Back to Main Menu                                    |'), nl,
    write('|                                                         |'), nl,
    write('|#########################################################|'), nl.

murus_gallicus :-
    print_menu,
    read(Choice),
    resolve_choice(Choice).