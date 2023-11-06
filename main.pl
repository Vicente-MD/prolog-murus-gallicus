:- consult(menu).
:- consult(play).
:- consult(board).
:- consult(pvp).
:- consult(pvbot).

% Resolve game choice and start the game.
resolve_game_choice(0) :- 
    play.
resolve_game_choice(1) :- 
    initial_state(Board),
    display_game(Board),
    current_player(CurrentPlayer),
    opposite_player(OtherPlayer),
    play_pvp(Board).
resolve_game_choice(2) :- 
    initial_state(Board),
    display_game(Board),
    play_pvb(Board).

resolve_choice(1) :-
    print_game_options,
    read(Choice),
    resolve_game_choice(Choice).
resolve_choice(_) :- false.

play :-
    print_menu,
    read(Choice),
    resolve_choice(Choice).