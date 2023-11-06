:- consult(menu).
:- consult(play).
:- consult(board).
:- consult(player).

% Resolve game choice and start the game.
resolve_game_choice(0) :- 
    murus_gallicus.
resolve_game_choice(1) :- 
    initial_board(Board),
    display_board(Board),
    current_player(CurrentPlayer),
    opposite_player(OtherPlayer),
    play_pvp(Board).
    % play_pvp(Board, CurrentPlayer, ' w-wall', OtherPlayer, ' b-wall').
resolve_game_choice(2) :- 
    murus_gallicus.

resolve_choice(1) :-
    print_game_options,
    read(Choice),
    resolve_game_choice(Choice).
resolve_choice(_) :- false.

murus_gallicus :-
    print_menu,
    read(Choice),
    resolve_choice(Choice).