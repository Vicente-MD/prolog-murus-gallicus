:- use_module(library(lists)).
:- use_module(library(system)).
:- consult('move.pl').
:- consult('capture.pl').
:- consult('jump.pl').
:- consult('player.pl').
:- consult('utils.pl').
:- consult('pvp.pl').

play_pvb(Player, Board) :-
    format('##### Current Player: ~w #####', [Player]), nl,
    current_player_tower(Tower),
    has_towers_in_board(Tower, Board),
    (
        Player = 'white' -> % Turno do jogador humano
        (
            valid_moves(Board, Player, Moves),
            (
                Moves = [] ->
                format('Player ~w has no valid moves left. Player ~w wins!', [Player, 'Bot']), nl
                ;
                read_and_validate_move(StartColumn, StartRow, EndColumn, EndRow),
                move(Board, [StartColumn, StartRow, EndColumn, EndRow], NewBoard),
                display_game(NewBoard),
                (
                    is_win_condition(Player, EndRow) ->
                    format('Player controlling the ~w pieces wins by reaching the opponent\'s first row!', [Player]), nl
                    ;
                    change_player(Player, NextPlayer),
                    play_pvb(NextPlayer, NewBoard)
                )
            )
        )
        ;
        % Turno do bot
        bot_move(Board, BotNewBoard),
        display_game(BotNewBoard),
        change_player(Player, NextPlayer),
        play_pvb(NextPlayer, BotNewBoard)
    ).

bot_move(Board, NewBoard) :-
    current_player(Player),
    (find_first_b_tower(Board, StartColIndex, StartRowInt) ->
        possible_moves(StartColIndex, StartRowInt, Moves, Board),
        random_member((EndColIndex, EndRowInt), Moves),
        int_to_atom(StartRowInt, StartRowAtom),
        int_to_atom(EndRowInt, EndRowAtom),
        index_to_column_letter(StartCol, StartColIndex),
        index_to_column_letter(EndCol, EndColIndex),
        get_middle_and_final_content(StartColIndex, StartRowInt, EndColIndex, EndRowInt, Board, MiddleContent, FinalContent),
        (own_piece(Player, MiddleContent, FinalContent) ->
            % Player must perform a simple move.
            write('Player made a move.'), nl,
            move(Board, [StartCol, StartRowAtom, EndCol, EndRowAtom], NewBoard)
            ;
            % Players piece should be captured.
            write('Player made a capture.'), nl,
            capture_piece(Board, [StartRowInt, StartColIndex], [EndRowInt, EndColIndex], NewBoard)
        )
        ;
        write('Error: There are no b-tower pieces for the bot to move.'), nl
    ).

find_first_b_tower(Board, StartColIndex, StartRowInt) :-
    between(1, 8, StartColIndex),
    between(1, 7, StartRowInt),
    get_content(Board, StartColIndex, StartRowInt, 'b-tower').

get_content(Board, ColIndex, Row, Content) :-
    nth1(Row, Board, RowList),        % Obtenha a lista de linha apropriada
    nth1(ColIndex, RowList, Content).

% Converte um número inteiro em um átomo
int_to_atom(Int, Atom) :-
    number_codes(Int, IntChars),     % Converte o número em uma lista de códigos de caracteres
    atom_codes(Atom, IntChars).      % Converte a lista de códigos de caracteres em um átomo
