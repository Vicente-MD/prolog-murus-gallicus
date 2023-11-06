:- use_module(library(lists)).
:- use_module(library(system)).
:- consult('move.pl').
:- consult('capture.pl').
:- consult('jump.pl').
:- consult('player.pl').
:- consult('utils.pl').

% play player vs player rule
% play_pvp(Board, Player, SoloPiece, Opponent, SoloOpponent) :-
play_pvp(Board) :-
    (
        current_player(Player),
        format('##### Current Player: ~w #####', [Player]),nl,
        current_player_tower(Tower),
        has_towers_in_board(Tower, Board) ->
        read_and_validate_move(StartColumn, StartRow, EndColumn, EndRow, StartColIndex, EndColIndex, StartRowInt, EndRowInt),
        get_middle_and_final_content(StartColIndex, StartRowInt, EndColIndex, EndRowInt, Board, MiddleContent, FinalContent),
        possible_moves(StartColIndex, StartRowInt, Moves),
        (
            verify_if_can_jump(Player, MiddleContent, FinalContent) ->
            (
                own_piece(Player, MiddleContent, FinalContent) ->
                % Player must perform a simple move.
                move(Board, [StartColumn, StartRow, EndColumn, EndRow], NewBoard),
                display_game(NewBoard)
                ;
                % Players piece should be captured.
                capture_piece(Board, [StartRowInt, StartColIndex], [EndRowInt, EndColIndex], NewBoard),
                display_game(NewBoard)
            )
            ;
            % Invalid move.
            write('Error: there is a tower, you cannot move like that.'), nl
        ),
        (
            (is_win_condition(Player, EndRowInt) ->
            format('Player controlling the ~w pieces wins by reaching the opponent\'s first row!', [Player]), nl
            ;
            % switch_players(Player, SoloPiece, Opponent, SoloOpponent, NextPlayer, NextSoloPiece, OtherOpponent, NextSoloOpponent),
            % play_pvp(NewBoard, NextPlayer, NextSoloPiece, OtherOpponent, NextSoloOpponent)
            change_player,
            play_pvp(NewBoard)
            )
        )
        ;
        format('Player ~w has no valid moves left. Player ~w wins!', [Player, Opponent]), nl
    ).

% Rule to check if a value is present in any sublist of a list of lists.
has_towers_in_board(_, []) :- fail.
has_towers_in_board(Value, [Sublist | _]) :- has_value(Value, Sublist).
has_towers_in_board(Value, [_ | Tail]) :- has_towers_in_board(Value, Tail).

has_value(_, []) :- fail.
has_value(Value, [Value | _]).
has_value(Value, [_ | Tail]) :- has_value(Value, Tail).

% Check if a player has reached the first row.
is_win_condition(Player, EndRowInt) :- 
    (Player = 'black', EndRowInt = 1 ; Player = 'white', EndRowInt = 7).

% Read and validate players move.
read_and_validate_move(StartColumn, StartRow, EndColumn, EndRow, StartColIndex, EndColIndex, StartRowInt, EndRowInt) :-
    (
        write('..vou ler...'), nl,
        attempt_move(StartColumn, StartRow, EndColumn, EndRow, StartColIndex, EndColIndex, StartRowInt, EndRowInt) ->
        (
            % Verifique se o movimento final é possível
            possible_moves(StartColIndex, StartRowInt, PossibleMoves),
            write(PossibleMoves), nl,
            write(EndColIndex), write(EndRowInt), nl,
            (member((EndColIndex, EndRowInt), PossibleMoves) ->
                true
            ; 
                write('Error: Invalid final move. The desired final position is not reachable from the starting position.\n'),
                fail
            )
        ;
            write('Error: Invalid move. You can only move two cells in any direction.\n'),
            fail
        )
    ).

read_and_validate_move(StartColumn, StartRow, EndColumn, EndRow, StartColIndex, EndColIndex, StartRowInt, EndRowInt) :-
    write('Error: Invalid move. You can only move two cells in any direction.\n'),
    read_and_validate_move(StartColumn, StartRow, EndColumn, EndRow, StartColIndex, EndColIndex, StartRowInt, EndRowInt).


% Attempt to read and validate the move.
attempt_move(StartColumn, StartRow, EndColumn, EndRow, StartColIndex, EndColIndex, StartRowInt, EndRowInt) :-
    read_position_start(StartColumn, StartRow),
    read_position_end(EndColumn, EndRow),
    column_letter_to_index(StartColumn, StartColIndex),
    column_letter_to_index(EndColumn, EndColIndex),
    atom_to_integer(StartRow, StartRowInt),
    atom_to_integer(EndRow, EndRowInt).

possible_moves(Col, Row, Moves) :-
    MaxCol is 8,
    MaxRow is 7,
    findall((NewCol, NewRow), (   % Encontre todas as possíveis novas posições
        (NewCol is Col + 2, NewRow is Row, NewCol >= 1, NewCol =< MaxCol);
        (NewCol is Col - 2, NewRow is Row, NewCol >= 1, NewCol =< MaxCol);
        (NewCol is Col, NewRow is Row + 2, NewRow >= 1, NewRow =< MaxRow);
        (NewCol is Col, NewRow is Row - 2, NewRow >= 1, NewRow =< MaxRow);
        (NewCol is Col + 2, NewRow is Row + 2, NewCol >= 1, NewCol =< MaxCol, NewRow >= 1, NewRow =< MaxRow);
        (NewCol is Col - 2, NewRow is Row + 2, NewCol >= 1, NewCol =< MaxCol, NewRow >= 1, NewRow =< MaxRow);
        (NewCol is Col + 2, NewRow is Row - 2, NewCol >= 1, NewCol =< MaxCol, NewRow >= 1, NewRow =< MaxRow);
        (NewCol is Col - 2, NewRow is Row - 2, NewCol >= 1, NewCol =< MaxCol, NewRow >= 1, NewRow =< MaxRow)
    ), Moves).

% Read a position from the user.
read_position_start(Column, Row) :-
    write('Enter start position (e.g. a1, d1) '),
    read(Position),
    atom_chars(Position, [Column|RowList]),
    nth0(0, RowList, Row).

read_position_end(Column, Row) :-
    write('Enter end position (e.g. a3, e3)'),
    read(Position),
    atom_chars(Position, [Column|RowList]),
    nth0(0, RowList, Row).



