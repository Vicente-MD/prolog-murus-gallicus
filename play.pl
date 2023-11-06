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
        format('middle content |~w|', [MiddleContent]),nl,
        format('final content |~w|', [FinalContent]),nl,
        (
            verify_if_can_jump(Player, MiddleContent, FinalContent) ->
            (
                write('can jump'),nl,
                own_piece(Player, MiddleContent, FinalContent) ->
                % Player must perform a simple move.
                write('own piece'),nl,
                simple_move(Board, StartColumn, StartRow, EndColumn, EndRow, NewBoard),
                display_board(NewBoard)
                ;
                % Players piece should be captured.
                capture_piece(Board, [StartRowInt, StartColIndex], [EndRowInt, EndColIndex], NewBoard),
                display_board(NewBoard)
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

% Read and validate players move.
read_and_validate_move(StartColumn, StartRow, EndColumn, EndRow, StartColIndex, EndColIndex, StartRowInt, EndRowInt) :-
    read_position_start(StartColumn, StartRow),
    read_position_end(EndColumn, EndRow),
    column_letter_to_index(StartColumn, StartColIndex),
    column_letter_to_index(EndColumn, EndColIndex),
    atom_to_integer(StartRow, StartRowInt),
    atom_to_integer(EndRow, EndRowInt).

% Check if a player has reached the first row.
is_win_condition(Player, EndRowInt) :- 
    (Player = 'b-tower', EndRowInt = 1 ; Player = 'w-tower', EndRowInt = 7).

% Switch players and their corresponding pieces for the next turn.
switch_players(Player, SoloPiece, Opponent, SoloOpponent, NextPlayer, NextSoloPiece, OtherOpponent, NextSoloOpponent) :-
    (Player = 'w-tower' -> NextPlayer = 'b-tower' ; NextPlayer = 'w-tower'),
    (SoloPiece = ' w-wall' -> NextSoloPiece = ' b-wall' ; NextSoloPiece = ' w-wall'),
    (Opponent = 'b-tower' -> OtherOpponent = 'w-tower' ; OtherOpponent = 'b-tower'),
    (SoloOpponent = ' b-wall' -> NextSoloOpponent = ' w-wall' ; NextSoloOpponent = ' b-wall').


read_position_start(Column, Row) :-
    write('Enter start position '),
    read(Position),
    atom_chars(Position, [Column|RowList]),
    nth0(0, RowList, Row).

read_position_end(Column, Row) :-
    write('Enter end position '),
    read(Position),
    atom_chars(Position, [Column|RowList]),
    nth0(0, RowList, Row).


