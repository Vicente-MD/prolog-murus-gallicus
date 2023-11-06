:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).
:- consult('board.pl').
:- consult('player.pl').
:- consult('utils.pl').

% Predicate that makes moves
% move(+GameState, +Move, -NewGameState)
% Predicate for a move in the "Murus Gallicus" game.
move(Board, [StartColumn, StartRow, EndColumn, EndRow], NewBoard) :-
    current_player(Player),
    current_player_wall(PlayerWall),
    current_player_tower(PlayerTower),

    % Convert row and column atoms to integers.
    atom_to_integer(StartRow, StartRowInt),
    atom_to_integer(EndRow, EndRowInt),
    column_letter_to_index(EndColumn, EndColumnInt),
    column_letter_to_index(StartColumn, StartColumnInt),

    % Validate if the move is within the board boundaries.
    valid_move_coordinates(StartRowInt, StartColumnInt, EndRowInt, EndColumnInt),

    % Verify if the selected piece is a tower and of the players color.
    validate_selected_piece(Board, StartRowInt, StartColumnInt),

    % Calculate middle row and column.
    MiddleRow is (StartRowInt + EndRowInt) // 2,
    MiddleCol is (StartColumnInt + EndColumnInt) // 2,

    % Verify if it is an empty space or a PlayerWall at the middle position.
    validate_middle_position(Board, MiddleRow, MiddleCol, DestinationPiece, StartRowInt, StartColumnInt, PlayerWall, IntermediateBoard),

    % Verify if the destination position is empty or contains a PlayerWall.
    validate_destination_position(IntermediateBoard, EndRowInt, EndColumnInt, DestinationPiece2, PlayerWall, NewBoard).

% Predicate to check if the move coordinates are within the board boundaries.
valid_move_coordinates(StartRow, StartCol, EndRow, EndCol) :-
    between(1, 7, StartRow),
    between(1, 8, StartCol),
    between(1, 7, EndRow),
    between(1, 8, EndCol).

% Predicate to validate the selected piece.
validate_selected_piece(Board, StartRow, StartCol) :-
    current_player_tower(PlayerTower),
    nth1(StartRow, Board, Row),
    nth1(StartCol, Row, InitialPiece),
    (InitialPiece = PlayerTower ->
        true; % Selected piece is a tower and of the players color
        write('Error: Invalid piece selection.\n'),
        fail
    ).


% Predicate to validate the middle position.
validate_middle_position(Board, MiddleRow, MiddleCol, DestinationPiece, StartRowInt, StartColumnInt, PlayerWall, IntermediateBoard) :-
    nth1(MiddleRow, Board, MiddleRowList),
    nth1(MiddleCol, MiddleRowList, DestinationPiece),

    remove_piece(Board, StartRowInt, StartColumnInt, TempBoard),
    current_player_tower(PlayerTower),

    (DestinationPiece = '       ' ->
        change_cell(TempBoard, MiddleRow, MiddleCol, PlayerWall, IntermediateBoard);
        DestinationPiece = PlayerWall ->
        change_cell(TempBoard, MiddleRow, MiddleCol, PlayerTower, IntermediateBoard);
        write('Error: Invalid middle position.\n'),
        fail
    ).

% Predicate to validate the destination position.
validate_destination_position(Board, EndRow, EndCol, DestinationPiece, PlayerWall, NewBoard) :-
    nth1(EndRow, Board, EndRowList),
    nth1(EndCol, EndRowList, DestinationPiece),
    current_player_tower(PlayerTower),

    (DestinationPiece = '       ' ->
        change_cell(Board, EndRow, EndCol, PlayerWall, NewBoard);
        DestinationPiece = PlayerWall ->
        change_cell(Board, EndRow, EndCol, PlayerTower, NewBoard);
        write('Error: Invalid destination position.\n'),
        fail
    ).


% Predicate to remove a piece at a specific row and column on the board.
remove_piece(Board, Row, Col, NewBoard) :-
    nth1(Row, Board, BoardRow),     % Select the row using nth1/3
    replace(BoardRow, Col, '       ', NewRow), % Replace the element in the row using replace/4
    replace(Board, Row, NewRow, NewBoard). % Replace the row in the board using replace/4
