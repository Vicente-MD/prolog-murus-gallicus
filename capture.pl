:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).
:- consult('player.pl').
:- consult('utils.pl').

capture_piece(Board,[StartRow,StartCol],[EndRow,EndCol],NewBoard):-
    current_player(Player),
    current_player_wall(PlayerWall),
    remove_piece(Board, StartRow, StartCol, TempBoard),

    % Check if the middle position is empty or if contains a PlayerWall.
    select_piece(TempBoard, StartRow, StartCol, DestinationPiece),
    (DestinationPiece = '       ' -> change_cell(TempBoard,StartRow,StartCol,PlayerWall,IntermediateBoard)),

    remove_piece(IntermediateBoard, EndRow, EndCol, NewBoard).

% Predicate to remove a piece at a specific row and column on the board.
remove_piece(Board, Row, Col, NewBoard) :-
    nth1(Row, Board, BoardRow),     % Select the row using nth1/3
    replace(BoardRow, Col, '       ', NewRow), % Replace the element in the row using replace/4
    replace(Board, Row, NewRow, NewBoard). % Replace the row in the board using replace/4

% Predicate to select a piece at a specific row and column on the board.
select_piece(Board, Row, Col, Piece) :-
    nth1(Row, Board, BoardRow),  % Select the row using nth1/3
    nth1(Col, BoardRow, Piece).  % Select the element in the row using nth1/3
