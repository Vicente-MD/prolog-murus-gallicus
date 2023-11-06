:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).
:- consult('board.pl').
:- consult('player.pl').
:- consult('utils.pl').

% Predicado para realizar um movimento no tabuleiro.
simple_move(Board, StartColumn, StartRow, EndColumn, EndRow, NewBoard) :-
    current_player(Player),
    current_player_wall(PlayerWall),

    atom_to_integer(StartRow, StartRowInt),
    atom_to_integer(EndRow, EndRowInt),
    column_letter_to_index(EndColumn, EndColumnInt),
    column_letter_to_index(StartColumn, StartColumnInt),

    write('...simple move...'),nl,

    format('player |~w|', [Player]),nl,
    format('wall |~w|', [PlayerWall]),nl,
    format('start col |~w|', [StartColumnInt]),nl,
    format('end col |~w|', [EndColumnInt]),nl,
    % Verifica se as coordenadas estão dentro dos limites do tabuleiro.
    (between(1, 7, StartRowInt), between(1, 8, StartColumnInt), between(1, 7, EndRowInt), between(1, 8, EndColumnInt)),

    % Verifica se a posição de início contém uma peça do jogador atual.
    nth1(StartRowInt, Board, Row),
    nth1(StartColumnInt, Row, InitialPiece),
    current_player_tower(PlayerTower),
    InitialPiece = PlayerTower,


    % Calcula a linha e coluna intermediárias.
    MiddleRow is (StartRowInt + EndRowInt) // 2,
    MiddleCol is (StartColumnInt + EndColumnInt) // 2,


    % Verifica se a posição intermediária está vazia ou contém uma peça PlayerWall.
    nth1(MiddleRow, Board, MiddleRowList),
    nth1(MiddleCol, MiddleRowList, DestinationPiece),
    
    remove_piece(Board, StartRowInt, StartColumnInt, TempBoard),
    
    % Faz a mudança na posição intermediária.
    (DestinationPiece = '       ' -> change_cell(TempBoard, MiddleRow, MiddleCol, PlayerWall, IntermediateBoard);
        DestinationPiece = PlayerWall -> change_cell(TempBoard, MiddleRow, MiddleCol, PlayerTower, IntermediateBoard)
    ),

    % Verifica se a posição de destino está vazia ou contém uma peça PlayerWall.
    nth1(EndRowInt, IntermediateBoard, EndRowList),
    nth1(EndColumnInt, EndRowList, DestinationPiece2),

    % Faz a mudança na posição de destino.
    (DestinationPiece2 = '       ' -> change_cell(IntermediateBoard, EndRowInt, EndColumnInt, PlayerWall, NewBoard);
        DestinationPiece2 = PlayerWall -> change_cell(IntermediateBoard, EndRowInt, EndColumnInt, PlayerTower, NewBoard)
    ).

% Predicate to remove a piece at a specific row and column on the board.
remove_piece(Board, Row, Col, NewBoard) :-
    nth1(Row, Board, BoardRow),     % Select the row using nth1/3
    replace(BoardRow, Col, '       ', NewRow), % Replace the element in the row using replace/4
    replace(Board, Row, NewRow, NewBoard). % Replace the row in the board using replace/4
