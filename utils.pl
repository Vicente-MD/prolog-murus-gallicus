:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).

% Predicado auxiliar para mapear a letra da coluna (entre 'a' e 'h') para o índice da lista (entre 1 e 8).
column_letter_to_index(Column, Index) :-
    char_code(Column, Code),
    Index is Code - 96.  % 'a' tem o código ASCII 97.

index_to_column_letter(Column, Index) :-
    Code is Index + 96,      % 'a' tem o código ASCII 97
    char_code(Column, Code).

atom_to_integer(Atom, Int) :-
    atom_chars(Atom, Chars),
    number_chars(Int, Chars).

% Predicado para substituir um elemento em uma lista.
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

% Predicado para mudar o valor de uma célula no tabuleiro.
change_cell(Board, Row, Col, NewValue, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace(OldRow, Col, NewValue, NewRow),
    replace(Board, Row, NewRow, NewBoard).

possible_moves(Col, Row, Moves, Board) :-
    MaxCol is 8,
    MaxRow is 7,
    findall((NewCol, NewRow), (
        (
            NewCol is Col + 2, NewRow is Row,
            NewCol >= 1, NewCol =< MaxCol,
            \+ has_tower(NewCol, NewRow, Board)
        );
        (
            NewCol is Col - 2, NewRow is Row,
            NewCol >= 1, NewCol =< MaxCol,
            \+ has_tower(NewCol, NewRow, Board)
        );
        (
            NewCol is Col, NewRow is Row + 2,
            NewRow >= 1, NewRow =< MaxRow,
            \+ has_tower(NewCol, NewRow, Board)
        );
        (
            NewCol is Col, NewRow is Row - 2,
            NewRow >= 1, NewRow =< MaxRow,
            \+ has_tower(NewCol, NewRow, Board)
        );
        (
            NewCol is Col + 2, NewRow is Row + 2,
            NewCol >= 1, NewCol =< MaxCol,
            NewRow >= 1, NewRow =< MaxRow,
            \+ has_tower(NewCol, NewRow, Board)
        );
        (
            NewCol is Col - 2, NewRow is Row + 2,
            NewCol >= 1, NewCol =< MaxCol,
            NewRow >= 1, NewRow =< MaxRow,
            \+ has_tower(NewCol, NewRow, Board)
        );
        (
            NewCol is Col + 2, NewRow is Row - 2,
            NewCol >= 1, NewCol =< MaxCol,
            NewRow >= 1, NewRow =< MaxRow,
            \+ has_tower(NewCol, NewRow, Board)
        );
        (
            NewCol is Col - 2, NewRow is Row - 2,
            NewCol >= 1, NewCol =< MaxCol,
            NewRow >= 1, NewRow =< MaxRow,
            \+ has_tower(NewCol, NewRow, Board)
        )
    ), Moves).

% has_tower/2 verifica se uma torre ("w-tower" ou "b-tower") está na posição (Col, Row) no tabuleiro.
has_tower(Col, Row, Board) :-
    nth1(Row, Board, RowContent),
    nth1(Col, RowContent, CellContent),
    (CellContent = 'w-tower' ; CellContent = 'b-tower').
