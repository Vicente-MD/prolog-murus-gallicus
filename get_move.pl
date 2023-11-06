% Read and validate players move.
read_and_validate_move(StartColumn, StartRow, EndColumn, EndRow, StartColIndex, EndColIndex, StartRowInt, EndRowInt) :-
    read_position_start(StartColumn, StartRow),
    read_position_end(EndColumn, EndRow),
    column_letter_to_index(StartColumn, StartColIndex),
    column_letter_to_index(EndColumn, EndColIndex),
    atom_to_integer(StartRow, StartRowInt),
    atom_to_integer(EndRow, EndRowInt).

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