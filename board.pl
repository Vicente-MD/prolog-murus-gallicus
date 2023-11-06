% Define the board as an 8x7 array.
initial_board([
    ['w-tower', 'w-tower', 'w-tower', 'w-tower', 'w-tower', 'w-tower', 'w-tower', 'w-tower'],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['b-tower', 'b-tower', 'b-tower', 'b-tower', 'b-tower', 'b-tower', 'b-tower', 'b-tower']
]).

% Predicate to display the board.
display_board(Board) :- nl,nl,print_board(Board),nl.


% Print the board.
print_board(Board) :-
    nl,
    print_board_rows(Board, 1),
    print_horizontal_line,
    print_column_labels,
    nl.

% Printing utility predicates.
print_board_rows([], _).
print_board_rows([Row | Rest], N) :-
    print_horizontal_line,
    write(N),
    write(' '),
    print_row(Row),
    nl,
    N1 is N + 1,
    print_board_rows(Rest, N1).
print_horizontal_line :- write('   +---------+---------+---------+---------+---------+---------+---------+---------+'), nl.
print_row([]).
print_row([Cell | Rest]) :- 
    write(' | '), 
    write(Cell),
    print_row(Rest).
print_column_labels :- 
    write('        A          B         C         D         E         F         G         H'), 
    nl.