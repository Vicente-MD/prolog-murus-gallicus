

can_jump([L, C], [L2, C2], Board) :-
    L2 is L + 2, C2 is C,
    is_empty([L2, C2], Board).

can_jump([L, C], [L2, C2], Board) :-
    L2 is L - 2, C2 is C,
    is_empty([L2, C2], Board).

can_jump([L, C], [L2, C2], Board) :-
    L2 is L, C2 is C + 2,
    is_empty([L2, C2], Board).

can_jump([L, C], [L2, C2], Board) :-
    L2 is L, C2 is C - 2,
    is_empty([L2, C2], Board).

can_jump([L, C], [L2, C2], Board) :-
    L2 is L + 2, C2 is C + 2,
    is_empty([L2, C2], Board).

can_jump([L, C], [L2, C2], Board) :-
    L2 is L + 2, C2 is C - 2,
    is_empty([L2, C2], Board).

can_jump([L, C], [L2, C2], Board) :-
    L2 is L - 2, C2 is C + 2,
    is_empty([L2, C2], Board).

can_jump([L, C], [L2, C2], Board) :-
    L2 is L - 2, C2 is C - 2,
    is_empty([L2, C2], Board).

is_empty([L, C], Board) :-
    nth0(L, Board, Row),
    nth0(C, Row, '       ').

find_jump_destinations([L, C], Board, Destinations) :-
    findall([L2, C2], can_jump([L, C], [L2, C2], Board), Destinations).

get_middle_and_final_content(StartColumn, StartRow, FinalColumn, FinalRow, Board, MiddleContent, FinalContent) :-
    MiddleColumn is (StartColumn + FinalColumn) // 2,
    MiddleRow is (StartRow + FinalRow) // 2,
    get_content_at([MiddleRow, MiddleColumn], Board, MiddleContent),
    get_content_at([FinalRow, FinalColumn], Board, FinalContent).

get_content_at([L, C], Board, Content) :-
    nth1(L, Board, Row),  % Select the row using nth1/3
    nth1(C, Row, Content).

verify_if_can_jump('white', MiddleContent, FinalContent) :-
    \+ (MiddleContent = 'b-tower'; FinalContent = 'b-tower').

verify_if_can_jump('black', MiddleContent, FinalContent) :-
    \+ (MiddleContent = 'w-tower'; FinalContent = 'w-tower').

own_piece('white', MiddleContent, FinalContent) :-
    \+ (MiddleContent = ' b-wall'; FinalContent = ' b-wall').

own_piece('black', MiddleContent, FinalContent) :-
    \+ (MiddleContent = ' w-wall'; FinalContent = ' w-wall').
