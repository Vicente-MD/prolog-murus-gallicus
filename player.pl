:- dynamic current_player/1.  % Declare current_player/1 como dinâmico para permitir a alteração.

% Inicializa o jogador atual com "white".
set_current_player :-
    retractall(current_player(_)),  % Remove qualquer valor anterior.
    asserta(current_player('white')).  % Define o jogador atual como "white".

% Predicado para alternar entre "white" e "black".
change_player :-
    current_player(Current),
    (Current = 'white' -> NewPlayer = 'black' ; NewPlayer = 'white'),
    retract(current_player(Current)),  % Remove o jogador atual.
    asserta(current_player(NewPlayer)).  % Define o novo jogador atual.

% Predicado para imprimir o jogador que não é o atual.
opposite_player(OtherPlayer) :-
    current_player(Current),
    (Current = 'white' -> OtherPlayer = 'black' ; OtherPlayer = 'white').

current_player_wall(SinglePiece) :-
    current_player(Current),
    (Current = 'white' -> SinglePiece = ' w-wall' ; Current = 'black' -> SinglePiece = ' b-wall').

current_player_tower(Tower) :-
    current_player(Current),
    (Current = 'white' -> SinglePiece = 'w-tower' ; Current = 'black' -> SinglePiece = 'b-tower').

opposite_player_wall(SinglePiece) :-
    current_player(Current),
    (Current = 'white' -> SinglePiece = ' b-wall' ; Current = 'black' -> SinglePiece = ' w-wall').

% Inicialize o jogador atual no início do jogo.
:- set_current_player.
