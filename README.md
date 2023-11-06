 # Programação Funcional e em Lógica

## Game 

Murus Gallicus

## Group Murus Gallicus_1

*Murus Gallicus_1*

- Vicente Meireles Damasceno 202301454  (50%)
- Joana Rita Batista Marques 202103346 (50%)

## Installation and Execution

In order to execute the Game, it is necessary to download and install SICStus Prolog 4.8. After opening SICStus, user must consult `mail.pl` file of the game source folder. To initiate the game, user must enter starting predicate play/0, and the Game Menu will appear with a set of options for the user to choose from.


## Description of the game

Murus Gallicus, is a two-player strategy 8x7 board game, whose goal is for one of the players to reach the other's farthest edge of the board. The Game players are Romans (light pieces) or Gauls (dark pieces). In the begining of the game, each player has 16 pieces, stacked in a pile of 2 pieces (that form a Tower) spread over the player initial board line. Individual pieces are called walls and a stack of 2 pieces is called a Tower.

In our game, Roman player pieces are represented as either ```w-tower``` or ```w-wall``` and Gaul player pieces are represented as either ```b-tower``` or ```b-wall```

### Game Play:
Starting with the Roman player, the players alternate taking turns and player can not pass a turn. On a player’s turn one of the following actions is allowed:
-Move a tower by distributing its two stones from its initial cell into the two nearest cells in any one direction (orthogonal or diagonal). Each destination cell must be empty or contain a friendly wall.
-Sacrifice a tower stone to remove an adjacent (orthogonal or diagonal) enemy wall. Sacrifice is not forced by the presence of an adjacent enemy wall.


## Game Logic

### Internal Game State Representation

To represent the game state, we used a list of lists to represent the current state of the game's board. The initial_board predicate receives the starting positions of the game board.

-Example of the Prolog representation of initial state:

```prolog
initial_board([
    ['w-tower', 'w-tower', 'w-tower', 'w-tower', 'w-tower', 'w-tower', 'w-tower', 'w-tower'],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['b-tower', 'b-tower', 'b-tower', 'b-tower', 'b-tower', 'b-tower', 'b-tower', 'b-tower']
]).
```

-Example of the Prolog representation of intermediate state:

```prolog
([
    ['       ', 'w-tower', '       ', 'w-tower', '       ', '       ', 'w-tower', 'w-tower'],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    [' w-wall', '       ', '       ', '       ', 'w-tower', '       ', '       ', '       '],
    [' w-wall', '       ', '       ', ' b-wall', '       ', '       ', '       ', '       '],
    ['       ', '       ', ' b-wall', '       ', '       ', 'b-tower', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['b-tower', '       ', 'b-tower', '       ', '       ', '       ', 'b-tower', 'b-tower']
]).
```

-Example of the Prolog representation of final state:

```prolog
([
    ['       ', '       ', 'w-tower', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', ' b-wall', '       ', '       ', '       '],
    ['       ', '       ', '       ', ' b-wall', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', '       ', '       ', '       ', '       ', '       ', '       ', '       '],
    ['       ', 'w-tower', '       ', '       ', '       ', 'b-tower', '       ', '       ']
]).
```


### Game State Visualization

#### Menu

We implemented a user friendly menu that displays available options for the user (print_menu/0), where initially use must choose to play or quit the game. Aftewards user should enter one of the options and depending on the option the user has choosen, another menu appears for the user to choose between game modes `Player vs Player` or `Player vs Bot` (print_game_options/0).

The Game state visualization can be obtained through `display_game` predicate that receives the current `GameState` as input and prints out the current board game display.

It is also used other printing utility predicates such as print_board_rows, print_horizontal_line, print_row, and print_column_labels to print the game board in a formatted way. The print_board_rows predicate prints each row of the game board, while the print_row predicate prints each cell in a row. The print_horizontal_line predicate prints a horizontal line to separate each row and the print_column_labels predicate prints the column labels for the game board.

### Move Validation and Execution

To validate and execute a play, we implemented the `play_pvp` predicate. It starts by checking which player's turn it is and prompts them to enter a move.
The `read_and_validate_move` predicate is used to read and validate the player's move. It checks if the move is valid according to the game predicates using the `verify_if_can_jump predicate`. If the move is valid, it updates the game board using either the `simple_move` or `capture_piece` predicates, depending on the situation. In case the move is invalid, it asks the player to enter a new move.

After the move is executed, the `is_win_condition` predicate is used to verify if the player has won by reaching the opponent's first row. If the player has won, the game ends and the winner is declared. If the player has not won, the `switch_players` predicate is used to switch to the next player's turn and their corresponding pieces. The `play_pvp` predicate is then called recursively with the updated board and the next player's turn. If a player has no valid moves left, the game ends and the other player is declared the winner.



## Conclusions

Creating this project was a significant challenge for us, due to Prologs declarative nature. The Prolog logic approach is very different from other programming paradigms we are familiar with. Specially because to this, it tooks us quite some time initially to figure out the best tactics to implement the game using prolog language.

In particular, one of the most challenging taks for us was to implement a solid way to validate game moves, particularly for situations involving stacked pieces.
With time and practice, as well as some trial-and-error, we were able to overcome this challenges and started to enjoy Prolog programming. Unfortunatelly, due to a very limited amount of time to fully dedicate ourselves to the project, there were a few features we would have liked to implement, but no longer had time to. One of the taks we would haved like to have implemented was to create a harder level for the user to play against the Bot by implementing a backtrack algorithm solution that it would yield the move with higher chances of succeed in winning the game.

Overall, this project allowed us to expand our programming skills as well as broaden our ways of thinking in order to solve a problem.


## Bibliography

-https://sites.google.com/site/theowlsnest02/home/murus-gallicus
-https://www.iggamecenter.com/en/rules/murusgallicus