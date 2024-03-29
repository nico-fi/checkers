:- module(draw,[print_logo/0, print_board/1]).


% Print logo and instructions.
print_logo :- writeln('
───────────────────────────────────────────────────────────────
 ░░░░░░ ░░   ░░ ░░░░░░░  ░░░░░░ ░░   ░░ ░░░░░░░ ░░░░░░  ░░░░░░░ 
▒▒      ▒▒   ▒▒ ▒▒      ▒▒      ▒▒  ▒▒  ▒▒      ▒▒   ▒▒ ▒▒      
▒▒      ▒▒▒▒▒▒▒ ▒▒▒▒▒   ▒▒      ▒▒▒▒▒   ▒▒▒▒▒   ▒▒▒▒▒▒  ▒▒▒▒▒▒▒ 
▓▓      ▓▓   ▓▓ ▓▓      ▓▓      ▓▓  ▓▓  ▓▓      ▓▓   ▓▓      ▓▓ 
 ██████ ██   ██ ███████  ██████ ██   ██ ███████ ██   ██ ███████ 
───────────────────────────────────────────────────────────────
    
To move the pieces, enter coordinates followed by a dot. Example: b3c4.\n\n').


% Print the current state of the board and highlight the legal moves for the player.
print_board(Player) :-
    (legal_moves(Player,Moves), !; Moves = []),
    tty_clear,
    writeln('   ╔═══════╤═══════╤═══════╤═══════╤═══════╤═══════╤═══════╤═══════╗'),
    print_row(8,Moves),
    writeln('   ╟───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────╢'),
    print_row(7,Moves),
    writeln('   ╟───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────╢'),
    print_row(6,Moves),
    writeln('   ╟───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────╢'),
    print_row(5,Moves),
    writeln('   ╟───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────╢'),
    print_row(4,Moves),
    writeln('   ╟───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────╢'),
    print_row(3,Moves),
    writeln('   ╟───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────╢'),
    print_row(2,Moves),
    writeln('   ╟───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────╢'),
    print_row(1,Moves),
    writeln('   ╚═══════╧═══════╧═══════╧═══════╧═══════╧═══════╧═══════╧═══════╝'),
    writeln('       a       b       c       d       e       f       g       h    \n\n').


% Print a given row of the board.
print_row(Y,LegalMoves) :-
    findall(S,(member(X,[1,2,3,4,5,6,7,8]),symbol(X,Y,LegalMoves,S)),L),
    writeln('   ║       │       │       │       │       │       │       │       ║'),
    format(' ~w ║   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   ║\n', [Y|L]),
    writeln('   ║       │       │       │       │       │       │       │       ║').


% Assign a symbol to each square of the board.
symbol(X,Y,_,'○') :- p(X,Y,white,m), !.
symbol(X,Y,_,'●') :- p(X,Y,black,m), !.
symbol(X,Y,_,'♔') :- p(X,Y,white,k), !.
symbol(X,Y,_,'♚') :- p(X,Y,black,k), !.
symbol(X,Y,LegalMoves,'𐤟') :- member([_,_,X,Y,_],LegalMoves), !.
symbol(_,_,_,' ').
