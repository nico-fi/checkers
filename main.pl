:- [drawing,move,search,heuristics].


% Predicate p(X,Y,Player,Fig) represents a piece on the board.
% Predicate cpu(Player) represents the color assigned to the CPU.
:- dynamic p/4, cpu/1.


% Welcome message.
:- writeln('To start a game, type play.').


% Start a new game.
play :-
    initialize_board,
    select_color,
    print_board(white),
    turn(white).


% Initialize the board with 12 white men and 12 black men.
initialize_board :-
    retractall(cpu(_)),
    retractall(p(_,_,_,_)),
    assert(p(2,1,white,m)),
    assert(p(4,1,white,m)),
    assert(p(6,1,white,m)),
    assert(p(8,1,white,m)),
    assert(p(1,2,white,m)),
    assert(p(3,2,white,m)),
    assert(p(5,2,white,m)),
    assert(p(7,2,white,m)),
    assert(p(2,3,white,m)),
    assert(p(4,3,white,m)),
    assert(p(6,3,white,m)),
    assert(p(8,3,white,m)),
    assert(p(1,6,black,m)),
    assert(p(3,6,black,m)),
    assert(p(5,6,black,m)),
    assert(p(7,6,black,m)),
    assert(p(2,7,black,m)),
    assert(p(4,7,black,m)),
    assert(p(6,7,black,m)),
    assert(p(8,7,black,m)),
    assert(p(1,8,black,m)),
    assert(p(3,8,black,m)),
    assert(p(5,8,black,m)),
    assert(p(7,8,black,m)).


% Ask the user to select his color.
select_color :-
    writeln('Select your color [white./black.]: '),
    read(Player),
    (member(Player,[white,black]); select_color),
    !,
    opponent(Player,O),
    asserta(cpu(O)).


% Match a player to his opponent.
opponent(white,black).
opponent(black,white).


% Check if the game is over.
turn(Player) :-
    \+ legal_moves(Player,_),
    !,
    opponent(Player,O),
    format('Winner: ~w!', O).


% Play a new CPU turn.
turn(Player) :-
    cpu(Player),
    !,
    alpha_beta_search(Player,4,-inf,inf,[X1,Y1,X2,Y2,Jumps],_),
    move_piece(X1,Y1,X2,Y2,Jumps),
    opponent(Player,O),
    print_board(O),
    turn(O).


% Play a new user turn.
turn(Player) :-
    legal_moves(Player,Moves),
    writeln('Enter coordinates followed by a period. For example: b3c4.\n'),
    read_move(Moves,[X1,Y1,X2,Y2,Jumps]),
    move_piece(X1,Y1,X2,Y2,Jumps),
    opponent(Player,O),
    print_board(O),
    turn(O).


% Read a move from the keyboard.
read_move(Moves,[X1,Y1,X2,Y2,Jumps]) :-
    read(Move),
    atom_codes(Move,[C1,C2,C3,C4]),
    X1 is C1 - 96,
    Y1 is C2 - 48,
    X2 is C3 - 96,
    Y2 is C4 - 48,
    member([X1,Y1,X2,Y2,Jumps],Moves),
    !.

% Ask the user to enter a new legal move.
read_move(Moves,Move) :-
    writeln('Illegal move. Try again.'),
    read_move(Moves,Move).