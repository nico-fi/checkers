% Predicates to define the state of the board.

:- dynamic p/4.

initialize_board :-
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

coord(C) :- member(C,[1,2,3,4,5,6,7,8]).

empty(X,Y) :-
    coord(X),
    coord(Y),
    \+ p(X,Y,_,_).



% Predicates to display the board.

print_board :-
    tty_clear,
    writeln('   ┌───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┐'),
    print_row(8),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(7),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(6),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(5),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(4),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(3),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(2),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(1),
    writeln('   └───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┘'),
    writeln('       a       b       c       d       e       f       g       h    '),
    nl.

print_row(Y) :-
    findall(S,(coord(X),symbol(X,Y,S)),L),
    writeln('   │       │       │       │       │       │       │       │       │'),
    format(' ~w │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │~n', [Y|L]),
    writeln('   │       │       │       │       │       │       │       │       │').

symbol(X,Y,○) :-
    p(X,Y,white,m),
    !.

symbol(X,Y,●) :-
    p(X,Y,black,m),
    !.

symbol(X,Y,♔) :-
    p(X,Y,white,k),
    !.

symbol(X,Y,♚) :-
    p(X,Y,black,k),
    !.

symbol(X,Y,࠰) :-
    legal_move(white,_,_,X,Y,_),
    !.

symbol(_,_,' ').



% Predicates to manage the game.

:- writeln('To start a game, type play.').

play :-
    initialize_board,
    print_board,
    writeln('Play moves by entering coordinates followed by a period. For example: b3c4.'),
    nl,
    turn(white).

turn(P) :-
    \+ legal_move(P,_,_,_,_,_),
    opponent(P,O),
    format('Winner: ~w!', O).

turn(P) :-
    make_move(P),
    print_board,
    opponent(P,O),
    turn(O).

opponent(white,black).
opponent(black,white).



% Predicates to perform a move.

make_move(P) :-
    read(Move),
    atom_codes(Move,[S1,S2,S3,S4]),
    X1 is S1 - 96,
    Y1 is S2 - 48,
    X2 is S3 - 96,
    Y2 is S4 - 48,
    legal_move(P,X1,Y1,X2,Y2,Jumps),
    forall(member([X,Y],Jumps),retract(p(X,Y,_,_))),
    move_piece(X1,Y1,X2,Y2).

make_move(P) :-
    writeln('Illegal move. Retry.'),
    make_move(P).

legal_move(P,X1,Y1,X2,Y2,Jumps) :-
    p(X1,Y1,P,T),
    empty(X2,Y2),
    capture(P,T,X1,Y1,X2,Y2,Jumps).

legal_move(P,X1,Y1,X2,Y2,[]) :-
    p(X1,Y1,P,T),
    empty(X2,Y2),
    next_row(P,T,Y1,Y2),
    next_col(X1,X2),
    \+ legal_move(P,_,_,_,_,[_|_]).

capture(P,T,X1,Y1,X2,Y2,[[XJ,YJ]|Jumps]) :-
    next_row(P,T,Y1,YJ),
    next_col(X1,XJ),
    opponent(P,O),
    p(XJ,YJ,O,OT),
    (OT = m; T = k),
    NewX1 is 2 * XJ - X1,
    NewY1 is 2 * YJ - Y1,
    empty(NewX1,NewY1),
    capture(P,T,NewX1,NewY1,X2,Y2,Jumps).

capture(P,T,X,Y,X,Y,[]) :-
    \+ capture(P,T,X,Y,_,_,[_|_]).

next_row(white,m,Y1,Y2) :-
    coord(Y1),
    Y2 is Y1 + 1.

next_row(black,m,Y1,Y2) :-
    coord(Y1),
    Y2 is Y1 - 1.

next_row(_,k,Y1,Y2) :-
    coord(Y1),
    (Y2 is Y1 - 1; Y2 is Y1 + 1).

next_col(X1,X2) :- (X2 is X1 - 1; X2 is X1 + 1).

move_piece(X1,Y1,X2,Y2) :-
    (Y2 is 1; Y2 is 8),
    retract(p(X1,Y1,P,m)),
    asserta(p(X2,Y2,P,k)).

move_piece(X1,Y1,X2,Y2) :-
    retract(p(X1,Y1,P,T)),
    asserta(p(X2,Y2,P,T)).



% Predicates to implement AI.



