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

symbol(X,Y,○) :- p(X,Y,white,m), !.
symbol(X,Y,●) :- p(X,Y,black,m), !.
symbol(X,Y,♔) :- p(X,Y,white,k), !.
symbol(X,Y,♚) :- p(X,Y,black,k), !.
symbol(X,Y,࠰) :-
    legal_moves(black,Moves),
    member([_,_,X,Y,_],Moves),
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
    \+ legal_moves(P,_),
    !,
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

make_move(white) :-
    alpha_beta(white,4,-inf,inf,[X1,Y1,X2,Y2,Jumps],_),
    forall(member([X,Y],Jumps),retract(p(X,Y,_,_))),
    move_piece(X1,Y1,X2,Y2),
    !.

make_move(black) :-
    read(Move),
    atom_codes(Move,[S1,S2,S3,S4]),
    X1 is S1 - 96,
    Y1 is S2 - 48,
    X2 is S3 - 96,
    Y2 is S4 - 48,
    legal_moves(black,Moves),
    member([X1,Y1,X2,Y2,Jumps],Moves),
    forall(member([X,Y],Jumps),retract(p(X,Y,_,_))),
    move_piece(X1,Y1,X2,Y2),
    !.

make_move(black) :-
    writeln('Illegal move. Retry.'),
    make_move(black).

legal_moves(P,Moves) :-
    bagof([X1,Y1,X2,Y2,Jumps],(
        p(X1,Y1,P,T),
        empty(X2,Y2),
        capture(P,T,X1,Y1,X2,Y2,Jumps)
    ),Moves),
    !.

legal_moves(P,Moves) :-
    bagof([X1,Y1,X2,Y2,[]],(
        p(X1,Y1,P,T),
        empty(X2,Y2),
        next_row(P,T,Y1,Y2),
        next_col(X1,X2)
    ),Moves).

capture(P,T,X1,Y1,X2,Y2,[[XJ,YJ]|Jumps]) :-
    next_row(P,T,Y1,YJ),
    next_col(X1,XJ),
    opponent(P,O),
    p(XJ,YJ,O,OT),
    (OT = m; T = k),
    NewX1 is 2 * XJ - X1,
    NewY1 is 2 * YJ - Y1,
    empty(NewX1,NewY1),
    capture(P,T,NewX1,NewY1,X2,Y2,Jumps),
    !.

capture(_,_,X,Y,X,Y,[]).

next_row(white,m,Y1,Y2) :- Y2 is Y1 + 1, !.
next_row(black,m,Y1,Y2) :- Y2 is Y1 - 1, !.
next_row(_,k,Y1,Y2) :-
    (Y1 > 1, Y2 is Y1 - 1);
    (Y1 < 8, Y2 is Y1 + 1).

next_col(X1,X2) :-
    (X1 > 1, X2 is X1 - 1);
    (X1 < 8, X2 is X1 + 1).

move_piece(X1,Y1,X2,Y2) :-
    (Y2 is 1; Y2 is 8),
    !,
    retract(p(X1,Y1,P,m)),
    asserta(p(X2,Y2,P,k)).

move_piece(X1,Y1,X2,Y2) :-
    retract(p(X1,Y1,P,T)),
    asserta(p(X2,Y2,P,T)).



% Predicates to implement AI.

alpha_beta(P,Depth,Alpha,Beta,BestMove,BestVal) :-
	Depth > 0,
	legal_moves(P,Moves),
	!,
	find_best(P,Depth,Moves,Alpha,Beta,BestMove,BestVal).

alpha_beta(_,_,_,_,_,Val) :- evaluate(Val).  % No legal moves or maximum depth reached. Evaluate the board

find_best(P,Depth,[Move|Moves],Alpha,Beta,BestMove,BestVal) :-
	test_move(Move,Removed),
    NewDepth is Depth - 1,
    opponent(P,O),
	alpha_beta(O,NewDepth,Alpha,Beta,_,Val),
	undo_move(Move,Removed),
	good_enough(P,Depth,Moves,Alpha,Beta,Move,Val,BestMove,BestVal).

good_enough(_,_,[],_,_,Move,Val,Move,Val) :- !.                      % No other candidate moves
good_enough(white,_,_,_,Beta,Move,Val,Move,Val) :- Val > Beta, !.    % Maximizer attained upper bound
good_enough(black,_,_,Alpha,_,Move,Val,Move,Val) :- Val < Alpha, !.  % Minimizer attained lower bound
good_enough(P,Depth,Moves,Alpha,Beta,Move,Val,BestMove,BestVal) :-   % Refine bounds and continue
	new_bounds(P,Alpha,Beta,Val,NewAlpha,NewBeta),
	find_best(P,Depth,Moves,NewAlpha,NewBeta,NewMove,NewVal),
	better_of(P,Move,Val,NewMove,NewVal,BestMove,BestVal).

new_bounds(white,Alpha,Beta,Val,Val,Beta) :- Val > Alpha, !.  % Maximizer increased lower bound
new_bounds(black,Alpha,Beta,Val,Alpha,Val) :- Val < Beta, !.  % Minimizer decreased upper bound
new_bounds(_,Alpha,Beta,_,Alpha,Beta).                        % Otherwise bounds unchanged

better_of(white,Move,Val,_,NewVal,Move,Val) :- Val > NewVal, !.  % Move is better than NewMove. Maximizer prefers higher values 
better_of(black,Move,Val,_,NewVal,Move,Val) :- Val < NewVal, !.  % Move is better than NewMove. Minimizer prefers lower values
better_of(_,_,_,NewMove,NewVal,NewMove,NewVal).                  % Otherwise NewMove is better than Move

test_move([X1,Y1,X2,Y2,Jumps],[p(X1,Y1,P1,T1)|Removed]) :-
    p(X1,Y1,P1,T1),
    findall(p(X,Y,P,T),(member([X,Y],Jumps),p(X,Y,P,T)),Removed),
    forall(member(M,Removed),retract(M)),
    move_piece(X1,Y1,X2,Y2).

undo_move([_,_,X2,Y2,_],Removed) :-
    retract(p(X2,Y2,_,_)),
    forall(member(M,Removed),asserta(M)).



% Heuristic function.

evaluate(Score) :-
	count_pawns(white,m,WhiteMen),
    count_pawns(black,m,BlackMen),
	count_pawns(white,k,WhiteKings),
	count_pawns(black,k,BlackKings),
	State is WhiteMen - BlackMen + 2.5 * (WhiteKings - BlackKings),
	count_center(white,WhiteCenter),
	count_center(black,BlackCenter),
	Center is WhiteCenter - BlackCenter,
    count_columns(white,WhiteColumns),
    count_columns(black,BlackColumns),
    Columns is WhiteColumns - BlackColumns,
    count_progress(white,WhiteProgress),
    count_progress(black,BlackProgress),
    Progress is WhiteProgress - BlackProgress,
    count_values(white,WhiteValues),
    count_values(black,BlackValues),
    Value is WhiteValues - BlackValues,
	Score is 80 * State + 40 * Center + 40 * Columns + 20 * Progress + Value.

count_pawns(P,T,N) :-
	findall(_,p(_,_,P,T),L),
	length(L,N).

count_center(P,N) :-
	findall(_,(
        p(X,Y,P,_),
        X > 2,
        X < 7,
        Y > 2,
        Y < 7
    ),L),
	length(L,N).

count_columns(P,N) :-
    findall(_,(
        p(X,Y,P,_),
        XR is X + 2,
        p(XR,Y,P,_),
        XU is X + 1,
        next_row(P,m,Y,YU),
        p(XU,YU,P,_)
    ),L),
    length(L,N).

count_progress(P,N) :-
    (P = white, S = 0; P = black, S = 9),
    !,
    findall(abs(S - Y),p(_,Y,P,m),ManProgress),
    findall(8,p(_,_,P,k),KingProgress),
    append(ManProgress,KingProgress,L),
    sum_list(L,N).

count_values(P,N) :-
    findall(Val,(p(X,Y,P,T),value(X,Y,P,T,Val)),L),
    sum_list(L,N).

value(8,1,white,m,100) :- !.
value(_,1,white,m,120) :- !.
value(1,2,white,m,110) :- !.
value(7,2,white,m,140) :- !.
value(_,2,white,m,130) :- !.
value(8,3,white,m,120) :- !.
value(_,3,white,m,140) :- !.
value(_,4,white,m,150) :- !.
value(8,5,white,m,170) :- !.
value(_,5,white,m,160) :- !.
value(_,6,white,m,170) :- !.
value(8,7,white,m,190) :- !.
value(_,7,white,m,180) :- !.
value(1,8,white,m,190) :- !.
value(_,8,white,m,200) :- !.
value(1,8,black,m,100) :- !.
value(_,8,black,m,120) :- !.
value(8,7,black,m,110) :- !.
value(2,7,black,m,140) :- !.
value(_,7,black,m,130) :- !.
value(1,6,black,m,120) :- !.
value(_,6,black,m,140) :- !.
value(_,5,black,m,150) :- !.
value(1,4,black,m,170) :- !.
value(_,4,black,m,160) :- !.
value(_,3,black,m,170) :- !.
value(1,2,black,m,190) :- !.
value(_,2,black,m,180) :- !.
value(8,1,black,m,190) :- !.
value(_,1,black,m,200) :- !.
value(_,_,_,k,200).
