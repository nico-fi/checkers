:- dynamic p/4, cpu/1.

% Predicates to display the board.

print_board(Player) :-
    legal_moves(Player,Moves),
    tty_clear,
    writeln('   ┌───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┐'),
    print_row(8,Moves),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(7,Moves),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(6,Moves),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(5,Moves),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(4,Moves),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(3,Moves),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(2,Moves),
    writeln('   ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤'),
    print_row(1,Moves),
    writeln('   └───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┘'),
    writeln('       a       b       c       d       e       f       g       h    '),
    nl,
    nl.

print_row(Y,Moves) :-
    findall(S,(member(X,[1,2,3,4,5,6,7,8]),symbol(X,Y,Moves,S)),L),
    writeln('   │       │       │       │       │       │       │       │       │'),
    format(' ~w │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │   ~w   │~n', [Y|L]),
    writeln('   │       │       │       │       │       │       │       │       │').

symbol(X,Y,_,'○') :- p(X,Y,white,m), !.
symbol(X,Y,_,'●') :- p(X,Y,black,m), !.
symbol(X,Y,_,'♔') :- p(X,Y,white,k), !.
symbol(X,Y,_,'♚') :- p(X,Y,black,k), !.
symbol(X,Y,Moves,'𐤟') :- member([_,_,X,Y,_],Moves), !.
symbol(_,_,_,' ').



% Predicates to manage the game.

:- writeln('To start a game, type play.').

play :-
    initialize_board,
    select_color,
    print_board(white),
    turn(white).

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

select_color :-
    writeln('Select your color [white./black.]: '),
    read(Player),
    (member(Player,[white,black]); select_color),
    !,
    opponent(Player,O),
    asserta(cpu(O)).

opponent(white,black).
opponent(black,white).

turn(Player) :-
    \+ legal_moves(Player,_),
    !,
    opponent(Player,O),
    format('Winner: ~w!', O).

turn(Player) :-
    cpu(Player),
    !,
    alpha_beta_search(Player,5,-inf,inf,[X1,Y1,X2,Y2,Jumps],_),
    move_pieces(X1,Y1,X2,Y2,Jumps),
    opponent(Player,O),
    print_board(O),
    turn(O).

turn(Player) :-
    legal_moves(Player,Moves),
    writeln('Enter coordinates followed by a period. For example: b3c4.\n'),
    read_move(Moves,[X1,Y1,X2,Y2,Jumps]),
    move_pieces(X1,Y1,X2,Y2,Jumps),
    opponent(Player,O),
    print_board(O),
    turn(O).

read_move(Moves,[X1,Y1,X2,Y2,Jumps]) :-
    read(Move),
    atom_codes(Move,[C1,C2,C3,C4]),
    X1 is C1 - 96,
    Y1 is C2 - 48,
    X2 is C3 - 96,
    Y2 is C4 - 48,
    member([X1,Y1,X2,Y2,Jumps],Moves),
    !.

read_move(Moves,Move) :-
    writeln('Illegal move. Try again.'),
    read_move(Moves,Move).



% Predicates to perform a move.

legal_moves(Player,Moves) :-
    bagof([X1,Y1,X2,Y2,Jumps],Fig^(
        p(X1,Y1,Player,Fig),
        empty_square(X2,Y2),
        capture(Player,Fig,X1,Y1,X2,Y2,[],Jumps)
    ),Candidates),
    check_priority(Candidates,Moves),
    !.

legal_moves(Player,Moves) :-
    bagof([X1,Y1,X2,Y2,[]],Fig^(
        p(X1,Y1,Player,Fig),
        empty_square(X2,Y2),
        next_row(Player,Fig,Y1,Y2),
        next_col(X1,X2)
    ),Moves).

capture(Player,Fig,X1,Y1,X2,Y2,PrevJumps,[[XJ,YJ]|Jumps]) :-
    next_row(Player,Fig,Y1,YJ),
    next_col(X1,XJ),
    \+ member([XJ,YJ],PrevJumps),
    opponent(Player,O),
    p(XJ,YJ,O,OFig),
    (OFig = m; Fig = k),
    NewX1 is 2 * XJ - X1,
    NewY1 is 2 * YJ - Y1,
    empty_square(NewX1,NewY1),
    capture(Player,Fig,NewX1,NewY1,X2,Y2,[[XJ,YJ]|PrevJumps],Jumps),
    !.

capture(_,_,X,Y,X,Y,_,[]).

check_priority(Candidates,Moves) :-
    findall(Len,(member([_,_,_,_,Jumps],Candidates),length(Jumps,Len)),Lengths),
    max_list(Lengths,Max),
    findall([X1,Y1,X2,Y2,Jumps],(member([X1,Y1,X2,Y2,Jumps],Candidates),length(Jumps,Max)),Moves).

empty_square(X,Y) :-
    member(X,[1,3,5,7]),
    member(Y,[2,4,6,8]),
    \+ p(X,Y,_,_).

empty_square(X,Y) :-
    member(X,[2,4,6,8]),
    member(Y,[1,3,5,7]),
    \+ p(X,Y,_,_).

next_row(white,m,Y1,Y2) :- Y2 is Y1 + 1, !.
next_row(black,m,Y1,Y2) :- Y2 is Y1 - 1, !.
next_row(_,k,Y1,Y2) :-
    (Y1 > 1, Y2 is Y1 - 1);
    (Y1 < 8, Y2 is Y1 + 1).

next_col(X1,X2) :-
    (X1 > 1, X2 is X1 - 1);
    (X1 < 8, X2 is X1 + 1).

move_pieces(X1,Y1,X2,Y2,Jumps) :-
    (Y2 is 1; Y2 is 8),
    !,
    forall(member([X,Y],Jumps),retract(p(X,Y,_,_))),
    retract(p(X1,Y1,Player,_)),
    asserta(p(X2,Y2,Player,k)).

move_pieces(X1,Y1,X2,Y2,Jumps) :-
    forall(member([X,Y],Jumps),retract(p(X,Y,_,_))),
    retract(p(X1,Y1,Player,Fig)),
    asserta(p(X2,Y2,Player,Fig)).



% Predicates to implement AI.

alpha_beta_search(Player,Depth,Alpha,Beta,BestMove,BestVal) :-
	Depth > 0,
	legal_moves(Player,Moves),
	!,
	find_best(Player,Depth,Moves,Alpha,Beta,BestMove,BestVal).

alpha_beta_search(_,_,_,_,_,Val) :- evaluate(Val).  % No legal moves or maximum depth reached. Evaluate the board

find_best(Player,Depth,[Move|Moves],Alpha,Beta,BestMove,BestVal) :-
	test_move(Move,Removed),
    NewDepth is Depth - 1,
    opponent(Player,O),
	alpha_beta_search(O,NewDepth,Alpha,Beta,_,Val),
	undo_move(Move,Removed),
	good_enough(Player,Depth,Moves,Alpha,Beta,Move,Val,BestMove,BestVal).

good_enough(_,_,[],_,_,Move,Val,Move,Val) :- !.                      % No other candidate moves
good_enough(white,_,_,_,Beta,Move,Val,Move,Val) :- Val > Beta, !.    % Maximizer attained upper bound
good_enough(black,_,_,Alpha,_,Move,Val,Move,Val) :- Val < Alpha, !.  % Minimizer attained lower bound
good_enough(Player,Depth,Moves,Alpha,Beta,Move,Val,BestMove,BestVal) :-   % Refine bounds and continue
	new_bounds(Player,Alpha,Beta,Val,NewAlpha,NewBeta),
	find_best(Player,Depth,Moves,NewAlpha,NewBeta,NewMove,NewVal),
	better_of(Player,Move,Val,NewMove,NewVal,BestMove,BestVal).

new_bounds(white,Alpha,Beta,Val,Val,Beta) :- Val > Alpha, !.  % Maximizer increased lower bound
new_bounds(black,Alpha,Beta,Val,Alpha,Val) :- Val < Beta, !.  % Minimizer decreased upper bound
new_bounds(_,Alpha,Beta,_,Alpha,Beta).                        % Otherwise bounds unchanged

better_of(white,Move,Val,_,NewVal,Move,Val) :- Val > NewVal, !.  % Move is better than NewMove. Maximizer prefers higher values 
better_of(black,Move,Val,_,NewVal,Move,Val) :- Val < NewVal, !.  % Move is better than NewMove. Minimizer prefers lower values
better_of(_,_,_,NewMove,NewVal,NewMove,NewVal).                  % Otherwise NewMove is better than Move

test_move([X1,Y1,X2,Y2,Jumps],[p(X1,Y1,P1,T1)|Removed]) :-
    p(X1,Y1,P1,T1),
    findall(p(X,Y,Player,Fig),(member([X,Y],Jumps),p(X,Y,Player,Fig)),Removed),
    move_pieces(X1,Y1,X2,Y2,Jumps).

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

count_pawns(Player,Fig,N) :-
	findall(_,p(_,_,Player,Fig),L),
	length(L,N).

count_center(Player,N) :-
	findall(_,(
        p(X,Y,Player,_),
        X > 2,
        X < 7,
        Y > 2,
        Y < 7
    ),L),
	length(L,N).

count_columns(Player,N) :-
    findall(_,(
        p(X,Y,Player,_),
        XR is X + 2,
        p(XR,Y,Player,_),
        XU is X + 1,
        next_row(Player,m,Y,YU),
        p(XU,YU,Player,_)
    ),L),
    length(L,N).

count_progress(Player,N) :-
    (Player = white, S = 0; Player = black, S = 9),
    !,
    findall(abs(S - Y),p(_,Y,Player,m),ManProgress),
    findall(8,p(_,_,Player,k),KingProgress),
    append(ManProgress,KingProgress,L),
    sum_list(L,N).

count_values(Player,N) :-
    findall(Val,(p(X,Y,Player,Fig),value(X,Y,Player,Fig,Val)),L),
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
