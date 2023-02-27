% Evaluate the board state using a basic heuristic function.
evaluate_board(basic,Score) :-
	count_pieces(white,m,WhiteMen),
    count_pieces(black,m,BlackMen),
	count_pieces(white,k,WhiteKings),
	count_pieces(black,k,BlackKings),
	Score is WhiteMen - BlackMen + 1.5 * (WhiteKings - BlackKings).


% Evaluate the board state using an optimized heuristic function.
evaluate_board(improved,Score) :-
	count_pieces(white,m,WhiteMen),
    count_pieces(black,m,BlackMen),
	count_pieces(white,k,WhiteKings),
	count_pieces(black,k,BlackKings),
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


% Count pieces of a given type.
count_pieces(Player,Fig,N) :-
	findall(_,p(_,_,Player,Fig),L),
	length(L,N).


% Count pieces in the center of the board.
count_center(Player,N) :-
	findall(_,(
        p(X,Y,Player,_),
        X > 2,
        X < 7,
        Y > 2,
        Y < 7
    ),L),
	length(L,N).


% Count formations of three pieces connected in the same direction.
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


% Count progress of pieces towards the opponent's side.
count_progress(Player,N) :-
    (Player = white, Start = 1, !; Player = black, Start = 8),
    findall(abs(Start - Y),p(_,Y,Player,m),L),
    sum_list(L,Sum),
    count_pieces(Player,k,Kings),
    N is Sum + 7 * Kings.


% Count value of pieces according to their position on the board.
count_values(Player,N) :-
    findall(Val,(p(X,Y,Player,Fig),value(X,Y,Player,Fig,Val)),L),
    sum_list(L,N).


% Assign a value to each square of the board.
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
