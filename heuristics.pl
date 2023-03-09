% Evaluate the board state using a heuristic function.
evaluate_board(inf) :- \+ legal_moves(black,_), !.
evaluate_board(-inf) :- \+ legal_moves(white,_), !.
evaluate_board(Score) :-
    count_pieces(white,m,WhiteMen),
    count_pieces(black,m,BlackMen),
    Men is WhiteMen - BlackMen,
    count_pieces(white,k,WhiteKings),
    count_pieces(black,k,BlackKings),
    Kings is WhiteKings - BlackKings,
    count_center(white,WhiteCenter),
    count_center(black,BlackCenter),
    Center is WhiteCenter - BlackCenter,
    count_arrows(white,WhiteArrows),
    count_arrows(black,BlackArrows),
    Arrows is WhiteArrows - BlackArrows,
    count_progress(white,WhiteProgress),
    count_progress(black,BlackProgress),
    Progress is WhiteProgress - BlackProgress,
    count_back(white,WhiteBack),
    count_back(black,BlackBack),
    Back is WhiteBack - BlackBack,
    count_threats(white,WhiteThreats),
    count_threats(black,BlackThreats),
    Threats is WhiteThreats - BlackThreats,
    count_mobility(white,WhiteMobility),
    count_mobility(black,BlackMobility),
    Mobility is WhiteMobility - BlackMobility,
    Score is 2 * Men + 3 * Kings + 0.2 * Center + 0.5 * Arrows + 0.2 * Progress + Back + Threats + 0.2 * Mobility.


% For a given player, count pieces of a given type.
count_pieces(Player,Fig,N) :-
    findall(_,p(_,_,Player,Fig),L),
    length(L,N).


% For a given player, count pieces in the center of the board.
count_center(Player,N) :-
    findall(_,(
        p(X,Y,Player,_),
        X > 2,
        X < 7,
        Y > 2,
        Y < 7
    ),L),
    length(L,N).


% For a given player, count men in the back row.
count_back(Player,N) :-
    (Player = white, Back = 1, !; Player = black, Back = 8),
    findall(_,p(_,Back,Player,m),L),
    length(L,N).


% For a given player, count formations of three pieces connected in the opponent's direction.
count_arrows(Player,N) :-
    findall(_,(
        p(X,Y,Player,_),
        XR is X + 2,
        p(XR,Y,Player,_),
        XU is X + 1,
        next_row(Player,m,Y,YU),
        p(XU,YU,Player,_)
    ),L),
    length(L,N).


% For a given player, count progress of pieces towards the opponent's side.
count_progress(Player,N) :-
    (Player = white, Back = 1, !; Player = black, Back = 8),
    findall(abs(Back - Y),p(_,Y,Player,m),L),
    sum_list(L,Sum),
    count_pieces(Player,k,Kings),
    N is Sum + 7 * Kings.


% For a given player, count possible jumps on the opponent's pieces.
count_threats(Player,N) :-
    findall(Jumps,(
        p(X1,Y1,Player,Fig),
        jump(Player,Fig,X1,Y1,_,_,[],Jumps)
    ),AllJumps),
    append(AllJumps,L),
    length(L,N).


% For a given player, count possible non-jump moves.
count_mobility(Player,N) :-
    findall(_,(
        p(X1,Y1,Player,Fig),
        next_row(Player,Fig,Y1,Y2),
        next_col(X1,X2),
        empty_square(X2,Y2)
    ),L),
    length(L,N).
