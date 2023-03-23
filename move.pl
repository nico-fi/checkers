:- module(move,[legal_moves/2,empty_square/2,next_row/4,next_col/2,jump/8,move_piece/5,simulate_move/2,undo_move/2,save_state/1,restore_state/1]).


% Find mandatory jump moves for the player.
legal_moves(Player,Moves) :-
    bagof([X1,Y1,X2,Y2,Jumps],Fig^(
        p(X1,Y1,Player,Fig),
        jump(Player,Fig,X1,Y1,X2,Y2,[],Jumps)
    ),Candidates),
    longest_jumps(Candidates,Moves),
    !.


% If no jump move is available for the player, find non-jump moves. Fail if no legal move is available.
legal_moves(Player,Moves) :-
    bagof([X1,Y1,X2,Y2,[]],Fig^(
        p(X1,Y1,Player,Fig),
        next_row(Player,Fig,Y1,Y2),
        next_col(X1,X2),
        empty_square(X2,Y2)
    ),Moves).


% Check if a dark square with odd column and even row is empty.
empty_square(X,Y) :-
    member(X,[1,3,5,7]),
    member(Y,[2,4,6,8]),
    \+ p(X,Y,_,_).


% Check if a dark square with even column and odd row is empty.
empty_square(X,Y) :-
    member(X,[2,4,6,8]),
    member(Y,[1,3,5,7]),
    \+ p(X,Y,_,_).


% Determine the next legal row for a given piece.
next_row(white,m,Y1,Y2) :- Y2 is Y1 + 1, !.
next_row(black,m,Y1,Y2) :- Y2 is Y1 - 1, !.
next_row(_,k,Y1,Y2) :-
    (Y1 > 1, Y2 is Y1 - 1);
    (Y1 < 8, Y2 is Y1 + 1).


% Determine the next legal column.
next_col(X1,X2) :-
    (X1 > 1, X2 is X1 - 1);
    (X1 < 8, X2 is X1 + 1).


% For a given piece, recursively jump over enemy pieces.
jump(Player,Fig,X1,Y1,X2,Y2,PrevJumps,[[XJ,YJ]|Jumps]) :-
    next_row(Player,Fig,Y1,YJ),
    next_col(X1,XJ),
    opponent(Player,Opp),
    p(XJ,YJ,Opp,OppFig),
    (OppFig = m; Fig = k),
    NewX1 is 2 * XJ - X1,
    NewY1 is 2 * YJ - Y1,
    empty_square(NewX1,NewY1),
    \+ member([XJ,YJ],PrevJumps),
    jump(Player,Fig,NewX1,NewY1,X2,Y2,[[XJ,YJ]|PrevJumps],Jumps).


% Base case for the recursive jump predicate.
jump(_,_,X,Y,X,Y,[_|_],[]).


% Select moves with longest jump sequence.
longest_jumps(Moves,Longest) :-
    findall(Len,(member([_,_,_,_,Jumps],Moves),length(Jumps,Len)),Lengths),
    max_list(Lengths,Max),
    findall([X1,Y1,X2,Y2,Jumps],(member([X1,Y1,X2,Y2,Jumps],Moves),length(Jumps,Max)),Longest).


% Move a piece (possibly promoting it) and remove captured pieces.
move_piece(X1,Y1,X2,Y2,Jumps) :-
    (Y2 is 1; Y2 is 8),
    !,
    forall(member([X,Y],Jumps),retract(p(X,Y,_,_))),
    retract(p(X1,Y1,Player,_)),
    assert(p(X2,Y2,Player,k)).


% Move a piece and remove captured pieces.
move_piece(X1,Y1,X2,Y2,Jumps) :-
    forall(member([X,Y],Jumps),retract(p(X,Y,_,_))),
    retract(p(X1,Y1,Player,Fig)),
    assert(p(X2,Y2,Player,Fig)).


% Simulate a move and store the removed pieces.
simulate_move([X1,Y1,X2,Y2,Jumps],[p(X1,Y1,Player1,Fig1)|Removed]) :-
    p(X1,Y1,Player1,Fig1),
    findall(p(X,Y,Player,Fig),(member([X,Y],Jumps),p(X,Y,Player,Fig)),Removed),
    move_piece(X1,Y1,X2,Y2,Jumps).


% Undo a move and restore the removed pieces.
undo_move([_,_,X2,Y2,_],Removed) :-
    retract(p(X2,Y2,_,_)),
    forall(member(Piece,Removed),assert(Piece)).


% Save the current state of the board.
save_state(State) :-
    findall(p(X,Y,Player,Fig),p(X,Y,Player,Fig),State).


% Restore a given state of the board.
restore_state(State) :-
    retractall(p(_,_,_,_)),
    forall(member(Piece,State),assert(Piece)).
