:- use_module([move,search,heuristics,draw]).


% Predicate p(X,Y,Player,Fig) represents a piece on the board.
% Predicate cpu(Player) represents the color assigned to the CPU.
:- dynamic p/4, cpu/1.


% Welcome message.
?- writeln('To start a game, type play.').


% Start a new game.
play :-
    print_logo,
    initialize_board,
    select_color,
    print_board(white),
    turn(white).


% Initialize the game with 12 white men and 12 black men.
initialize_board :-
    retractall(cpu(_)),
    retractall(p(_,_,_,_)),
    Whites = [[2,1],[4,1],[6,1],[8,1],[1,2],[3,2],[5,2],[7,2],[2,3],[4,3],[6,3],[8,3]],
    Blacks = [[1,6],[3,6],[5,6],[7,6],[2,7],[4,7],[6,7],[8,7],[1,8],[3,8],[5,8],[7,8]],
    forall(member([X,Y],Whites),assert(p(X,Y,white,m))),
    forall(member([X,Y],Blacks),assert(p(X,Y,black,m))).


% Ask the user to select the color to play with.
select_color :-
    write('Select your color [white./black.]: '),
    read(Player),
    opponent(Player,Opp),
    assert(cpu(Opp)),
    !;
    select_color.


% Match a player to his opponent.
opponent(white,black).
opponent(black,white).


% Check if the game is over.
turn(Player) :-
    \+ legal_moves(Player,_),
    opponent(Player,Opp),
    format('Winner: ~w!', Opp),
    !.


% Play a new CPU turn.
turn(Player) :-
    cpu(Player),
    !,
    writeln('Thinking...\n'),
    search_move(Player,4,3,[X1,Y1,X2,Y2,Jumps]),
    move_piece(X1,Y1,X2,Y2,Jumps),
    opponent(Player,Opp),
    print_board(Opp),
    C1 is X1 + 96,
    C2 is Y1 + 48,
    C3 is X2 + 96,
    C4 is Y2 + 48,
    atom_codes(Move,[C1,C2,C3,C4]),
    format('Last CPU move: ~w.\n\n', [Move]),
    turn(Opp).


% Play a new user turn.
turn(Player) :-
    legal_moves(Player,Moves),
    write('Your turn '),
    read_move(Moves,[X1,Y1,X2,Y2,Jumps]),
    move_piece(X1,Y1,X2,Y2,Jumps),
    opponent(Player,Opp),
    print_board(Opp),
    turn(Opp).


% Read a legal move from the keyboard.
read_move(LegalMoves,[X1,Y1,X2,Y2,Jumps]) :-
    read(Move),
    atom_codes(Move,[C1,C2,C3,C4]),
    X1 is C1 - 96,
    Y1 is C2 - 48,
    X2 is C3 - 96,
    Y2 is C4 - 48,
    member([X1,Y1,X2,Y2,Jumps],LegalMoves),
    !.


% Ask the user to enter a new legal move.
read_move(LegalMoves,Move) :-
    write('Try again '),
    read_move(LegalMoves,Move).
