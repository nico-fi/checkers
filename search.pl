% Implementation of the alpha-beta search adapted from the book:
% "Bratko, I. (2011). Prolog Programming for Artificial Intelligence (4th ed.). Addison-Wesley Educational, p. 585"


% Perform a heuristic alpha-beta search to find the best move for a player.
alpha_beta_search(Player,Depth,Alpha,Beta,BestMove,BestVal) :-
	Depth > 0,
	legal_moves(Player,Moves),
	!,
	best(Player,Depth,Moves,Alpha,Beta,BestMove,BestVal).


% If maximum depth is reached or no legal moves are available, evaluate the board.
alpha_beta_search(_,_,_,_,_,Val) :- evaluate_board(Val).


% Select the best move from a list of candidates. Best is either maximum or minimum, depending on the player.
best(Player,Depth,[Move|Moves],Alpha,Beta,BestMove,BestVal) :-
	simulate_move(Move,Removed),
	NewDepth is Depth - 1,
	opponent(Player,Opp),
	alpha_beta_search(Opp,NewDepth,Alpha,Beta,_,Val),
	undo_move(Move,Removed),
	good_enough(Player,Depth,Moves,Alpha,Beta,Move,Val,BestMove,BestVal).


% Check if a move is sufficiently good to make the correct decision. If not, try the next candidate move.
good_enough(_,_,[],_,_,Move,Val,Move,Val) :- !.                           		% No other candidate moves
good_enough(white,_,_,_,Beta,Move,Val,Move,Val) :- Val > Beta, !.         		% Maximizer attains upper bound
good_enough(black,_,_,Alpha,_,Move,Val,Move,Val) :- Val < Alpha, !.       		% Minimizer attains lower bound
good_enough(Player,Depth,Moves,Alpha,Beta,Move,Val,BestMove,BestVal) :-  % Otherwise refine bounds and continue
	new_bounds(Player,Alpha,Beta,Val,NewAlpha,NewBeta),
	best(Player,Depth,Moves,NewAlpha,NewBeta,NewMove,NewVal),
	is_better(Player,Move,Val,NewMove,NewVal,BestMove,BestVal).


% Determine a new (Alpha,Beta) interval.
new_bounds(white,Alpha,Beta,Val,Val,Beta) :- Val > Alpha, !.     % Maximizer increases lower bound
new_bounds(black,Alpha,Beta,Val,Alpha,Val) :- Val < Beta, !.     % Minimizer decreases upper bound
new_bounds(_,Alpha,Beta,_,Alpha,Beta).                           % Otherwise bounds unchanged


% Determine which of two moves is better.
is_better(white,Move,Val,_,NewVal,Move,Val) :- Val > NewVal, !.  % Move is better than NewMove. Maximizer prefers higher values 
is_better(black,Move,Val,_,NewVal,Move,Val) :- Val < NewVal, !.  % Move is better than NewMove. Minimizer prefers lower values
is_better(_,_,_,NewMove,NewVal,NewMove,NewVal).                  % Otherwise NewMove is better than Move
