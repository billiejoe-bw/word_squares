
:- use_module(words).
:- use_module(printsolution, [print_solution/2]).
:- use_module(library(clpfd), [transpose/2]).


solution_template(0, _, []) :- !.

solution_template(NumRows, NumCols, [Row|Rows]) :-
	NumRows > 0,
	length(Row, NumCols),
	N is NumRows - 1,
	solution_template(N, NumCols, Rows).	
	

solve(NumRows, NumCols, WordsUsed, Rows, Cols) :-
	assert_words(NumRows, NumCols),
	solution_template(NumRows, NumCols, Rows),
	transpose(Rows, Cols),
	fill_in(next_row, WordsUsed, Rows, Cols).
	
	
fill_in(_, _, [], []) :- !.

fill_in(next_row, WordsUsed, [], Cols) :-
	!,
	fill_in(next_col, WordsUsed, [], Cols).

fill_in(next_col, WordsUsed, Rows, []) :-
	!,
	fill_in(next_row, WordsUsed, Rows, []).

fill_in(next_row, WordsUsed, [Row|Rows], Cols) :-
	!,
	row_word(Row, RowWord),
	\+ member(RowWord, WordsUsed),
	
	% Check that every column can still be completed into a word.
	foreach(member(Col, Cols), \+ \+ once(col_word(Col, _))),

	fill_in(next_col, [RowWord|WordsUsed], Rows, Cols).
	
fill_in(next_col, WordsUsed, Rows, [Col|Cols]) :-
	!,
	col_word(Col, ColWord),
	\+ member(ColWord, WordsUsed),
	
	% Check that every row can still be completed into a word.
	foreach(member(Row, Rows), \+ \+ once(row_word(Row, _))),
	
	fill_in(next_row, [ColWord|WordsUsed], Rows, Cols).


% Invoke this to generate the word squares.

generate_squares(NumRows, NumCols) :-
	solve(NumRows, NumCols, [], Rows, Cols),
	print_solution(Rows, Cols).


