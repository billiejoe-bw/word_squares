
:- module(printsolution, [print_solution/2]).

:- use_module(words).


% Print the solutions in a readable format.

print_solution(Rows, Cols) :-
	
	nl,
	
	format("Row words:~n"),
	foreach(member(Row, Rows), print_word(row_word, Row)),
	nl,
	
	format("Column words:~n"),
	foreach(member(Col, Cols), print_word(col_word, Col)),
	nl,

	foreach(member(Row, Rows), print_row(Row)),
	nl.
	

print_word(PredName, Word) :-
	T =.. [PredName, Word, WordWithoutBlanks],
	call(T),
	format("  ~p~n", [WordWithoutBlanks]).
	
print_row(Row) :-
	format("|"),
	foreach(member(X, Row), format("~w|", [X])),
	nl.
	
