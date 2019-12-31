
:- module(words, [assert_words/2, row_word/2, col_word/2]).
:- dynamic row_word/2, col_word/2.

:- use_module(library(random), [random_permutation/2, setrand/1]).

read_file(Stream,[]) :-
    at_end_of_stream(Stream),
    !.

read_file(Stream,[X|L]) :-
    read_line_to_codes(Stream,X),
    read_file(Stream,L).


% Encode a word as a list of two-letter atoms.

encode_word([], []).

encode_word([W,Z|Zs], [X|Xs]) :-
    char_type(W, lower),
    char_type(Z, lower),
    atom_codes(X, [W, Z]),
    encode_word(Zs, Xs).

intersperse([], [], []).

intersperse([X|Xs], Ys, [X|Zs]) :-
    intersperse(Xs, Ys, Zs).

intersperse(Xs, [Y|Ys], [Y|Zs]) :-
    intersperse(Xs, Ys, Zs).


% Generate all the legal ways of putting blanks into the word.

add_blanks(Word, N, Res) :-
    length(Word, M),
	N >= M,
    M * 2 >= N,
    K is N - M,
	K > 0,
    length(Blanks, K),
    maplist(=('  '), Blanks),
    intersperse(Word, Blanks, Res).


word_with_blanks(N, WordAsAtom, WordWithBlanks) :-  % N is number of pairs, not letters.
	open('c:\\puzzle\\words', read, Str),
    read_file(Str, Lines),
    close(Str),
	setrand(rand(10,432,8363)),
	random_permutation(Lines, ShuffledLines),
    member(Line, ShuffledLines),
	atom_codes(WordAsAtom, Line),
	encode_word(Line, Word),
	add_blanks(Word, N, WordWithBlanks).

	
assert_words_1(PredName, N) :-
	word_with_blanks(N, WordAsAtom, WordWithBlanks),
	Term =.. [PredName, WordWithBlanks, WordAsAtom],
	assert(Term),
	fail.

assert_words_1(_, _).


% Assert all the legal options for the rows and columns.

assert_words(NumRows, NumCols) :-
	retractall(row_word(_, _)),
	retractall(col_word(_, _)),
	assert_words_1(row_word, NumCols),
	assert_words_1(col_word, NumRows),
	aggregate_all(count, row_word(X, Y), NumRowWords),
	aggregate_all(count, col_word(X, Y), NumColWords),
	format("No. of row possibilities: ~p~n", [NumRowWords]),
	format("No. of column possibilities: ~p~n", [NumColWords]).
