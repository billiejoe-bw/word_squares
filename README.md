# Christmas puzzler 2019: a word squares puzzle

### The puzzle

This Prolog code solves a puzzle to generate word squares of a particular kind:

- Each square contains 2 letters or no letters
- Each row and each column must contain an uncapitalised word from `/usr/share/dict/words`
- When reading across or down, use both letters from each square to make words, ignoring the blanks
- 50% or more of the cells in each row and column must be filled
- At least one square in each row and column must be blank
- No repeated words

Here are a couple of examples:

```
|tr|an|sp|ro|se|  |
|ia|tr|ic|  |  |al|
|li|  |  |st|  |er|
|st|  |  |ra|to|se|
|  |al|al|  |us|  |
```

```
|un|co|us|  |  |
|li|mp|  |in|  |
|ke|  |tu|  |pa|
|ab|  |la|te|  |
|ly|ri|  |  |st|
|  |se|  |nd|er|
|  |  |te|er|er|
```

As you can see, there are some pretty weird "words" in `/usr/share/dict/words`.


### My solution

- My solution is developed using SWI-Prolog 8.0.3.
- To run the code, start the Prolog interpreter and type `consult(solve).` to load in the `solve.pl` program. Then type for example `generate_squares(5, 3).` to generate solutions with 5 rows and 3 columns. When it finds a solution, the program will print it like this:

```
Row words:
  uncous
  cinema
  starting

Column words:
  unci
  cost
  usar
  neti
  mang

|un|co|us|  |  |
|ci|  |  |ne|ma|
|  |st|ar|ti|ng|
```
- You can then hit semi-colon ; to get another solution, or press return if you're done.


### The algorithm

- The basic strategy is a depth-first backtracking search; Prolog's execution model is depth-first backtracking search, so the language seems a reasonable fit for the problem.

- The code alternates between filling in a full row and filling in a full column. Rows are fill from the top down; columns are filled from left to right.

- The actual search predicate `fill_in/4` relies heavily on _unification_, a feature which isn't found in many programming languages but subsumes both assignment and pattern matching.

- Each time a row (resp. column) is filled, the program checks that all columns (resp. rows) can still be completed into a word. The idea of this is to abandon choices that can't lead to a solution as early as possible. (Because of Prolog's "negation as failure" thing, you can use a double negation `\+ \+` to make sure that completions are possible without committing to them.)


### A final idea

If you only care about generating some larger solutions (and don't care whether the program will exhaustively find _all_ solutions), a reasonable strategy seems to be to calculate two solutions _without any blanks_ and then, as long as they don't contain any words in common, you can glue them together like this:

```
|re|li|st|  |  |  |
|li|br|al|  |  |  |
|be|et|le|  |  |  |
|ra|ti|ng|  |  |  |
|te|st|er|  |  |  |
|  |  |  |re|wi|re|
|  |  |  |di|st|ad|
|  |  |  |st|on|er|
|  |  |  |ra|wi|sh|
|  |  |  |in|sh|ip|
```

Some minimal edits to the code will generate solutions of this form.
