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
