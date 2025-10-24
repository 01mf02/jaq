# Examples

The following examples should give an impression of what jaq can currently do.
You should obtain the same outputs by replacing jaq with jq.

Access a field:

    $ echo '{"a": 1, "b": 2}' | jaq '.a'
    1

Add values:

    $ echo '{"a": 1, "b": 2}' | jaq 'add'
    3

Construct an array from an object in two ways and show that they are equal:

    $ echo '{"a": 1, "b": 2}' | jaq '[.a, .b] == [.[]]'
    true

Apply a filter to all elements of an array and filter the results:

    $ echo '[0, 1, 2, 3]' | jaq 'map(.*2) | [.[] | select(. < 5)]'
    [0, 2, 4]

Read (slurp) input values into an array and get the average of its elements:

    $ echo '1 2 3 4' | jaq -s 'add / length'
    2.5

Repeatedly apply a filter to itself and output the intermediate results:

    $ echo '0' | jaq '[recurse(.+1; . < 3)]'
    [0, 1, 2]

Lazily fold over inputs and output intermediate results:

    $ seq 1000 | jaq -n 'foreach inputs as $x (0; . + $x)'
    1 3 6 10 15 [...]


## Lewis's Puzzle

The following puzzle was communicated to me at a workshop by a certain Mr. Lewis,
where I solved it together with him in jq.
It goes as follows:

> We have a sequence of strings:
> 
> ~~~
> X
> XYZX
> XYZXABXYZX
> ~~~
> 
> For example, the 4th letter of the 2nd string (always counting from zero) is 'A'.
> The task is to find the 10244th letter of the 30th string.

First, let us understand how this sequence is built.
To get the next sequence of letters,
we take the previous sequence,
concatenate it with the next two letters in the alphabet,
then concatenate it with the previous sequence again.

If we take numbers instead of letters, we can write this down as:

~~~
X(0  ) = 0
X(N+1) = X(N) (m+1) (m+2) X(N), where m is the largest element in X(N)
~~~

We can now write the strings as JSON arrays.
The first array is `[0]`, and we can produce each following array by
a filter `next`, on which we `recurse` to get an sequence of all arrays.

~~~
def next: . + [max + (1, 2)] + .;
[0] | limit(3; recurse(next)) -->
[0]
[0,1,2,0]
[0,1,2,0,3,4,0,1,2,0]
~~~

However, this does not scale well --- getting to the 30th array
will take a very long time, because the arrays grow exponentially.
Feel free to try it, but watch out not to get your RAM eaten. :)
(I recommend monitoring RAM usage when doing this experiment.
Otherwise, you may very well crash your computer due to memory exhaustion.
Guess how I know?)

To solve this problem, we can exploit jq's sharing.
Note that each array contains
a portion to the left that is equal to
a portion on the right;
for example, `[0, 1, 2, 0]` in the 2nd array.
We can therefore choose a slightly different array representation that
allows us to *share* all the equal parts of the array, just by
inserting the previous arrays into a new array.

~~~
def next: [., .[2] + (1,2), .];
[0] | limit(3; recurse(next)) -->
[0]
[[0],1,2,[0]]
[[[0],1,2,[0]],3,4,[[0],1,2,[0]]]
~~~

In all arrays produced by `next`, the first and the last elements are now
shared, meaning that they are stored only a single time in memory.
That allows us to store exponentially large data in linear memory,
thus cracking the puzzle.

To get the largest number of the previous array, we used the fact that
the 2nd element of each array contains the largest number in the array.
For example,
the 2nd element of the 1st array is `2`, and
the 2nd element of the 2nd array is `4`.
(For the 0th array, the 2nd element is `null`,
but the maximum element of that array is `0`.
However, `null` is interpreted by addition just like `0`,
so this difference does not matter to us.)
We can therefore get the two next largest numbers very elegantly via
`.[2] + (1, 2)`.

We can now get the numbers of any such array with `.. | numbers`.
For example, the numbers of the 2nd array are:

~~~
[[[0],1,2,[0]],3,4,[[0],1,2,[0]]] | .. | numbers -->
0 1 2 0 3 4 0 1 2 0
~~~

Putting all this together, we get our solution via:

~~~
def next: [., .[2] + (1,2), .];
[0] | nth(30; recurse(next)) | nth(10244; .. | numbers) -->
2
~~~

This now runs almost instantaneously, and gives us the answer `2`.
Going back to the original puzzle, because `X = 0`, `Y = 1`, `Z = 2`,
the final answer to the puzzle is `Z`.
