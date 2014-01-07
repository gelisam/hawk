# Hawk Documentation

```bash
> seq 3 | hawk -a 'L.reverse'
3
2
1
```

Hawk is a command-line tool for manipulating text. Like [awk](http://cm.bell-labs.com/cm/cs/awkbook/index.html), it supports an unbounded number of text transformations, implemented by the user via an expression language. In the case of Hawk, the expression language is [Haskell](http://www.haskell.org/), whose basics are assumed to be familar to the reader.


## Overview: one-liners and beyond

Command-line tools such as awk are best for one-liners, while compiled languages such as Haskell are intended for larger projects, spanning several modules and thousands of lines of code. Hawk fills the gap between those two extremes.

```bash
> seq 10 | hawk -ad 'L.takeWhile (/="7") . L.dropWhile (/="3")'
3
4
5
6
```

Hawk lets you write Haskell one-liners on the command-line. When your one-liner grows too large to fit on a single line, move some of it to `~/.hawk/prelude.hs`, a Haskell module which hosts your custom helper functions.

```bash
> echo 'between x y = L.takeWhile (/=y) . L.dropWhile (/=x)' >> ~/.hawk/prelude.hs
> seq 10 | hawk -ad 'between "3" "7"'
3
4
5
6
```

The above example illustrates how to create a new helper function `between`, and how to use it from Hawk. Whenever we use such helper functions, we will link to an example prelude containing the required definitions, like this:

([prelude.hs](between/prelude.hs))

If your prelude becomes too crowded, it might be a sign that your task is too big for Hawk. Simply move your prelude file to the folder of your choice, as the first module of a new Haskell project.


## When should I use Hawk?

At the moment, Hawk is best at filtering and reorganizing collections of strings organized in a table or as a sequence of lines. Many standard command-line tools can be easily approximated using [very short Haskell expressions](http://www.haskell.org/haskellwiki/Simple_Unix_tools); for example, here is a simplified version of [`head`](http://en.wikipedia.org/wiki/Head_%28Unix%29).

```bash
> seq 10 | hawk -a 'L.take 3'
1
2
3
```

The original `head`, of course, is shorter and has more options. Hawk is useful when there isn't already a standard unix tool for what you need. For example, there is a standard tool for flipping tables vertically, but there is no tool for flipping them horizontally:

```bash
> printf "1 2 3\n4 5 6\n7 8 9\n" | tac
7 8 9
4 5 6
1 2 3
> printf "1 2 3\n4 5 6\n7 8 9\n" | hawk -m 'L.reverse'
3 2 1
6 5 4
9 8 7
```

Similarly-short expressions could be used to transpose, rotate, flatten, and so on. With a bit more effort, much more involved transformations are also possible. For example, here we use a helper function to parse the input's indentation structure into a tree, and we use a few concatenations to shape this tree into a lisp-style expression.

```bash
> cat example.in
foo
  bar1
  bar2
    baz
  bar3
> hawk -ad 'postorder (\x -> printf "(%s)" . L.intercalate " " . (unpack x:))' example.in
(foo (bar1) (bar2 (baz)) (bar3))
```
([prelude.hs](postorder/prelude.hs))

Numerical operations are possible, but a bit inconvenient because Hawk exposes its input as a collection of ByteString values, which need to be [unpacked](http://hackage.haskell.org/packages/archive/bytestring/latest/doc/html/Data-ByteString-Lazy-Char8.html#v:unpack) and [read](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:read) before being manipulated.

```bash
> seq 3 | hawk -ad 'sum . L.map (read . B.unpack)'
6
```

It is possible to populate `~/.hawk/prelude.hs` with some utility functions
for easy conversion from ByteString.

```bash
> seq 3 | hawk -ad 'sum . L.map toInt'
6
```
([prelude.hs](conversions/prelude.hs))

## Modes

Without any flag, Hawk simply evaluates the given Haskell expressions.

```bash
> hawk '2 ^ 100'
1267650600228229401496703205376
```

In order to transform the input, it is necessary to use one of Hawk's two other modes, `--apply` and `--map`.

```bash
> printf "1 2 3\n4 5 6\n7 8 9\n" | hawk -a 'L.take 2'
1 2 3
4 5 6
```

With the first mode, we `--apply` the user expression to the entire input. With the second, the expression is applied to each line, using `map`.

```bash
> printf "1 2 3\n4 5 6\n7 8 9\n" | hawk -m 'L.take 2'
1 2
4 5
7 8
```

In a future version of Hawk, type inference will be used to infer the appropriate mode. See the `magic` branch for a prototype.


## Input Formats

By default, Hawk reads and writes text in a tabular format typical of the
command-line: each line is seen as whitespace-separated columns. The lines
separator is the newline character. To better understand how Hawk sees the
input, just call the [show](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:show) function:

```bash
> printf "1 2 3\n4 5 6\n7 8 9\n" | hawk -a 'show'
[["1","2","3"],["4","5","6"],["7","8","9"]]
```

So internally Hawk represents the input as a table, that is a list of lists
of ByteString. The function that the user provides works on that datatype.

```bash
> printf "1 2 3\n4 5 6\n7 8 9\n" | hawk -a 'id :: [[ByteString]] -> [[ByteString]]'
1 2 3
4 5 6
7 8 9
```

It is possible to change the `--words-delimiter` for tables using `-d`.

```bash
> printf "1\t2\t3\n4\t5\t6\n7\t8\t9\n" | hawk -a -d'\t' 'id'
1	2	3
4	5	6
7	8	9
```

```bash
> printf "1,2,3\n4,5,6\n7,8,9\n" | hawk -ad, id
1,2,3
4,5,6
7,8,9
```

It is also possible to change the `--lines-delimiter` using `-D`.

```bash
> printf "x1*y1*z1 + x2*y2*z2" | hawk -D' + ' -d'*' -a 'L.transpose'
x1*x2 + y1*y2 + z1*z2
```

Of course, tables are not the only common command-line format. If you don't
need lines to be separated into words, simply pass an empty `--words-delimiter`.

```bash
> seq 3 | hawk -d -a 'show :: [ByteString] -> String'
["1","2","3"]
```

Finally, to work directly on the ByteString just pass an empty `--lines-delimiter`.

```bash
> seq 3 | hawk -d -D -a 'show :: ByteString -> String'
"1\n2\n3\n"
```


## Output Formats

Hawk writes text in a tabular format typical of the command-line.

The type of the user function must be compatible with this tabular format.
Recall that, in Hawk, tabular means list of lists of ByteString where the first
list is the list of lines, the second list is the list of words for each line
and the ByteString is the content of a single cell.

For the cell content type, any type that is instance of
[Show](http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#t:Show)
is valid.

For the list of words, any type that is instance of
[Row](https://raw.github.com/gelisam/hawk/master/src/System/Console/Hawk/Representable.hs) can be used. All standard data types are instance of this class.

For the list of lines, any type that is instance of
[Rows](https://raw.github.com/gelisam/hawk/master/src/System/Console/Hawk/Representable.hs) can be used. All standard data types are instance of this class.

Let's do some examples to better understand how this works.

```bash
> hawk '[[B.pack "1",B.pack "2"], [B.pack "3",B.pack "4"]]'
1 2
3 4
```

The given expression creates a list of lists of ByteString, that is exactly the
type that Hawk uses. The output is created using the default delimiter for words,
that is space, and lines, that is newline. Note that the whole expression is
what we called list of lines, while the expressions `[B.pack "1",B.pack "2"]`
and `[B.pack "3",B.pack "4"]` are list of words and `B.pack "1"`, `B.pack "2"`
, `B.pack "3"` and `B.pack "4"` are the cell values.

Changing the type of the cell values is still valid if the given type is instance
of `Show`. For examples, we can use String or Float instead of ByteString.

```bash
> hawk '[["1","2"], ["3","4"]]'     
1 2
3 4
```

```bash
> hawk '[[1,2], [3,4]] :: [[Float]]'
1.0 2.0
3.0 4.0
```

In the examples above we used list as type for the list of words and for the
list of lines but what happens when we use other types? Each type has its own
representation but usually if the type is a container then it is represented
like a list (of words or lines) else it is represented as itself.

```bash
> hawk '1 :: Double'
1.0
```

```bash
> hawk '(True,False)'
True
False
```

```bash
> hawk '[(1,2),(3,4)] :: [(Int,Float)]'
1 2.0
3 4.0
```

The output delimiters for words and lines are by default the same delimiters
used for the input. They can be changed using `--output-words-delim` and
`--output-lines-delim`.

```bash
> hawk -O' or ' '(True,False)'                  
True or False
```

```bash
> hawk -o'\t' '[(1,2),(3,4.0)] :: [(Int,Float)]'
1	2.0
3	4.0
```

Combining the input delimiters options with the output delimiters options can
be used to change the format of the given input.

```bash
> > echo '1 2 3\n4 5 6'  | hawk -m -d' ' -o'*' -D'\n' -O'+' 'id' 
1*2*3+4*5*6
```

## User Prelude

The file `~/.hawk/prelude.hs` contains the Hawk context and can be used to
customize Hawk. If the directory `~/.hawk` doesn't
exist, Hawk creates it the first time is run with a default `prelude.hs` file

```haskell
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
import Prelude
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
```

Using this file it is possible to use all the functions of `Prelude` in Hawk.
It is also possible to use `ByteString` and `List` functions using the two
qualifiers:

```bash
> seq 3 | hawk -a 'L.length'
3
```

The `prelude.hs` file can be seen as a regular Haskell file with some limitations:

- only the `LANGUAGE` [pragma](http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/pragmas.html) is supported
- [import modules](http://www.haskell.org/haskellwiki/Import) is restricted to
importing them totally: there is no way to import only a part of a module, so
something like `import Prelude (try)` won't import only try from the `Prelude`
module. The support is planned for future releases
