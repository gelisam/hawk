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


## Flags

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

By default, Hawk reads and writes text in a tabular format typical of the command-line: whitespace-separated columns. Hawk represents such a table as a list of lists.

```bash
> printf "1 2 3\n4 5 6\n7 8 9\n" | hawk -a 'id :: [[ByteString]] -> [[ByteString]]'
1 2 3
4 5 6
7 8 9
```

Other common `--delimiter`s for tables include tabs and commas.

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

It is also possible to change the line-delimiter.

```bash
> printf "x1*y1*z1 + x2*y2*z2" | hawk -D' + ' -d'*' -a 'L.transpose'
x1*x2 + y1*y2 + z1*z2
```

Of course, tables are not the only common command-line format. If you don't need lines to be separated into words, simply pass an empty `--delimiter`.

```bash
> seq 3 | hawk -d -a 'id :: [ByteString] -> [ByteString]'
1
2
3
```

(todo: input, bytestream input)


## Output Formats

(todo)

## User Prelude

(todo)
