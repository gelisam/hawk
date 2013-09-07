# Hawk Documentation

```bash
> seq 3 | hawk -a 'L.reverse'
3
2
1
```

Hawk is a command-line tool for manipulating text. Like [awk](http://cm.bell-labs.com/cm/cs/awkbook/index.html), the command supports an open-ended set of text transformations expressed via an expression language. In the case of Hawk, the expression language is [Haskell](http://www.haskell.org/), whose basics are assumed to be familar to the reader.


## Overview

Command-line tools such as [awk](http://en.wikipedia.org/wiki/AWK) are best for one-liners, while compiled languages such as [Haskell](http://www.haskell.org) are intended for larger projects, spanning several modules and thousands of lines of code.

```bash
> seq 10 | hawk -ad 'L.takeWhile (/="7") . L.dropWhile (/="3")'
3
4
5
6
```

Hawk lets you write Haskell one-liners on the command-line, and continues to support you as your one-liner grows into ten, one hundred lines. When your command-line expression becomes too large, move some of it to `~/.hawk/prelude.hs`, a Haskell module which hosts your custom helper functions. Once that file becomes too crowded, simply move that file to the folder of your choice, as the first module of a new Haskell project.

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


## When should I use Hawk?

At the moment, Hawk is best at filtering and reorganizing collections of "opaque" strings organized in a table or as a sequence of lines. A simple example would be to emulate [head](http://en.wikipedia.org/wiki/Head_%28Unix%29), while a more advanced example would be to parse a python-style indentation-based syntax and to convert it to a lisp-style representation of that tree structure.

```bash
> seq 10 | hawk -a 'L.take 3'
1
2
3
```

```bash
> cat example.in
foo
  bar1
  bar2
    baz
  bar3
> $ hawk -ad 'postorder (\x xs -> "("<>x<>(B.concat(L.map(" "<>)xs))<>")")' example.in
(foo (bar1) (bar2 (baz)) (bar3))
```

Numerical operations are possible, but a bit inconvenient because we expose the input as a collection of ByteString values, which need to be [unpacked](http://hackage.haskell.org/packages/archive/bytestring/latest/doc/html/Data-ByteString-Lazy-Char8.html#v:unpack) and [read](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:read) before being manipulated.

```bash
> seq 3 | hawk -ad 'sum . L.map (read . B.unpack)'
6
```


## Hawk Modes

(todo)

## Input Formats

(todo)

## Output Formats

(todo)

## User Prelude

(todo)
