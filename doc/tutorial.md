# Hawk Tutorial

This tutorial aims to provide examples of usage of Hawk.
It is a work in progress and subject to changes over time. It is divided in
three sections: Introduction, Basic Examples and Advanced Examples.

The introduction briefly explains
what are input and outputs of Hawk and what are the apply and the map modes.

The Basic Example section contains examples that don't require to change
`~/.hawk/prelude.hs`.

The Advanced Example section contains more advanced examples that require to
change `~/.hawk/prelude.hs`


## Introduction

Before starting with real examples it is important to describe the input and the
output of Hawk because they can be misleading. There are three steps involved
in a Hawk run:

1. convert the input into a tabular format. By default Hawk considers the input
as a list of lines where each line is a list of words.
- apply the user expression to this formatted input
- print to output a command-line representation of the user expression output

For instance, to redirect the standard input to the standard output we
can apply the `id` function:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -a 'id'
1 2 3
4 5 6
7 8 9
```

By default Hawk works on a list of lists of strings. But instead of showing this
list of lines as the `show` function would do, Hawk shows every element of that
list as a line. This representation of the output makes Hawk easy to use in
the command-line but has the disadvantage to hide the structure of the user
output.

It is possible to see the `show` representation of the output by applying the
`show` function:


```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -a 'show'
[["1","2","3"],["4","5","6"],["7","8","9"]]
```

Hawk doesn't process strings and shows them as they are. Therefore the output
of `show`, that is a string representation of a given value, is shown as itself.
The difference with applying `id` is that `id` returns a
list and Hawk transforms lists into lines. Every time the output of Hawk isn't
clear, it is recommended to apply the `show` function to get a visual hint.

It is possible to change the lines delimiter and the words delimiter by using,
respectively, `-D` and `-d`:

```bash
> echo '1,2,3\n4,5,6\n7,8,9' | hawk -d, -a 'show'
[["1","2","3"],["4","5","6"],["7","8","9"]]
> echo -n '1 2 3|4 5 6|7 8 9' | hawk -D'|' -a 'show'
[["1","2","3"],["4","5","6"],["7","8","9"]]
> echo '1,2,3|4,5,6|7,8,9' | hawk -D'|' -d, -a 'show'
[["1","2","3"],["4","5","6"],["7","8","9"]]
```

A special case is when one of the two delimiters is set to empty string. If
only the word delimiter is set to empty string then the input is still split
in lines but lines are not split in words:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -d -a 'show'
["1 2 3","4 5 6","7 8 9"]
```

If also the lines delimiter is set to empty string then the input to the user
expression is not formatted at all:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -D -d -a 'show'
"1 2 3\n4 5 6\n7 8 9\n"
```

The input of Hawk is, by default, a list of lines. Therefore we can apply
list functions to the input. Let's see some examples.

To take the first 2 lines we can use the `take` function:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -a 'L.take 2'
1 2 3
4 5 6
```

The `L` of `L.take` is the module alias given to `Data.List`. It can be found
in `~/.hawk/prelude.hs`.
Using the `show` function we can see that the result of applying `L.take 2`
is exactly what we want:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -a 'show . L.take 2'
[["1","2","3"],["4","5","6"]]
```

To get the second word of each line we can map the list indexing function `!!` to
each line:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -a 'L.map (!! 1)'
2
5
8
```

Mapping a function to each line is so useful that Hawk has a mode `-m`
that is equivalent to applying the map function. Hence, the previous
example can be written:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -m '!! 1'
2
5
8
```

It is possible to use `show` in map mode to get visual hints on the output:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -m 'show . (!! 1)'
"2"
"5"
"8"
```

Mapping the `show` function has the effect to show only the underlying structure
of each line. In this case, each line is transformed into a string representing
the word extracted.

To get the last two words of each line we can `drop` the first word:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -m 'L.drop 1'
2 3
5 6
8 9
```

To reverse the input lines we can apply `reverse`:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -a 'L.reverse'
7 8 9
4 5 6
1 2 3
```

To reverse the words of each line we can map `reverse`:

```bash
> echo '1 2 3\n4 5 6\n7 8 9' | hawk -m 'L.reverse'
3 2 1
6 5 4
9 8 7
```


##Basic Examples

### Select one field and input delimiter

Get all the user in a UNIX system:

```bash
> cat /etc/passwd | hawk -d: -m 'L.head'
root
daemon
bin
sys
```

The option `-d` changes the delimiter from spaces to colon.

### Select multiple fields

Get all the usernames with their userid, that are respectively the first and
third field:

```bash
> cat /etc/passwd | hawk -d: -m '\l -> (l !! 0, l !! 2)'
root:0
daemon:1
bin:2
sys:3
```

There are two things to see in this example. The first is that the output type
should not be necessarily a list of lines. Hawk can show many Haskell types
as list of lines. In this case the output format is a list of tuples:

```bash
> cat /etc/passwd | hawk -d: -m 'show . \l -> (l !! 0, l !! 2)'
("root","0")
("daemon","1")
("bin","2")
("sys","3")
```

and Hawk represents tuples in the same way it represents lists.
The second thing is that the output delimiter is set by default equal
to the input delimiter,
so in this case colon is used.

### Output delimiter

We can change the output format by overriding
the output delimiter with `-o` followed by the separator:

```bash
> cat /etc/passwd | hawk -d: -o'\t' -m '\l -> (l !! 0, l !! 2)' 
root	0
daemon	1
bin	2
sys	3
```

### Sort lines

We can sort by applying the sort function:

```bash
> cat /etc/passwd | hawk -d: -o'\t' -a 'L.sort'
bin	x	2	2	bin	/bin	/bin/sh
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
root	x	0	0	root	/root	/bin/bash
sys	x	3	3	sys	/dev	/bin/sh
```

### Filter lines

To filter by certain usernames, like "root" and "bin", we can apply the filter function:

```bash
> cat /etc/passwd | hawk -d: -o'\t' -a 'L.filter ((`L.elem` ["root","bin"]) . L.head)' 
root	x	0	0	root	/root	/bin/bash
bin	x	2	2	bin	/bin	/bin/sh
```

We can combine filtering, selection and sorting in a single expression using
function composition:

```bash
> cat /etc/passwd | hawk -d: -o'\t' -a 'L.sort . L.map (\l -> (l !! 0, l !! 2)) . L.filter ((`L.elem` ["root","bin"]) . L.head)'
bin	2
root	0
```

### Group lines

We would like to count how many users use each shell. The shell is the last
field in `/etc/passwd`:

```bash
> cat /etc/passwd | hawk -d: -o'\t' -a 'L.map (\l -> (head l,L.length l)) . L.group . L.sort . L.map L.last'
/bin/bash	2
/bin/false	19
/bin/sh	19
```

### Label output fields

Hawk fields are
[ByteString](http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Char8.html)
and for this reason we can't use String functions. For example, to prepend
a label to each field, we cannot use
[List append ++] (
http://hackage.haskell.org/package/base/docs/Data-List.html#v:-43--43-).
We can prepend a label to each field by using the [ByteString append](
http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Char8.html#v:append)
function:

```bash
> cat /etc/passwd | hawk -d: -o'\t' -m '\l -> ("username:" `B.append` (l !! 0), "uid:" `B.append` (l !! 2))'
username:root	uid:0
username:daemon	uid:1
username:bin	uid:2
username:sys	uid:3
```

The `B` of `B.append` is the module alias given to `Data.ByteString.Lazy.Char8`.
It can be found in `~/.hawk/prelude.hs`.


## Advanced Examples


### Highest userid

We would like to extract the username of the user with the highest userid. For
instance, on my `/etc/passwd` the user `nobody` has the highest uid, that is
65534.

An explanation of how to do this in Haskell is:

- for each line
  - select the first and the third fields
  - convert the third field to int
- select the line with the highest third field
- output only the first field

A first attempt can be something like:

```bash
> cat /etc/passwd | hawk -d: -a \
'fst . L.maximumBy (\(u1,id1) (u2,id2) -> id1 `compare` id2) . L.map (\l -> (l !! 0, (read . B.unpack $ l !! 2)::Int))'
nobody
```

This can be decompose in:

- `L.map (\l -> (l !! 0, (read . B.unpack $ l !! 2)::Int)` is used to extract
the first and the third field and convert the third field to integer. To do
the conversion we use `read . B.unpack` and we set the type to `Int` with `::Int`
- `L.maximumBy (\(u1,id1) (u2,id2) -> id1 `compare` id2) ` compares the ids of
each user
- `fst` extracts the first field of the highest user

This code is both long and ugly. We can make it cleaner by adding some utilities
in `~/.hawk/prelude.hs`. First of all we need a way to convert `ByteString` to
`Int` like:

```haskell
toInt :: B.ByteString -> Int
toInt = read . B.unpack
```

Then we import the combinator
[on](http://hackage.haskell.org/package/base/docs/Data-Function.html#v:on) to
remove boilerplate from the comparison. The modified prelude is:

```haskell
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
import Prelude
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import Data.Function (on)

toInt :: B.ByteString -> Int
toInt = read . B.unpack
```
We can now write our highest userid expression like:

```bash
> cat /etc/passwd | hawk -d: -a 'L.head . L.maximumBy (compare `on` (toInt . (!! 2)))' 
nobody
```

The important part of this expression is the comparison that can be read as
"compare two lines on their third field treated as Int".

This example show how easy and useful is configure the runtime with custom
functions and imports. When something cannot be easily expressed with the
default prelude, it is always possible to customize it.


### For each shell extract the users list

We want to extract the list of users that use a certain shell and we want to
print, for each shell, a line that starts with the shell name followed by a colon
and the list of users separated by spaces. For instance, for `bash` the line
on my computer would be `/bin/bash: mario root`.

The algorithm that we are going to use is:

- for each line extract the last (shell) and the first (username) fields
- sort and group by shell
- format the output

We populate `~/.hawk/prelude.hs` with:

```haskell
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
import Control.Arrow ((&&&),second)
import Prelude
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Data.Map as M
import Data.Function (on)


groupedWith :: (Ord k) => (a -> k) -> (a -> b) -> [a] -> [(k,[b])]
groupedWith toKey toVal = L.map toTuple
                        . L.groupBy ((==) `on` fst)
                        . L.sortBy (compare `on` fst)
                        . L.map (toKey &&& toVal)
 where toTuple (ls@(l:_)) = (fst l,L.map snd ls)
```

and then we use `groupedWith` in our expression:

```bash
> cat /etc/passwd | hawk -d: -o': ' -a 'L.map (second B.unwords) . groupedWith fst snd . L.map (L.last &&& L.head)'
/bin/bash: mario root 
/bin/sh: sys bin daemon
```

In the used expression, `L.map (L.last &&& L.head)` is used to extract the last and
the first fields, `groupedWith fst snd` is used to group by last field and
`L.map (second B.unwords)` is used to format all the users as expected.
