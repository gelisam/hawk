HSL
===

Haskell command line text processor

Tool to quickly compile and run haskell streaming text processors on the
command line.

Features a powerful way of working quickly with JSON.

Examples
========

Poor man's [`tac`](http://www.gnu.org/software/coreutils/manual/html_node/tac-invocation.html):

    > seq 3 | hsl reverse
    3
    2
    1

The input is a `[ByteString]`, an array of lines.

    > echo '!LSH olleH'  | hsl 'map B.reverse'
    Hello HSL!

The output is also an array of lines... but it could also be an array of tuples, in which case the tuples are displayed as tab-separated columns.

    > printf 'helloWorld\nnicePlanet\n' | hsl "map (break isUpper)"
    hello	World
    nice	Planet

Or a single value...

    > printf 'literary\ncheeseburger' | hsl 'maximumBy (comparing B.length) . concatMap B.words'
    cheeseburger

Full Haskell syntax is supported. Go crazy!

    > printf "hello\nworld\n" | hsl 'take 2 . repeat . filter (B.isPrefixOf "w")'
    world
    world

A few builtins, such as `hist`, are provided for convenience. 

    > cat src/HSL/* | hsl 'take 5 . reverse . hist . B.words . B.concat'
    41	->
    37	=
    20	where
    17	tb
    17	Scalar

You can add your own builtins to `src/HSL/Stdlib.hs`. Send us yours in a pull request!


JSON
====

One of HSL's most useful builtin is `json`. It's like `cut`, but for JSON!

More concretely, if you have a file with one JSON expression per line, `json` lets you easily specify a path from which to extract elements.

    > cat cat_memes.json 
    {"name": "nyan", "year": 2011}
    {"name": "longcat", "year": 2007}
    {"name": "ceiling cat", "year": 2006}
    {"name": "invisible bike cat", "year": 2008}
    > cat cat_memes.json | hsl 'sort . json i "year"'
    2006
    2007
    2008
    2011

The `i` argument specifies the type of the result. `i` (Int), `s` (ByteString)
and `f` (Float) are provided for this purpose. The following are equivalent:

    echo '[[1,2],[3,4]]' | hsl 'json (undefined::[(Int,Int)]) ""'
    1	2
    3	4

    echo '[[1,2],[3,4]]' | hsl 'json [(i,i)] ""'
    1	2
    3	4

The empty string in the above example is an empty path, referring to the entire JSON expression. The general syntax for paths is illustrated in the following example:

    > echo '{"a":{"b":["Hello","World"]}}' | hsl 'json s "a b 0"'
    Hello

If you need to extract more than one value per line, use `json2` or `json3`.

    > echo '{"born":1938, "age":75}' | hsl 'map (uncurry (+)) . json2 (i,i) "born" "age"'
    2013


Installing
==========

    > git clone 'https://github.com/ssadler/hsl.git' && cd hsl
    > # Install dependencies... You probably already have them (?)
    > echo 'alias hsl="'`pwd`'/hsl"' >> ~/.bashrc

