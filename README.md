HSL
===

An experimental branch of [HSL](https://github.com/ssadler/hsl), a Haskell text processor for the command-line.

There are many other similar tools: [eddie](https://code.google.com/p/eddie/), [HSProcess](https://github.com/melrief/HSProcess/), and of course the original [HSL](https://github.com/ssadler/hsl).

This experimental branch differs from those other tools by relying on type inference to guess how you need the input to be parsed. With `eddie` and `HSProcess`, command-line flags are used to tell the tool whether you want your Haskell expression to be applied to each line or to the entire stream. In contrast, my version uses the type of your Haskell expression to decide how to apply it:

    -- reverse :: [a] -> [a],
    -- so we split the input into lines
    > seq 3 | hsl reverse
    3
    2
    1

    -- swap :: (a, b) -> (b, a),
    -- so we split each line into two fields
    > printf "A\t1\nB\t2" | hsl swap
    1	A
    2	B

    -- [5,4..1] :: [Int],
    -- not a function, so we ignore the input
    > hsl [5,4..1]
    5
    4
    3
    2
    1

More Examples
========

Since each line is a `ByteString` and not a `[Char]`, we can easily infer whether you intend to reverse lines or characters:

    > printf 'Hello\nWorld'  | hsl B.reverse
    olleH
    dlroW

Full Haskell syntax is supported. Go crazy!

    > printf "hello\nworld\n" | hsl 'take 2 . repeat . head . filter (B.isPrefixOf "w")'
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

    > printf '[1,2]\n[3,4]' | hsl 'json (undefined::(Int,Int)) ""'
    1	2
    3	4

    > printf '[1,2]\n[3,4]' | hsl 'json (i,i) ""'
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

