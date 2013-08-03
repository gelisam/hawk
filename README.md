HSL
===

Haskell command line text processor

Tool to quickly compile and run haskell streaming text processors on the
command line.

Features a powerful way of working quickly with JSON.

Examples
========

    > echo '!LSH olleH'  | hsl 'B.reverse . head'
    Hello HSL!

    > printf "hello\nworld\n" | hsl 'take 2 . repeat . filter (B.isPrefixOf "w")'
    world
    world

    > cat src/HSL/* | hsl 'take 5 . reverse . hist . B.words . B.concat'
    41	->
    37	=
    20	where
    17	tb
    17	Scalar

    > echo '{"a": 1}' | hsl 'json tI "a"'
    1

    > echo '[1,2]' | hsl 'json [tI] ""'
    1
    2

    > echo '{"a": "Hello", "b": ["", "World"]}' | hsl 'json2 (tS, tS) "a" "b 1"'
    Hello	World


Installing
==========

    > git clone 'https://github.com/ssadler/hsl.git' && cd hsl
    > # Install dependencies... You probably already have them (?)
    > echo 'alias hsl="'`pwd`'/hsl"' >> ~/.bashrc


JSON parsing
============

The convenience function `json` produces a series of values from a series of
input JSONs:

`json :: FromJSON a => a -> Text -> [ByteString] -> [a]`

`a` above is not used at runtime; it is a hint so that the compiler knows what
type we want back. `tI` (Int), `tS` (ByteString) and `tF` (Float) are provided for this
purpose. The following are equivalent:

    echo '[[[1,2],[3,4]]]' | hsl 'json (undefined::[(Int,Int)]) "0"'
    1	2
    3	4

    echo '[[[1,2],[3,4]]]' | hsl 'json [(tI,tI)] "0"'
    1	2
    3	4

The second argument defines the location of the value we want to pull out;
space separated, so integers are array indices and everything else is an object key.
If none is provided then the top level object is returned. See examples above.

See also: [FromJSON](http://hackage.haskell.org/packages/archive/aeson/0.6.1.0/doc/html/Data-Aeson.html#t:FromJSON)

`json2` and `json3` are also provided, to allow extraction of more values, ie:

`json2 :: (FromJSON a, FromJSON b) => (a,b) -> Text -> Text -> [ByteString] -> [(a,b)]`

