# Hawk

Transform text from the command-line using Haskell expressions.


## About

Command-line tools such as [awk](http://en.wikipedia.org/wiki/AWK) are best for one-liners, while compiled languages such as [Haskell](http://www.haskell.org) are intended for larger projects, spanning several modules and thousands of lines of code.

Hawk lets you write Haskell one-liners on the command-line, and continues to support you as your one-liner grows into ten, one hundred lines. When your command-line expression becomes too large, move some of it to `~/.hawk/prelude.hs`, a Haskell module which hosts your custom helper functions. Once that file becomes too crowded, simply move that file to the folder of your choice, as the first module of a new Haskell project.


## Haskell Expressions

The simplest way to use Hawk is to evaluate Haskell expressions.

    > hawk '2 ^ 100'
    1267650600228229401496703205376

So far, so [`bc`](http://en.wikipedia.org/wiki/Bc_%28programming_language%29). Or python, or any other language with builtin arbitrary-precision arithmetic. But does any of those languages support lazy lists? Bash and Haskell both do!

    > hawk '[2 ^ i | i <- [0..]]' | grep '^1' | head
    1
    16
    128
    1024
    16384
    131072
    1048576
    16777216
    134217728
    1073741824

To make it easier for other commands to process Hawk's output, we display one element per line instead of using Haskell's `[1,2,4,...]` syntax. For the same reason, we display lists of lists as tables.

    > hawk 'unfoldr (Just . splitAt 3) [1..]' | head -n 3
    1 2 3
    4 5 6
    7 8 9

The `unfoldr` function is not part of the Prelude, so in order to run the above, we need to add `import Data.List` to our `~/.hawk/prelude.hs`. While we're modifying the configuration file, let's also give a name to the above expression.

    > cat ~/.hawk/prelude.hs
    {-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
    import Data.List
    
    splitEvery n = unfoldr (Just . splitAt n)
    box9 = take 3 $ splitEvery 3 [1..]

The `ExtendedDefaultRules` language pragma is important because without it, Haskell will often complain that our numeric literals have ambiguous types.

    > hawk 'take 2 box9'
    1 2 3
    4 5 6


## Manipulating Text

Hawk can `--apply` a Haskell expression to its input, represented as a list of lists.

    > hawk 'take 2 box9' | hawk -a transpose
    1 4
    2 5
    3 6

Different tools use different characters to separate columns, use `-d` to tell Hawk which one you need.

    $ hawk -d',' box9
    1,2,3
    4,5,6
    7,8,9

As a special case, using `-d` without specifying a delimiter tells Hawk not to split lines into words. The input is then a `[ByteString]` instead of a `[[ByteString]]`. Similarly, `-D` will tell Hawk not to split the input into lines, so the input will be a `ByteString`. The next version of Hawk will use type inference to determine which of the three input modes is needed.

    > cat haskell-awk.cabal | hawk -d -a 'takeWhile (/= "") . dropWhile (/= "Source-Repository head")'
    Source-Repository head
        type: git
        location: https://github.com/gelisam/hawk

Oh, that's a useful pattern! Let's add it to our `~/.hawk/prelude.hs`.

    > tail -n4 ~/.hawk/prelude.hs
    between :: Eq a => a -> a -> [a] -> [a]
    between start end = takeWhile (/= end)
                      . dropWhile (/= start)
    section header = between header ""

As usual in Haskell, type signatures are optional.

    > cat haskell-awk.cabal | hawk -ad 'section "Source-Repository head"'
    Source-Repository head
        type: git
        location: https://github.com/gelisam/hawk


## Map mode

When operating on a stream of text, it is very common to apply the same transformation to each line. In Haskell, this is easily done using `map`.

    > hawk box9 | hawk -a 'map ("0":)'
    0 1 2 3
    0 4 5 6
    0 7 8 9

Since mapping is so common, Hawk offers `--map` mode as a shortcut.

    > hawk box9 | hawk -m '(!!1)'
    2
    5
    8


## Installation

To install the development version, clone this repository and use `cabal install` or `cabal-dev install` to compile Hawk and its dependencies. Cabal installs the binary to `~/.cabal/bin/hawk`, while cabal-dev installs it to `./cabal-dev/bin/hawk`.
