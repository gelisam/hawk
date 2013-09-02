# Hawk

Transform text from the command-line using Haskell expressions.


## About

Command-line tools such as [awk](http://en.wikipedia.org/wiki/AWK) are best for one-liners, while compiled languages such as [Haskell](http://www.haskell.org) are intended for larger projects, spanning several modules and thousands of lines of code.

Hawk lets you write Haskell one-liners on the command-line, and continues to support you as your one-liner grows into ten, one hundred lines. When your command-line expression becomes too large, move some of it to `~/.hawk/prelude.hs`, a Haskell module which hosts your custom helper functions. Once that file becomes too crowded, simply move that file to the folder of your choice, as the first module of a new Haskell project.


## Examples

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

The `unfoldr` function is not part of the Prelude, so in order to run the above, we need to add `import Data.List` to our `~/.hawk/prelude.hs`. While we're modifying it, let's also give a name to the above expression.

    > cat ~/.hawk/prelude.hs
    {-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
    import Data.List
    splitEvery n = unfoldr (Just . splitAt n)

The `ExtendedDefaultRules` language pragma is important because without it, Haskell will often complain that our numeric literals have ambiguous types.

    > hawk 'splitEvery 3 [1..]' | head -n 3
    1 2 3
    4 5 6
    7 8 9
