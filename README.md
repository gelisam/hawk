# Hawk

Command-line lazy text processor using Haskell.
Like [awk](http://cm.bell-labs.com/cm/cs/awkbook/index.html) but functional.

## Examples

Evaluate an expression:

```bash
> hawk '2 ^ 100'
1267650600228229401496703205376
```

Apply an expression to stdin:

```bash
> seq 1 4 | hawk -a reverse
4
3
2
1
```

Map an expression to each line of stdin:

```bash
> echo '1 2\n3 4' | hawk -m '!! 1'
2
4
```

## Configure Hawk

The Hawk configuration is stored in `$HOME/prelude.hs`, a standard
Haskell file in which the user can define the extensions to use, the modules
to import and frequently used functions.

For instance, with the default configuration `hawk 'box 3 3 [1..]'` won't compile.
Adding `box` to `$HOME/prelude.hs` will make it available in `hawk`:

```bash
> hawk 'box 3 3 [1..]'

Won't compile:
    Not in scope: `box'

> cat $HOME/.hawk/prelude.hs
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
import Prelude
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
> echo 'box r c = L.take r . L.unfoldr (Just . splitAt c)' >> $HOME/.hawk/prelude.hs
> hawk 'box 3 3 [1..]'
1 2 3
4 5 6
7 8 9
```

## Text format

Hawk reads and write text in a tabular format typical of the command-line.
Hawk decodes and encodes into list of lines, where a line is a list of words. It
is possible to use `show` to see the "real form" of the data:

```bash
> seq 1 4 | hawk -a id
1
2
3
4
> seq 1 10 | hawk -a show
[["1"],["2"],["3"],["4"]]
```

The default lines delimiter is `\n` and the default words delimiter is space.
They can be changed with the options `-D` and `-d`:

```bash
seq 1 4 | tr '\n' ',' | hawk -D',' -a show
[["1"],["2"],["3"],["4"]]
```

## Installation

To install the development version, clone this repository and use `cabal
install` or `cabal-dev install` to compile Hawk and its dependencies. Cabal
installs the binary to `~/.cabal/bin/hawk`, while cabal-dev installs it to
`./cabal-dev/bin/hawk`. The first run will create a default configuration into
`$HOME/.hawk/prelude.hs` if it doesn't exist.
