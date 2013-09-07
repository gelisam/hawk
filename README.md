# Hawk

Transform text from the command-line using Haskell expressions. Similar to [awk](http://cm.bell-labs.com/cm/cs/awkbook/index.html), but using Haskell as the text-processing language.


## Examples

The Haskell expression `(!! 1)` takes the second element from a list. Using Hawk to `--map` this expression over all input lines, we can extract the second column of the output of `ps aux`.

```bash
    > ps aux | hawk -m '(!! 1)' | head -n3
    PID
    20509
    188
```

That behaviour was similar to the standard unix tool [`cut`](http://en.m.wikipedia.org/wiki/Cut_%28Unix%29). Many other standard command-line tools can be easily approximated using [other short Haskell expressions](http://www.haskell.org/haskellwiki/Simple_Unix_tools).

By adding custom function definitions to `~/.hawk/prelude.hs`, it is easy to `--apply` much more advanced manipulations to the input:

```bash
    > ps aux | hawk -a 'tree (!!1) (!!2) . tail' | head -n 6
    188
      189
        20509
        20611
      190
        2790
```

For more details, see the [documentation](doc.md).


## Installation

To install the development version, clone this repository and use `cabal install` or `cabal-dev install` to compile Hawk and its dependencies. Cabal installs the binary to `~/.cabal/bin/hawk`, while cabal-dev installs it to `./cabal-dev/bin/hawk`. The first run will create a `~/.hawk/prelude.hs` skeleton from which you can import more modules and implement your custom transformations.
