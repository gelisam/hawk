# Hawk [![Hackage](https://img.shields.io/hackage/v/haskell-awk.svg)](https://hackage.haskell.org/package/haskell-awk) [![Build Status](https://github.com/gelisam/hawk/workflows/CI/badge.svg)](https://github.com/gelisam/hawk/actions)

Transform text from the command-line using Haskell expressions. Similar to [awk](http://cm.bell-labs.com/cm/cs/awkbook/index.html), but using Haskell as the text-processing language.

## Examples

In [Unix](http://en.wikipedia.org/wiki/Unix) the file `/etc/passwd` is used to
keep track of every registered user in the system. Each entry in the file
contains information about a single user, using a simple colon-separated format.
For example:

```
root:x:0:0:root:/root:/bin/bash
```

The first field is the username. We can use Hawk to list all usernames as follows:

```bash
> cat /etc/passwd | hawk -d: -m 'head'
root
```

The `-d` option tells Hawk to use `:` as field delimiters, causing the first line to be interpreted as `["root", "x", "0", "0", "root", "/root", "/bin/bash"]`.
The `-m` tells Hawk to map a function over each line of the input. In this case, the function `head` extracts the first field of the line, which happens to be the username.

We could of course have achieved identical results by using awk instead of Hawk:

```bash
> cat /etc/passwd | awk -F: '{print $1}'
root
```

While Hawk and awk have similar use cases, the philosophy behind the two is very
different. Awk uses a specialized language designed to concisely express many text transformations,
while Hawk uses the general-purpose language Haskell, which is also known for being concise, among other things.
There are many standard command-line tools that can be easily approximated using
[short Haskell expressions](https://wiki.haskell.org/Simple_Unix_tools).

Another important difference is that while awk one-liners are self-contained, Hawk encourages the use of libraries and user-defined functions. By adding function definitions, module imports and language pragmas to Hawk's user-configurable [prelude file](https://github.com/gelisam/hawk/tree/main/doc#user-prelude), those functions, libraries and language extensions become available to Hawk one-liners.
For instance, we could add a `takeLast` function extracting the last `n` elements from a list, and use it to (inefficiently) approximate `tail`:

```bash
> echo 'takeLast n = reverse . take n . reverse' >> ~/.hawk/prelude.hs
> seq 0 100 | hawk -a 'takeLast 3'
98
99
100
```

For more details, see the
[presentation](http://melrief.github.io/HawkPresentation/#/) and the
[documentation](https://github.com/gelisam/hawk/tree/main/doc/README.md).

## Installation

To install hawk, either run `stack install haskell-awk` or `cabal install haskell-awk`.

You should be ready to use Hawk:

```bash
> hawk '[1..3]'
1
2
3
```

The first run will create a default configuration file into
`~/.hawk/prelude.hs` if it doesn't exist.

## Alternatives

You can also try [hwk](https://hackage.haskell.org/package/hwk) which is fairly similar to this project though simpler.
