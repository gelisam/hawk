# Hawk

Transform text from the command-line using Haskell expressions. Similar to [awk](http://cm.bell-labs.com/cm/cs/awkbook/index.html), but using Haskell as the text-processing language.

## Examples

In [Unix](http://en.wikipedia.org/wiki/Unix) the file `/etc/passwd` is used to
keep track of every registered user in the system. Each entry in the file
contains colon-separated informations about a single user, like:

```
root:x:0:0:root:/root:/bin/bash
```

The first field is the username. To extract every user registered in the system you can do:

```bash
> cat /etc/passwd | hawk -d: -m 'head'
```

The `-d` option tells Hawk to use ':' as word delimiters and `-m head` will
map [head](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html#v:head) over each line extracting the user. The same result can be achieved
using awk:

```bash
> cat /etc/passwd | awk -F: '{print $1}'
```

While Hawk and awk have similar use cases, the philosophy behind the two is very
different. Instead of creating an ad-hoc language like awk does, Hawk uses
Haskell and can easily use all the libraries available for it.
There are many standard command-line tools that can be easily approximated using
[short Haskell expressions](http://www.haskell.org/haskellwiki/Simple_Unix_tools).
The Hawk runtime environment is made to be fully customizable: there is
a [prelude file](https://github.com/gelisam/hawk/tree/master/doc#user-prelude)
which contains imported modules, language extensions and
user defined functions. This file can be populated with functions to use inside
Hawk. For instance, we could add a `takeNFrom` function that gets elements in an
interval of the input:

```bash
> echo 'takeNFrom i c = take c . drop i' >> ~/.hawk/prelude.hs
> seq 0 100 | hawk -a 'takeNFrom 10 3'
10
11
12
```


For more details, see the [documentation](doc/README.md).

## Installation

To install the development version, clone this repository and use `cabal
install` or `cabal-dev install` to compile Hawk and its dependencies. Cabal
installs the binary to `~/.cabal/bin/hawk`, while cabal-dev installs it to
`./cabal-dev/bin/hawk`. The first run will create a default configuration into
`~/.hawk/prelude.hs` if it doesn't exist.
