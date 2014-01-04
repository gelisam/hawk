# Hawk

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

The `-d` option tells Hawk to use `:` as word delimiters, causing the first line to be interpreted as `["root", "x", "0", "0", "root", "/root", "/bin/bash"]`.
The `-m` tells Hawk to map a function over each line of the input. In this case, the function `head` extracts the first word of the line, which happens to be the username.

We could of course have achieved identical results by using awk instead of Hawk:

```bash
> cat /etc/passwd | awk -F: '{print $1}'
root
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
