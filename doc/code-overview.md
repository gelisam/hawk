# Hawk Hacking Guide

Here is a tour of the codebase, as of June 2014.

## Overall structure

Hawk's codebase is divided into three folders: [`src`](https://github.com/gelisam/hawk/blob/master/src), [`runtime`](https://github.com/gelisam/hawk/blob/master/runtime), and [`tests`](https://github.com/gelisam/hawk/blob/master/tests).

The `src` folder contains the source for the `hawk` executable. Most of the code is located here.

The `runtime` folder contains the source for a small library which Hawk needs to load at runtime. Since hackage ignores executables, the `runtime` modules are the only ones listed on Hawk's [hackage page](hackage.haskell.org/package/haskell-awk). If you're looking for a module in the `System.Console.Hawk` namespace and you can't find it under `src/System/Console/Hawk`, remember that it might be under `runtime/System/Console/Hawk` instead.

The `tests` folder contains *extra* tests. Most functions already have [doctests](https://github.com/sol/doctest-haskell#readme) above their definitions, but only a small documentation-friendly amount. When the doctests become too long, or if we need a different kind of tests, the code for those tests ends up here.

## Sections

Hawk applies the user expression to stdin and prints the result to stdout. The actual evaluation is handled by the [hint](http://hackage.haskell.org/package/hint) package, which in turn delegates to GHC. So what is left for Hawk to do? Lots of small adjustments, actually.

### Cabal sandboxes

One such adjustment is to accomodate cabal sandboxes. User expressions have access to all the modules imported by the user prelude, assuming those modules have been installed in the same package database as Hawk itself. This allows Hawk and those packages to be installed inside a sandbox, in order not to interfere with other Haskell projects.

The [System.Console.Hawk.Sandbox](https://github.com/gelisam/hawk/blob/master/src/System/Console/Hawk/Sandbox.hs) module detects the location of the sandbox, if any, and modifies the behavior of hint so that it looks for modules inside this sandbox.

### User prelude

Another adjustment to hint's default behavior has to do with module loading. The user expression is supposed to have access to the same symbols as are available inside the user prelude, but even though asking hint to load the user prelude causes the modules it imports to be loaded as well, those modules aren't brought into scope. To obtain the expected behavior, [System.Console.Hawk.UserPrelude](https://github.com/gelisam/hawk/blob/master/src/System/Console/Hawk/UserPrelude) parses the user prelude and extracts its import statements, so they can be imported along with the user prelude. We also extract and reapply the language extensions, such as `NoImplicitPrelude` and `OverloadedStrings`.

For some of those extensions, reapplying them is not enough. The `NoImplicitPrelude` extension is supposed to turn off GHC's default implicit-Prelude behavior, but in hint, this behavior is already off by default. To simulate the turned-on behavior, [System.Console.Hawk.UserPrelude.Extend](https://github.com/gelisam/hawk/blob/master/src/System/Console/Hawk/UserPrelude/Extend.hs) adds an extra "`import Prelude`" statement to the original user prelude, and we ask hint to load this modified version instead.

We save this modified user prelude under `~/.hawk/cache/cached_prelude.hs`, so named because we also compile and cache the result until the original user prelude changes. In order to compile it, we need to make another small adjustment: adding a module declaration, to prevent GHC from complaining about a missing `main`.

Those two modifications entail yet another adjustment: since GHC gives line numbers in its error messages, we need to somehow compensate for the fact that our extra "`module MyModule where`" and "`import Prelude`" lines make all later line numbers off by two when compared to the original user prelude. Our solution is [Data.HaskellSource](https://github.com/gelisam/hawk/blob/master/src/Data/HaskellSource.hs), a module which keeps track of where each original line used to be, and inserts `LINE` pragmas as appropriate to fix GHC's error messages.

Note that unlike the previous modules, `Data.HaskellSource` isn't located inside the `System.Console.Hawk` namespace. This is because its functionality is not unique to Hawk: other programs might conceivably want to modify Haskell source files in a way which doesn't screw up the line numbers. The same applies to many other modules, such as [Data.HaskellModule](https://github.com/gelisam/hawk/blob/master/src/Data/HaskellModule), which extracts and adds import statements and language extensions. If somebody requests it, we could easily package up those modules and publish them to hackage.

### Context directory

The user prelude may live in the `~/.hawk` context directory, or in another directory specified on the command-line, or in a `.hawk` folder placed in a parent of the current directory. The job of [System.Console.Hawk.Context](https://github.com/gelisam/hawk/blob/master/src/System/Console/Hawk/Context) is to figure out which context directory to use, and to fill it with a default user prelude if needed.

In addition to the user prelude, the context directory also contains a few cached files:

    $ ls ~/.hawk/cache
    cache/cached_prelude.hs
    cache/cached_prelude.hi
    cache/cached_prelude.o
    cache/cached_prelude.dat

The `.hs` file is our modified version of the user prelude, the `.hi` and `.o` files are generated by GHC during compilation, and the `.dat` file remembers the list of imports and language extensions used by the user prelude, so we don't have to re-parse the file.

The format of the `.dat` file is determined by [Data.Cache](https://github.com/gelisam/hawk/blob/master/src/Data/Cache.hs), a tiny experimental library for specifying caching policies using combinators, and by [Control.Monad.Trans.State.Persistent](https://github.com/gelisam/hawk/blob/master/src/Control/Monad/Trans/State/Persistent.hs), an even tinier library for persisting state to disk (it's just a wrapper around [writeFile](http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:writeFile)).

### Locking

Since the `.dat` file is shared between all the instances of Hawk, it is necessary to use [System.Console.Hawk.Lock](https://github.com/gelisam/hawk/blob/master/src/System/Console/Hawk/Lock.hs) to prevent two simultaneous instances from fighting over the file. Running two Hawk instances in parallel is more common than you would think: simply pipe the output of one Hawk command into another.

One subtle design point of the locking strategy is that like most command-line text processors, we want the beginning of the transformed stream to be passed down the pipe before reading the rest of the input. Therefore, we must be careful not to hold the lock during the processing itself, otherwise the data would accumulate inside the second Hawk instance while waiting for the first instance to release the lock. Instead, the lock is only held while *computing* the action which will do the processing, not while *executing* it.

### Runtime

The text-processing action is computed by evaluating the user expression in an environment which contains the user prelude, the modules it imports, and a few extra definitions specific to Hawk. Those definitions live in a small library, named "haskell-awk", which is installed at the same time as the `hawk` executable. The reason those definitions are placed in a separate library is because hint has no way to import definitions which only exist in an executable.

One of those definitions which must be imported is the type of the expression interpreted by hint. Since we are interpreting a function, [System.Console.Hawk.Runtime.Base](https://github.com/gelisam/hawk/blob/master/runtime/System/Console/Hawk/Runtime/Base.hs) exports two types of interest: an input type named `HawkRuntime` describing everything the `hawk` executable needs to transfer into the hint world, and an output type which is just a wrapper around `IO ()`.

Currently, `HawkRuntime` only describes two things: the format to use for the input, and the format to use for the output. The input format specifies whether stdin should be divided into records and fields, and if so which delimiters to split on. The output format also specifies delimiters, but it doesn't specify how the output should be divided, because that depends on the output type of the user expression. To dispatch on this type, [System.Console.Hawk.Representable](https://github.com/gelisam/hawk/blob/master/runtime/System/Console/Hawk/Representable.hs) defines a type class and a bunch of instances, allowing the output format to vary according to the output type.

Like we said above, hint evaluates a function from `HawkRuntime` to `IO ()`. This function isn't the user expression itself, but the wrapped expression `"processTable (<user_expression>)"`, where `processTable` is a function from [System.Console.Hawk.Runtime](https://github.com/gelisam/hawk/blob/master/runtime/System/Console/Hawk/Runtime.hs) which accepts a function operating on bytestrings and a `HawkRuntime`. It then consumes the input as specified by the `HawkRuntime`, and applies the bytestring function to the result.

### Modes and formats

The `processTable` function only knows how to handle functions which accept a 2D table of bytestrings. This corresponds to Hawk's `--apply` mode. To obtain the other modes ("map" and "eval"), [System.Console.Hawk](https://github.com/gelisam/hawk/blob/master/src/System/Console/Hawk.hs) modifies the user expression so that it begins with `map` or `const`. Similar manipulations are required when `-D''` and `-d''` are used to disable record- and field-splitting, because then the input type becomes `B.ByteString` or `[B.ByteString]` instead of `[[B.ByteString]]`.

### OptionParser

To determine which mode and input format to use, we must of course parse the command-line arguments. We use [Control.Monad.Trans.OptionParser](https://github.com/gelisam/hawk/blob/master/src/Control/Monad/Trans/OptionParser.hs), a monadic wrapper around the standard [GetOpt](http://hackage.haskell.org/package/base-4.7.0.0/docs/System-Console-GetOpt.html) library. I don't know how it compares to the many other [alternatives](https://www.haskell.org/haskellwiki/Command_line_option_parsers), but it allows flag arguments to be given types more precise than String, and as such has a greater focus on error handling, in case incorrectly-typed arguments are given on the command-line.

### UncertainT

In fact, most of the codebase has to worry about error handling, for which we use the [Control.Monad.Trans.Uncertain](https://github.com/gelisam/hawk/blob/master/src/Control/Monad/Trans/Uncertain.hs) monad transformer. It allows any monadic code to display a warning or fail with an error message.

## Future development

In terms of features, we like to give the user the choice about everything, but to pick good defaults. In terms of implementation, we like to implement many small generic libraries, allowing the Hawk-specific code to be as short and declarative as possible. We also write a lot of tests: low-level tests as a form of documentation and high-level tests to prevent regressions.

We currently plan to add more modes and input formats, not by adding more cases to the existing logic, but by implementing a more generic system which would give the users the freedom to define their own modes and their own formats. We also plan to add an extra configuration file, which would allow Hawk's behaviour to be changed more significantly and more permanently than by using command-line arguments for everything.

For more up-to-date plans, see our [list of issues](https://github.com/gelisam/hawk/issues) on GitHub.
