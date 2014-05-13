# What's new?

## New since 1.0

In a nutshell, custom context directories and GHC 7.8 compatibility.

### Breaking changes

* The detection of whether the user prelude needs to be recompiled has been improved, leading to the removal of the no-longer useful `--recompile` flag.

### "Breaking" changes (if you used internal stuff)

* The `haskell-awk` library has changed.
* The format of `~/.hawk/cache` has changed.

### New features

* GHC 7.8 compatibility.
* A context directory other than `~/.hawk` can be specified.
* A `.hawk` directory can be placed in the current directory (or one of its ancestors) and it will be used instead of `~/.hawk`. This allows different projects to use different user preludes.

### Bugfixes

* Some examples from the documentation needed minor changes in order to produce the documented output.
* `hawk ""` is now a syntax error.
* `cabal install --enable-tests` no longer complains that the runtime isn't installed.

### Minor improvements

* The nomenclature for "lines" and "words" is now "records" and "fields", because the delimiters don't need to be newline and space.
* Usage is shown when `hawk` is called with no arguments.
* A warning message is displayed when `~/.hawk` is first created.
* More succinct `--help` output.
* More uniform presentation of warnings and error messages.
