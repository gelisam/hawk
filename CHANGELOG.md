# What's new?

## Next
* support ghc-8.10, ghc-9.0, ghc-9.2, and ghc-9.4.8

## New in 1.2.0.1
* support ghc-8.6 again and back to ghc-8.0 (Stackage LTS 9)
* fix missing test files in released tarball (#257)

## New in 1.2

* Added support for ghc-9.0.1, ghc-8.10 and ghc-8.8, dropped support for ghc-8.6 and below.
* Drop support for Windows. Please contact us if you would like us to re-enable it.
* Error messages use the correct column number.
* The flag `-d` without an argument no longer implies `-o` without an argument. That is, when using whitespace to delimit words in the input, we now also use whitespace to delimit words in the output, we no longer use the empty string as a delimiter.
* Hawk can now be installed using either stack, cabal-v1, cabal-v2, or cabal-sandbox.
* Hawk no longer suspiciously opens a port; we now use files for locking, not a unix socket.

## New in 1.1.1

GHC 7.10 compatibility.

## New in 1.1

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
