# Hawk

Transform text from the command-line using Haskell expressions.

## About

Command-line tools such as [awk](http://en.wikipedia.org/wiki/AWK) are best for one-liners, while compiled languages such as [Haskell](http://www.haskell.org) are intended for larger projects, spanning several modules and thousands of lines of code.

Hawk lets you write Haskell one-liners on the command-line, and continues to support you as your one-liner grows into ten, one hundred lines. When your command-line expression becomes too large, move some of it to `~/.hawk/prelude.hs`, a Haskell module which hosts your custom helper functions. Once that file becomes too crowded, simply move that file to the folder of your choice, as the first module of a new Haskell project.
