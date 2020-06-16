# Contributing to Hawk

Welcome aboard! If you're in a hurry, don't worry about these guidelines, just send us a pull request or open a [new issue](https://github.com/gelisam/hawk/issues/new) to start a discussion thread with us. Alternatively, if you would prefer to discover the codebase on your own, we have written a document giving an [overview of the codebase](doc/code-overview.md).

## What needs to be done?

Some of the issues are marked as [good first issue](https://github.com/gelisam/hawk/issues?q=label%3A%22good+first+issue%22), [medium](https://github.com/gelisam/hawk/issues?labels=medium) and [hard](https://github.com/gelisam/hawk/issues?labels=hard). Of course, all issues will be somewhat hard if you're not familar with the codebase, so feel free to ask questions via a [new issue](https://github.com/gelisam/hawk/issues/new)!

"Easy" means you should be able to find a solution just by finding the appropriate place in the code (presumably with our help) and making a small local change.

"Medium" issues will require you to link a few pieces together, either by getting familiar with an outside library, an unusual Haskell feature, or some other part of Hawk's codebase.

"Hard" issues may require the code to be refactored in order to create a place where the new feature can be added. Ideally, the refactoring and the addition of the feature should be in separate commits.

Issues with no difficulty markers might be off limits, or maybe we simply haven't updated the difficulty tags in a while. If one of those issues looks interesting to you, just add a comment and ask!

## Contributing a patch

If you already wrote the patch, you can simply send it to us by email of via a pull request. If you haven't written it yet, please read on.

### Bugfixes

Please send us a pull request! We like to add tests to prevent regressions, but don't worry about it. Just explain how you encountered the bug in your pull request and we'll take care of the rest.

### New features

While you could certainly send a pull request, it's better to begin by opening an issue so we can discuss the feature with you first. Very often, feature requests coincide with features that are already planned, or could easily be implemented in terms of them.

### Planned features

Instead of implementing one feature at a time, we like to come up with generic systems under which many features arise as user customizations. For example, there are many issues requesting new input formats such as json and yaml. We do plan to support those, but instead of adding `--json` and `--yaml` flags, we want to allow Hawk to support arbitrary input formats via a configuration file explaining how to parse each.

So if the feature you want is in the same category as several other issues which have been opened for a long time, it's probably better to open an issue and to wait for the generic system than to submit an implementation and see it being made obsolete by the new system.
