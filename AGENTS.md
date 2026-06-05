# The Glasgow Haskell Compiler

A contributor's cheatsheet.

## Documentation

The Wiki is a comprehensive resource about GHC development. Use it when this
cheatsheet is insufficient.

Documentation for master is built with GitLab CI:

- User's Guide - command line options, language extensions, and so on.
- Libraries - Haddock for `base`, `containers`, `transformers`, and other boot
  libraries.

## Getting the Code

GHC has its own GitLab instance.

```sh
git clone --recurse-submodules https://gitlab.haskell.org/ghc/ghc.git
```

`git pull` also requires `--recurse-submodules` because GHC uses git
submodules.

## Preparing the System

Nix users are fortunate to have `ghc.nix`:

```sh
nix develop git+https://gitlab.haskell.org/ghc/ghc.nix.git -c $SHELL
```

This will install `alex`, `happy`, `texlive`, and other build dependencies.

## Building the Compiler

For the first time:

```sh
./boot && ./configure
cabal v2-update
hadrian/build -j
```

When building inside `ghc.nix`, prefer `configure_ghc` instead of a plain
`./configure` so Nix-provided include and library paths are passed through:

```sh
nix develop git+https://gitlab.haskell.org/ghc/ghc.nix.git -c bash -lc 'configure_ghc && hadrian/build -j'
```

If `hadrian/build -j` fails while compiling `terminfo` with
`fatal error: term.h: No such file or directory`, the ncurses development
headers are not in the configured include path. Reconfigure with
`configure_ghc`; a plain `./configure` inside `ghc.nix` can miss those curses
arguments.

Quick stage2 rebuild:

```sh
hadrian/build -j --freeze1
```

## Running

The build artifacts are stored in the `_build` directory.

Run the freshly built GHCi:

```sh
_build/stage1/bin/ghc --interactive
```

## Testing

Run a particular set of tests:

```sh
hadrian/build -j --freeze1 test --only="T1 T2 T3"
```

Use `-a` to accept the output of failing tests.

Omit `--only` to run the entire testsuite.

## Debugging

Pass the `-ddump-tc-trace` flag to dump the type checker debug output;
`-ddump-rn-trace` for the renamer.

Build GHC with assertions enabled:

```sh
hadrian/build -j --flavour=Devel2
```

Note that some tests may not pass in this configuration.

## Building the User's Guide

To build the User's Guide and Haddock documentation for boot libraries:

```sh
hadrian/build -j --freeze1 docs --docs=no-sphinx-pdfs
```

The generated HTML documentation is saved at:

```text
_build/docs/html/index.html
```

## Communication

To report a bug, use the GitLab issue tracker.

To contribute a change, open a GitLab merge request.

To propose a new language feature, use the ghc-proposals platform.

For a technical discussion or a question, use the ghc-devs mailing list or the
`#ghc` channel on libera.chat (IRC).

Serokell, 2026-05-15. Source on GitHub.
