# Flatfile X Generator

CLI tool that helps generate "X" configs.

## Local Dev

1. `cabal build`
2. `cabal exec -- ffx`

## Example Commands

1. Init (default) `ffx init --template typescript`
2. Init (javascript) `ffx init --template javascript`
3. Init (local) `ffx init --template file:../some/dir`
4. Init (remote) `ffx init --template remote:https://github.com/asdf/asdf.git`
5. Publish `ffx publish --file ../path/to/some/file`
