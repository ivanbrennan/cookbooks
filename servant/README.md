# [Servant](http://www.servant.dev/) cookbook

## Development environment

Provision a development environment with [Nix](https://nixos.org/):
```sh
nix-shell
```

## Build

```sh
ghcid --command='cabal v2-repl' --no-height-limit --reverse-errors --clear --lint
```

## Run

```sh
cabal v2-run
```
