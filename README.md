# testify

## Build testify

```bash
nix-build
```

or

```bash
stack build
```

## Develop with live reloading

```bash
nix-shell --command "ghcid --command 'cabal repl' -W -T Main.dev"
```

## Build and run in Docker

```bash
./docker-load.sh
./docker-run.sh
```
