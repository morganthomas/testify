# testify

## Build testify

```bash
nix-build
./build-static.sh
```

## Run testify

## Build and run the Docker image

```bash
./docker-load.sh
./docker-run.sh
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
