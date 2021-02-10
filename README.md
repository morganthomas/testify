# testify

My best friend!

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
