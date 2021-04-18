{ chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, compiler ? "ghc865"
, withHoogle ? false
, doHoogle ? false
, doHaddock ? false
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, strictDeps ? false
, isJS ? false
, asShell ? false
, system ? builtins.currentSystem
, optimize ? true
, docker ? false
}:
let


  recurse-ghcjs = import ./default.nix {
    inherit chan compiler withHoogle doHoogle doHaddock enableLibraryProfiling enableExecutableProfiling strictDeps asShell system optimize;
    docker = false;
    isJS = true;
  };


  # It's a shpadoinkle day
  shpadoinkle = builtins.fetchGit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "b61144563bf2842bb51441e68bbb8471487d49cc";
    ref    = "master";
  };


  # Additional ignore patterns to keep the Nix src clean
  ignorance = [
    "*.md"
    "figlet"
    "*.nix"
    "*.sh"
    "*.yml"
  ];


  # Get some utilities
  inherit (import (shpadoinkle + "/nix/util.nix") { inherit compiler isJS pkgs; }) compilerjs gitignore doCannibalize;


  # Build faster by doing less
  chill = p: (pkgs.haskell.lib.overrideCabal p {
    inherit enableLibraryProfiling enableExecutableProfiling;
  }).overrideAttrs (_: {
    inherit doHoogle doHaddock strictDeps;
  });


  # Overlay containing Shpadoinkle packages, and needed alterations for those packages
  # as well as optimizations from Reflex Platform
  shpadoinkle-overlay =
    import (shpadoinkle + "/nix/overlay.nix") { inherit compiler chan isJS enableLibraryProfiling enableExecutableProfiling; };


  # Haskell specific overlay (for you to extend)
  haskell-overlay = with pkgs.haskell.lib; hself: hsuper: {
    bsb-http-chunked = dontCheck hsuper.bsb-http-chunked;
    directory-tree = dontCheck hsuper.directory-tree;
    happy = dontCheck hsuper.happy;
    http2 = dontCheck hsuper.http2;
    http-date = dontCheck hsuper.http-date;
    iproute = dontCheck hsuper.iproute;
    network-byte-order = dontCheck hsuper.network-byte-order;
    servant-foreign = dontCheck hsuper.servant-foreign;
    streaming-commons = dontCheck hsuper.streaming-commons;
    unix-time = dontCheck hsuper.unix-time;
    wai-app-static = dontCheck hsuper.wai-app-static;
    wai-extra = dontCheck hsuper.wai-extra;
  };


  # Top level overlay (for you to extend)
  testify-overlay = self: super: {
    haskell = super.haskell //
      { packages = super.haskell.packages //
        { ${compilerjs} = super.haskell.packages.${compilerjs}.override (old: {
            overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) haskell-overlay;
          });
        };
      };
    };


  # Complete package set with overlays applied
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
    }) {
    inherit system;
    overlays = [
      shpadoinkle-overlay
      testify-overlay
    ];
  };


  ghcTools = with pkgs.haskell.packages.${compiler};
    [ cabal-install
      ghcid
    ] ++ (if isJS then [] else [ stylish-haskell ]);


  # We can name him George
  testify = pkgs.haskell.packages.${compilerjs}.callCabal2nix "testify" (gitignore ignorance ./.) {};

  selenium-server-standalone-jar = builtins.fetchurl { url = "https://selenium-release.storage.googleapis.com/2.53/selenium-server-standalone-2.53.1.jar"; };

  testify-build = (if isJS && optimize then doCannibalize else x: x) (chill testify);


in with pkgs; with lib;

  if inNixShell || asShell
  then pkgs.haskell.packages.${compilerjs}.shellFor {
    inherit withHoogle;
    packages    = _: [ testify ];
    COMPILER    = compilerjs;
    buildInputs = ghcTools ++ [ jre phantomjs2 ];
    shellHook   = ''
      ${lolcat}/bin/lolcat ${./figlet}
      cat ${./intro}
    '';
  }
  else if docker
  then import ./docker-image.nix { inherit pkgs;
                                   selenium-server-standalone-jar = ./selenium-server-standalone-2.53.1.jar;
                                   testify = testify-build;
                                   testify-js = recurse-ghcjs;
                                   jre = pkgs.jre; }
  else if isJS
  then stdenv.mkDerivation {
         name = "testify-static";
         src = ./src;
         buildInputs = [ testify-build ];
         configurePhase = ''
           echo configuring
         '';
         buildPhase = ''
           echo building
           mkdir -p $out
           cp ./index.html $out
           cp -r ${testify-build.outPath}/* $out
         '';
         installPhase = ''
           echo installing
         '';
       }
  else testify-build
