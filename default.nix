{ chan ? "5272327b81ed355bbed5659b8d303cf2979b6953"
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


  # It's a shpadoinkle day
  shpadoinkle = builtins.fetchGit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "1f81a7933112a8e054b8701d9c9921c1fc7cb105";
    ref    = "before";
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
    import (shpadoinkle + "/nix/overlay.nix") { inherit compiler chan isJS; };


  # Haskell specific overlay (for you to extend)
  haskell-overlay = hself: hsuper: {
    "happy" = pkgs.haskell.lib.dontCheck hsuper.happy;
    servant-foreign = pkgs.haskell.lib.dontCheck hsuper.servant-foreign;
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
      java -jar selenium-server-standalone-2.53.1.jar &
    '';
  }
  else if docker
  then import ./docker-image.nix { inherit pkgs;
                                   selenium-server-standalone-jar = ./selenium-server-standalone-2.53.1.jar;
                                   testify = testify-build;
                                   jre = pkgs.jre; }
  else testify-build
