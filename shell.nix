let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  haskellDeps = ps: with ps; [ base hspec rio ];

  ghc = pkgs.haskell.compiler.ghc961 haskellDeps;

  inputs = [ pkgs.cabal-install pkgs.gcc pkgs.ghc pkgs.ghcid pkgs.llvm pkgs.nixfmt ];

  # hooks = ''
  #   mkdir -p .nix-stack
  #   export STACK_ROOT=$PWD/.nix-stack
  # '';
  hooks = ''
  '';
in pkgs.stdenv.mkDerivation {
  name = "ffx";
  src = ./.;
  buildInputs = inputs;
  shellHook = hooks;
}
