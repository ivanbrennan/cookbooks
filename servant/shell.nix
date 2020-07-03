{ pkgs ? (import ./nix/nixpkgs)
, compiler ? "ghc883"
}:

with pkgs;

let
  ghc = haskell.packages.${compiler}.ghcWithPackages (_: []);
in
  mkShell {
    buildInputs = [
      cabal-install
      ghc
      haskellPackages.ghcid
      hlint
      zlib
    ];
    shellHook = ''
      export LD_LIBRARY_PATH="${zlib}/lib";
    '';
  }
