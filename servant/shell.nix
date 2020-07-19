{ pkgs ? (import ./nix/nixpkgs)
, compiler ? "ghc883"
}:

with pkgs;

let
  ghc = haskell.packages.${compiler}.ghcWithPackages (_: []);
  pgclient = pkgs.copyPathToStore ./pgclient;
in
  mkShell {
    buildInputs = [
      cabal-install
      ghc
      haskellPackages.ghcid
      hlint
      ormolu
      postgresql_10
      zlib
    ];
    shellHook = ''
      export LD_LIBRARY_PATH="${zlib}/lib";

      export PGDATABASE="''${PGDATABASE:-servant_overview}" \
             PGDATA="''${PGDATA:-$PWD/nix/pgdata}" \
             PGHOST="''${PGHOST:-$PWD/nix/sockets}" \
             PGPORT="''${PGPORT:-5433}" \
             PGUSER="''${PGUSER:-$USER}"

      trap "
        cd '$PWD'
        '${pgclient}' remove
      " EXIT

      '${pgclient}' add
    '';
  }
