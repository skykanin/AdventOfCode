{
  description = "Package build and dev environment for Advent of Code";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }: {
   
    # setup devShell for x86_64-linux.
    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        inherit (lib) makeLibraryPath;
        hs = haskell.packages.ghc884;
        tools = [
          binutils-unwrapped
          hs.ghc
          hs.cabal-install
          hs.ghcid
          hs.ormolu
        ];
        libraries = [];
        libraryPath = "${makeLibraryPath libraries}";
      in
        mkShell {
          buildInputs = tools ++ libraries;
          shellHook = ''
            export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${libraryPath}"
            export LIBRARY_PATH="${libraryPath}"
          '';
        };
  };
}
