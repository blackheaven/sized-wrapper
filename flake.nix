{
  description = "sized-wrapper";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc923;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
      in
      rec
      {
        packages.sized-wrapper = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "sized-wrapper" ./sized-wrapper rec {
            # Dependency overrides go here
          };
        packages.sized-wrapper-quickcheck = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "sized-wrapper-quickcheck" ./sized-wrapper-quickcheck rec {
            # Dependency overrides go here
            sized-wrapper = packages.sized-wrapper;
          };
        packages.sized-wrapper-aeson = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "sized-wrapper-aeson" ./sized-wrapper-aeson rec {
            # Dependency overrides go here
            sized-wrapper = packages.sized-wrapper;
          };
        # packages.sized-wrapper-text = # (ref:haskell-package-def)
        #   haskellPackages.callCabal2nix "sized-wrapper-text" ./sized-wrapper-text rec {
        #     # Dependency overrides go here
        #     sized-wrapper = packages.sized-wrapper;
        #   };

        defaultPackage = pkgs.linkFarmFromDrvs "all-sized-wrapper" (pkgs.lib.unique (builtins.attrValues packages));


        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            haskell-ci
          ];
          inputsFrom = [
            packages.sized-wrapper-aeson.env
          ];
        };
      });
}
