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
        # packages.sizedty-wrapper-quickcheck = # (ref:haskell-package-def)
        #   haskellPackages.callCabal2nix "sizedty-wrapper-quickcheck" ./sizedty-wrapper-quickcheck rec {
        #     # Dependency overrides go here
        #     sizedty-wrapper = packages.sizedty-wrapper;
        #   };
        # packages.sizedty-wrapper-aeson = # (ref:haskell-package-def)
        #   haskellPackages.callCabal2nix "sizedty-wrapper-aeson" ./sizedty-wrapper-aeson rec {
        #     # Dependency overrides go here
        #     sizedty-wrapper = packages.sizedty-wrapper;
        #   };
        # packages.sizedty-wrapper-text = # (ref:haskell-package-def)
        #   haskellPackages.callCabal2nix "sizedty-wrapper-text" ./sizedty-wrapper-text rec {
        #     # Dependency overrides go here
        #     sizedty-wrapper = packages.sizedty-wrapper;
        #   };

        defaultPackage = pkgs.linkFarmFromDrvs "all-sized-wrapper" (pkgs.lib.unique (builtins.attrValues packages));


        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            haskell-ci
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
