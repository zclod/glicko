let config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {

                glicko = self.callCabal2nix "glicko" ./glicko.cabal {};

            };
        };
    };
};

pkgs = import <nixpkgs> {inherit config;};

in {
    glicko = pkgs.haskellPackages.glicko;
}
