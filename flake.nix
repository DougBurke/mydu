{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "An opinionated disk-usage tool";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      # supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        mydu = final.haskellPackages.callCabal2nix "mydu" ./. {};
      });
      packages = forAllSystems (system: {
         mydu = nixpkgsFor.${system}.mydu;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.mydu);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.mydu];
          # withHoogle = true;
          buildInputs = with haskellPackages; [
            # haskell-language-server
	    ghcid
            hlint
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = "export PS1='\\e[1;34mdev[mydu] > \\e[0m'";
        });
  };
}
