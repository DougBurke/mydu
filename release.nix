{ sources ? import ./nix/sources.nix
# , nixpkgs ? import sources.nixpkgs {}
, pkgs ? import sources.nixpkgs {}
, compiler ? "ghc902"
}:

let
  # inherit (nixpkgs) pkgs;
  
  # since we are in a sub-directory
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "mydu" =
        hself.callCabal2nix "mydu" (gitignore ./.) {};

      # formatting =
      #   hself.callHackage "formatting" "7.1.2" {};
      # formatting = hself.formatting_7_1_2;
      
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."mydu"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      # pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hlint
      pkgs.niv
    ];
    # withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."mydu");

in
  {
   inherit shell;
   inherit exe;
   inherit myHaskellPackages;
   "mydu" = myHaskellPackages."mydu";
  }

