{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      devSystem = "x86_64-linux";
      forAllSystems =
        f:
        builtins.listToAttrs (
          map (system: {
            name = system;
            value = f system;
          }) systems
        );
      name = "proto-sim";
      version = "0.1.0.0";
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          # 'nix build' will build this
          default = pkgs.stdenv.mkDerivation {
            pname = name;
            version = version;
            src = ./app;

            buildInputs = with pkgs; [
              (pkgs.haskellPackages.ghcWithPackages (
                p: with p; [
                  random
                  list-duplicate
                  containers_0_8
                  split
                  docopt
                  text
                  safe-coloured-text
                ]
              ))
            ];

            buildPhase = ''
              mkdir -p $out/bin
              ghc --make Main.hs -o $out/bin/${name}
            '';
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # This must be the result of the function call, NOT the function itself.
          # It returns a derivation containing GHC + the libraries.
          testEnv = pkgs.haskellPackages.ghcWithPackages (
            p: with p; [
              random
              list-duplicate
              containers_0_8
              split
              docopt
              text
              safe-coloured-text
              hspec
              QuickCheck
            ]
          );
        in
        {
          # 1. Check that the main package builds
          package = self.packages.${system}.default;

          # 2. Run the tests (requires Spec.hs)
          # test =
          #   pkgs.runCommand "tests-${version}"
          #     {
          #       # Make the 'app' directory available as $src
          #       src = ./app;
          #
          #       # Tools needed to run the build (GHC env) go here
          #       nativeBuildInputs = [ testEnv ];
          #     }
          #     ''
          #       # Copy source files to current directory to avoid read-only issues
          #       cp -r $src/* .
          #
          #       # Run the test script
          #       # runhaskell uses the libraries from testEnv automatically
          #       runhaskell Spec.hs
          #
          #       # Create output file to signal success
          #       touch $out
          #     '';
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          # 'nix run' will build this
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/${name}";
            meta = {
              description = "${name} is a simulator for population protocols with snipers";
            };
          };
        }
      );

      devShells.${devSystem}.default =
        let
          pkgs = nixpkgs.legacyPackages.${devSystem};

          testEnv = pkgs.haskellPackages.ghcWithPackages (
            p: with p; [
              random
              list-duplicate
              containers_0_8
              split
              docopt
              text
              safe-coloured-text
              hspec
              QuickCheck
            ]
          );
        in
        pkgs.mkShell {
          packages = [
            testEnv
          ];
          shellHook = ''
            echo -e "\nloaded haskell environment with Glasgow Haskell Compiler ${testEnv.version}\n"
          '';
        };
    };
}
