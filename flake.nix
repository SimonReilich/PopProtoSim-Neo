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

      checks = forAllSystems (system: {
        # Check that the main package builds
        package = self.packages.${system}.default;
      });

      apps = forAllSystems (system: {
        # 'nix run' will build this
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/${name}";
          meta = {
            description = "${name} is a simulator for population protocols with snipers";
          };
        };
      });

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

          hie-bios-deps = pkgs.writeShellScriptBin "hie-bios-deps" ''
            echo "-i."
            echo "-XOverloadedStrings" # Example of adding default extensions
            # Add any other GHC flags your project needs
          '';
        in
        pkgs.mkShell {
          packages = [
            testEnv
            pkgs.haskell-language-server
            pkgs.haskellPackages.hoogle
            pkgs.ormolu
          ];
          shellHook = ''
            echo -e "\nloaded haskell environment with Glasgow Haskell Compiler ${testEnv.version}\n"
            printf "cradle:\n  direct:\n    arguments:\n      - \"-iapp\"\n      - \"Main.hs\"" >> hie.yaml
          '';
        };
    };
}
