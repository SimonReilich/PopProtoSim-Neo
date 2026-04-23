{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
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
      version = "v1.2.0";
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          haskellEnv = pkgs.haskellPackages.ghcWithPackages (
            p: with p; [
              random
              list-duplicate
              containers_0_8
              split
              docopt
              text
              safe-coloured-text
            ]
          );
        in
        {
          default = self.packages.${system}.proto-sim;

          proto-sim = pkgs.stdenv.mkDerivation {
            pname = name;
            version = version;
            src = ./app;

            buildInputs = [ haskellEnv ];

            buildPhase = ''
              mkdir -p $out/bin
              ghc --make Main.hs -o $out/bin/${name}
            '';
          };

          library = pkgs.stdenv.mkDerivation {
            pname = "${name}-library";
            version = version;
            src = ./app;

            buildInputs = [ haskellEnv ];

            buildPhase = ''
              mkdir -p $out/lib $out/include
              ghc --make -dynamic -fPIC -shared Main.hs -o $out/lib/libprotosim.so
              cp Main_stub.h $out/include/protosim.h
            '';
          };
        }
      );

      checks = forAllSystems (system: {
        package = self.packages.${system}.default;
      });

      apps = forAllSystems (system: {
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
            rm hie.yaml
            printf "cradle:\n  direct:\n    arguments:\n      - \"-iapp\"\n      - \"Main.hs\"" >> hie.yaml
          '';
        };
    };
}
