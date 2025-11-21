{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    haskell-flake = {
      url = "github:srid/haskell-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`, e.g. from Hackage
          packages = {
            random.source = "1.3.1";
            list-duplicate.source = "0.1.0.0";
            containers.source = "0.8";
            split.source = "0.2.5";
            docopt.source = "0.7.0.8";
            text.source = "2.1.3";
            safe-coloured-text.source = "0.3.0.2";
          };

          autoWire = [ "packages" "apps" "checks" ];
        };
      };
    };
}