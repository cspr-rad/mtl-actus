{
  description = "Execute ACTUS traces in metric temporal logic";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    parts.url = "github:hercules-ci/flake-parts";
    fmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    effects.url = "github:hercules-ci/hercules-ci-effects";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      parts,
      fmt,
      effects,
    }:
    let
      mainSystem = "x86_64-linux";
    in
    parts.lib.mkFlake { inherit inputs; } (
      { withSystem, ... }:
      {
        systems = [
          mainSystem
          "aarch64-darwin"
          "x86_64-darwin"
        ];
        imports = [
          effects.flakeModule
          ./nix/actus
          ./nix/shells.nix
          ./nix/comms
          fmt.flakeModule
          ./nix/format.nix
          ./nix/herc.nix
        ];
      }
    );
}
