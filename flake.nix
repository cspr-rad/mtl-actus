{
  description = "Execute ACTUS traces in metric temporal logic";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    parts.url = "github:hercules-ci/flake-parts";
    fmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      parts,
      fmt,
    }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      imports = [
        ./nix/actus
        ./nix/shells.nix
        ./nix/comms
        fmt.flakeModule
        ./nix/format.nix
      ];
      flake.herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
