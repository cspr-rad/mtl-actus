{ inputs, ... }:
{
  perSystem =
    { pkgs, config, ... }:
    {
      treefmt.config = {
        projectRootFile = "flake.nix";
        programs = {
          nixfmt-rfc-style.enable = true;
          prettier.enable = true;
        };
      };
    };
}
