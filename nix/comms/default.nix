{ inputs, ... }:
{
  perSystem =
    { config, pkgs, ... }:
    {
      packages = {
        # This needs to be built outside of nix builder with a shell, cuz of imports
        # dc-talk = import ./may-2024-actus-dc {
        #   inherit (inputs) self;
        #   inherit pkgs;
        # };
        whitepaper = import ./whitepaper {
          inherit (inputs) self;
          inherit pkgs;
        };
        whitepaper-typst = import ./whitepaper/typst.nix {
          inherit (inputs) self;
          inherit pkgs;
        };
      };
    };
}
