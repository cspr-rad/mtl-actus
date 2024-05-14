{ inputs, ... }:
{
  perSystem =
    { config, pkgs, ... }:
    let
      may-2024-actus-dc = import ./may-2024-actus-dc {
        inherit (inputs) self;
        inherit pkgs;
      };
    in
    {
      packages = {
        # This needs to be built outside of nix builder with a shell, cuz of typst imports
        # dc-talk = import ./may-2024-actus-dc {
        #   inherit (inputs) self;
        #   inherit pkgs;
        # };
        may-2024-medium-article = may-2024-actus-dc.blog;
        whitepaper = import ./whitepaper {
          inherit (inputs) self;
          inherit pkgs;
        };
        # whitepaper-typst = import ./whitepaper/typst.nix {
        #   inherit (inputs) self;
        #   inherit pkgs;
        # };
      };
    };
}
