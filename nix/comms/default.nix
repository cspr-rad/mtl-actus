{ inputs, ... }:
{
  perSystem =
    { config, pkgs, ... }:
    {
      packages = {
        dc-talk = import ./may-2024-actus-dc {
          inherit (inputs) self;
          inherit pkgs;
        };
        whitepaper = import ./whitepaper {
          inherit (inputs) self;
          inherit pkgs;
        };
      };
    };
}
