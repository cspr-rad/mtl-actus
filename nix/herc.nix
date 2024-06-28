{ inputs, withSystem, ... }:
let
  ciSystem = "x86_64-linux";
  packages = inputs.self.packages.${ciSystem};
  whitepaper = packages.whitepaper;
in
{
  herculesCI =
    { config, ... }:
    withSystem ciSystem (
      { hci-effects, pkgs, ... }:
      let
        run-condition = config.repo.branch == "master";
        comms-effect = import ./comms/effect.nix { inherit whitepaper pkgs; };
      in
      {
        ciSystems = [ "x86_64-linux" ];
        onPush = {
          comms =
            if run-condition then
              { outputs.effects = comms-effect; }
            else
              {
                outputs = {
                  inherit whitepaper;
                };
              };
          lean-codebase.outputs = packages.default;
          devshells.outputs =
            let
              devShells = inputs.self.devShells.${ciSystem};
            in
            {
              inherit (devShells) lean comms;
            };
        };
      }
    );
}
