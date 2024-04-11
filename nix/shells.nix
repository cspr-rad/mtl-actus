{ ... }:
{
  perSystem =
    { config, pkgs, ... }:
    {
      devShells =
        let
          greeting = "ACTUS in metric temporal logic";
        in
        {
          lean =
            with pkgs;
            mkShell {
              name = "mtlactus-dev";
              shellHook = "echo ${greeting}";
              buildInputs = [ lean4 ];
            };
          typst =
            with pkgs;
            mkShell {
              name = "mtlactus-typesetting";
              shellHook = "echo ${greeting}";
              buildInputs = [
                typst
                typstfmt
                typst-lsp
                typst-live
              ];
            };
        };
    };
}
