{ inputs, ... }:
{
  perSystem =
    { pkgs, config, ... }:
    {
      packages.default = pkgs.stdenv.mkDerivation {
        name = "mtl-actus";
        src = "${inputs.self}/src";
        buildInputs = [ pkgs.lean4 ];
        buildPhase = "lake build";
        installPhase = ''
          mkdir -p $out
          cp -r .lake/* $out
        '';
      };
    };
}
