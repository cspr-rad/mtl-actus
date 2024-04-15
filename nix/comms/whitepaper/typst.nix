{ self, pkgs }:
pkgs.stdenv.mkDerivation {
  name = "mtl-actus-whitepaper";
  buildInputs = [ pkgs.typst ];
  src = "${self}/comms/whitepaper/";
  buildPhase = ''
    typst compile source.typ
  '';
  installPhase = ''
    mkdir -p $out
    cp source.pdf $out/whitepaper.pdf
  '';
}
