{ self, pkgs }:
pkgs.stdenv.mkDerivation {
  name = "mtl-actus-talk";
  buildInputs = [ pkgs.typst ];
  src = "${self}/comms/may-2024-actus-dc/";
  buildPhase = ''
    typst compile source.typ
  '';
  installPhase = ''
    mkdir -p $out
    cp source.pdf $out/talk.pdf
  '';
}
