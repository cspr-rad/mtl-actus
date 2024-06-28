{ self, pkgs }:
pkgs.stdenv.mkDerivation {
  name = "mtl-actus-whitepaper";
  buildInputs = [
    pkgs.pandoc
    pkgs.texliveSmall
  ];
  src = "${self}/comms/whitepaper";
  buildPhase = ''
    pandoc -t latex -f org --citeproc $src/source.org -o whitepaper.pdf
  '';
  installPhase = ''
    mkdir -p $out
    cp whitepaper.pdf $out
  '';
}
