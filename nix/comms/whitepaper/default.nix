{ self, pkgs }:
pkgs.stdenv.mkDerivation {
  name = "mtl-actus-whitepaper";
  buildInputs = [
    pkgs.pandoc
    pkgs.texliveSmall
  ];
  src = "${self}/comms/whitepaper";
  buildPhase = ''
    export FONTCONFIG_PATH=$src
    pandoc \
           --from org \
           --to latex \
           --csl $src/acm.csl \
           --number-sections \
           --citeproc \
           $src/source.org --out whitepaper.pdf
  '';
  installPhase = ''
    mkdir -p $out
    cp whitepaper.pdf $out
  '';
}
