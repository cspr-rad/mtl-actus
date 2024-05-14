{ self, pkgs }:
{
  long = pkgs.stdenv.mkDerivation {
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
  };
  short = pkgs.stdenv.mkDerivation {
    name = "mtl-actus-microdeck";
    buildInputs = [ pkgs.typst ];
    src = "${self}/comms/may-2024-actus-dc/";
    buildPhase = ''
      typst compile short.typ
    '';
    installPhase = ''
      mkdir -p $out
      cp short.pdf $out/short.pdf
    '';
  };
  blog = pkgs.stdenv.mkDerivation {
    name = "mtl-actus-medium-article-1";
    buildInputs = [ pkgs.pandoc ];
    src = "${self}/comms/may-2024-actus-dc/";
    buildPhase = ''
      pandoc -t docx -o medium-article.docx medium-article.org
    '';
    installPhase = ''
      mkdir -p $out
      cp medium-article.docx $out/medium-article-1.docx
    '';
  };
}
