{ whitepaper, pkgs, ... }:

let
  effectScript = "putStateFile whitepaper.pdf ${whitepaper}/whitepaper.pdf";
in
pkgs.effects.mkEffect { inherit effectScript; }
