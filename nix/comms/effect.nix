{ whitepaper, pkgs, ... }:

let
  effectScript = "putStateFile whitepaper.pdf ${whitepaper}/whitepaper.pdf";
in
pkgs.hci-effects.mkEffect { inherit effectScript; }
