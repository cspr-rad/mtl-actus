{ whitepaper, hci-effects, ... }:

let
  effectScript = "putStateFile whitepaper.pdf ${whitepaper}/whitepaper.pdf";
in
hci-effects.mkEffect { inherit effectScript; }
