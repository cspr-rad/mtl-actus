{
  mainSystem,
  inputs,
  withSystem,
  whitepaper,
}:

{ branch, ... }:
withSystem mainSystem (
  {
    config,
    hci-effects,
    pkgs,
    inputs',
    ...
  }:
  let
    effectScript = "putStateFile whitepaper.pdf ${whitepaper}/whitepaper.pdf";
  in
  pkgs.effects.mkEffect { inherit effectScript; }
)
