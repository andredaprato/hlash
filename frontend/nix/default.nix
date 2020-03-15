{ sources ? import ./sources.nix }:
with
  {
  overlay = _ : pkgs:
    {
      niv = import sources.niv {};
      spago2nix = import sources.spago2nix {};
      easy-ps = (import sources.easy-purescript-nix {}).buildInputs ;
    };
  };
import sources.nixpkgs
  { overlays = [ overlay ]  ; config = {}; }
