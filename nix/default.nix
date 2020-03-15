{ sources ? import ./sources.nix
}:
with
  { overlay = self : pkgs :
    {
      inherit (import sources.niv {}) niv;
      ghcide = (import sources.ghcide-nix {}).ghcide-ghc865;
    };
    haskellnix = (import sources."haskell.nix");
  };
  import sources.nixpkgs
    { overlays = [ overlay ] ++ haskellnix.overlays ; config = haskellnix.config ;}
