{ pkgs ? import ./nix { inherit sources; }
,  sources ? import ./nix/sources.nix }:
{ hlash-client =
    let spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
        removeHashBang = drv: drv.overrideAttrs (oldAttrs: {
          buildCommand = builtins.replaceStrings ["#!/usr/bin/env"] [""] oldAttrs.buildCommand;
        }); 
    in
      pkgs.stdenv.mkDerivation {
        name = "learn-halogen";
        src = ./src;
        buildPhase = 
          '' 
          ${removeHashBang spagoPkgs.installSpagoStyle}
          ${removeHashBang spagoPkgs.buildSpagoStyle} 
          ${removeHashBang spagoPkgs.buildFromNixStore}
             '';
 }; 
}
