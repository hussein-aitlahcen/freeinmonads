{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, free, hspec, mtl, stdenv }:
      mkDerivation {
        pname = "freeinmonads";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base free hspec mtl ];
        testHaskellDepends = [ base free hspec mtl ];
        homepage = "https://github.com/hussein-aitlahcen/freeinmonads#readme";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
