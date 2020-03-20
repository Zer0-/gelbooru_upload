{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }: 
let

  inherit (nixpkgs) pkgs;

  req = pkgs.callPackage ./req.nix {};

  f = { mkDerivation, base, stdenv, req, cabal-install,
        cryptohash-md5, base64-bytestring, hxt, hxt-css
      }:
      mkDerivation {
        pname = "gelbooru-upload";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base req cryptohash-md5 base64-bytestring hxt hxt-css
        ];
        testHaskellDepends = [ cabal-install ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {
    req = req;
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
