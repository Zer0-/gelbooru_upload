{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, authenticate-oauth, base
      , blaze-builder, bytestring, case-insensitive, connection, hspec
      , hspec-core, hspec-discover, http-api-data, http-client
      , http-client-tls, http-types, modern-uri, monad-control, mtl
      , QuickCheck, retry, stdenv, text, time, transformers
      , transformers-base, unordered-containers, unliftio-core
      }:
      mkDerivation {
        pname = "req";
        version = "3.5.0";
        src = ../req;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          aeson authenticate-oauth base blaze-builder bytestring
          case-insensitive connection http-api-data http-client
          http-client-tls http-types modern-uri monad-control mtl retry text
          time transformers transformers-base unliftio-core
        ];
        testHaskellDepends = [
          aeson base blaze-builder bytestring case-insensitive hspec
          hspec-core http-client http-types modern-uri monad-control mtl
          QuickCheck retry text time unordered-containers
        ];
        testToolDepends = [ hspec-discover ];
        doCheck = false;
        homepage = "https://github.com/mrkkrp/req";
        description = "Easy-to-use, type-safe, expandable, high-level HTTP client library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  drv
  #if pkgs.lib.inNixShell then drv.env else drv
