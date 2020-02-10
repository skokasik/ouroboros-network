{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "ouroboros-network-framework";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.mtl)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.cborg)
          (hsPkgs.io-sim-classes)
          (hsPkgs.typed-protocols)
          (hsPkgs.network)
          (hsPkgs.network-mux)
          (hsPkgs.contra-tracer)
          (hsPkgs.dns)
          (hsPkgs.iproute)
          (hsPkgs.serialise)
          (hsPkgs.typed-protocols-examples)
          (hsPkgs.cardano-prelude)
          ];
        };
      exes = {
        "demo-ping-pong" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.directory)
            (hsPkgs.network-mux)
            (hsPkgs.network)
            (hsPkgs.ouroboros-network-framework)
            (hsPkgs.io-sim-classes)
            (hsPkgs.stm)
            (hsPkgs.text)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-examples)
            ];
          };
        };
      tests = {
        "ouroboros-network-framework-tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.cborg)
            (hsPkgs.serialise)
            (hsPkgs.io-sim-classes)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-examples)
            (hsPkgs.network)
            (hsPkgs.network-mux)
            (hsPkgs.ouroboros-network-framework)
            (hsPkgs.ouroboros-network-testing)
            (hsPkgs.contra-tracer)
            (hsPkgs.dns)
            (hsPkgs.iproute)
            (hsPkgs.io-sim)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ../../././ouroboros-network-framework;
    }