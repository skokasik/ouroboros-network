{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { demo = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "Win32-network"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "Win32 network API";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs.base) (hsPkgs.bytestring) (hsPkgs.Win32) ];
        };
      exes = {
        "named-pipe-demo" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.Win32)
            (hsPkgs.Win32-network)
            ];
          };
        };
      tests = {
        "test-Win32-network" = {
          depends = [
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.stm)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.Win32)
            (hsPkgs.Win32-network)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../././Win32-network; }