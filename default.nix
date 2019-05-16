# default.nix - define the build environment for RaptorJIT
#
# This file can be used by 'nix-build' or 'nix-shell' to create a
# pristine build environment with precisely the expected software in
# $PATH. This makes it possible to build raptorjit in the same way on
# any machine.
#
# See README.md for usage instructions.

{ pkgs ? (import ./pkgs.nix) {}
, source ? pkgs.lib.cleanSource ./.
, version ? "dev"
, check ? false }:

let
  callPackage = (pkgs.lib.callPackageWith { inherit pkgs source version; });
  raptorjit = (callPackage ./raptorjit.nix {});
  raptorjit-assert = raptorjit.overrideAttrs(
                       old: { NIX_CFLAGS_COMPILE = " -DLUA_USE_ASSERT"; });
  test = name: args: (callPackage ./test.nix { inherit name args; raptorjit = raptorjit-assert; });
  test-capi = callPackage ./test-capi.nix { inherit pkgs raptorjit; };
  test-libraptorjit = callPackage ./test-libraptorjit.nix { inherit pkgs raptorjit; };
  check-generated-code = (callPackage ./check-generated-code.nix { inherit raptorjit; });
  nowarnings = raptorjit.overrideAttrs(
                 old: { NIX_CFLAGS_COMPILE = "-Werror"; });
in

# Build RaptorJIT and run multiple test suites.
{
  raptorjit  = raptorjit;
  test-O3    = test "O3"    "-O3";
  test-O2    = test "O2"    "-O2";
  test-O1    = test "O1"    "-O1";
  test-nojit = test "nojit" "-joff";
  test-capi  = test-capi;
  test-libraptorjit = test-libraptorjit;
  # Test that generated bytecode is compatible
  test-bytecode-compat = pkgs.runCommand "test-bytecode-compat"
                                           { buildInputs = [ raptorjit ]; }
    ''
      echo "print('ok')" > test.lua
      raptorjit -bg test.lua test.raw
      raptorjit test.raw | grep ok
      touch $out
    '';
} //
(if check then { inherit nowarnings check-generated-code; } else {})

