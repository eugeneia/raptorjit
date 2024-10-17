{ pkgs, raptorjit, ... }:
with pkgs;
#with llvmPackages_12.stdenv;
with stdenv;

let lpty =
  fetchurl rec {
    url = "http://tset.de/downloads/lpty-1.2.2-1.tar.gz";
    sha256 = "071mvz79wi9vr6hvrnb1rv19lqp1bh2fi742zkpv2sm1r9gy5rav";
  };
in

mkDerivation {
  name = "test-capi";
  src = lpty;
  phases = "unpackPhase buildPhase testPhase";
  buildInputs = [ luajit raptorjit which ];
  testPhase = ''
    ${raptorjit}/bin/raptorjit -e 'require "lpty"  print("Successfully loaded a C library.")' \
      > $out
  '';
}

