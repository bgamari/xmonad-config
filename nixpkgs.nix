{ nixpkgs ? (import <nixpkgs> {}) }:

let src = nixpkgs.fetchFromGitHub {
  name = "nixpkgs";
  owner = "nixos";
  repo = "nixpkgs";
  rev = "5b09b92a16f2bafbfcba4fbdfe7084a2b2d97d75";
  sha256 = "0x1ar1wlay432hd5a5zd2h80ld5xj2qpxg6ak42jlsvpi5mmdbmw";
};
in import src {}
