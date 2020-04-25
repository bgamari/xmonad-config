{ nixpkgs ? (import <nixpkgs> {}) }:

let src = nixpkgs.fetchFromGitHub {
  name = "nixpkgs";
  owner = "nixos";
  repo = "nixpkgs";
  rev = "c47830563954d4ab4e593a9db6275ce828497f52";
  sha256 = "1xqcwzyzimay2kh8nqabi5b0ly3lc50c0w6asqjl0g97nckr2fj0";
};
in import src {}
