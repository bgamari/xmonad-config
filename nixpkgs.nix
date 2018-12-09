{ nixpkgs ? (import <nixpkgs> {}) }:

let src = nixpkgs.fetchFromGitHub {
  name = "nixpkgs";
  owner = "nixos";
  repo = "nixpkgs-channels";
  rev = "e85c1f586807b5acd244df4c45a5130aa3f0734d";
  sha256 = "1xy1qgam0i2fyqhaczw0qrx8yv3hgdh9jp47wmln5ljiixr5ic5n";
};
in import src {}
