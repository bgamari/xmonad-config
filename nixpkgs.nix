{ nixpkgs ? (import <nixpkgs> {}) }:

let src = nixpkgs.fetchFromGitHub {
  name = "nixpkgs";
  owner = "nixos";
  repo = "nixpkgs";
  rev = "4e623d16e3247f9e6e34c2095367468c127222ad";
  sha256 = "0nsyak5np2gvmq9sgq1fwh7p7cvlk6bada2yixdqbnjf8affwxkp";
};
in import src {}
