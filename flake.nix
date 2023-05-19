{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Used for shell.nix
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, fenix, ... }:
  let system = "x86_64-linux";
   pkgs = import nixpkgs { inherit system;  };
  aarch64-gnu-tch = import nixpkgs {
    localSystem = system;
    crossSystem = {
      config = "aarch64-unknown-linux-gnu";
    };
  };
   in {
    
     devShells.x86_64-linux.default = pkgs.mkShell {
       nativeBuildInputs = with pkgs; [
         fenix.packages.x86_64-linux.minimal.toolchain
       ];
       buildInputs = with pkgs; [
         rust-analyzer
         qemu
         aarch64-gnu-tch.buildPackages.gcc
         aarch64-gnu-tch.buildPackages.glibc
         aarch64-gnu-tch.buildPackages.gdb
       ];
     };


  };
}
