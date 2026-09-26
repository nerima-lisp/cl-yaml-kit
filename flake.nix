{
  description = "YAML 1.2.2 reader and writer for Common Lisp";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, cl-nix-forge, cl-weave, treefmt-nix }:
    cl-nix-forge.lib.aarch64-darwin.mkPackageFlake {
      inherit self nixpkgs;
      pname = "cl-yaml-kit";
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      asd = ./cl-yaml-kit.asd;
      root = ./.;
      lispCheckDependencies = ctx: [ cl-weave.packages.${ctx.system}.cl-weave ];
      timeoutSeconds = 600;
      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;
    };
}
