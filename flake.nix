{
  description = "YAML 1.2.2 reader and writer for Common Lisp";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-regex-kit = {
      url = "github:nerima-lisp/cl-regex-kit/v2.1.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    yaml-test-suite = {
      url = "github:yaml/yaml-test-suite/data-2022-01-17";
      flake = false;
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      cl-nix-forge,
      cl-weave,
      cl-regex-kit,
      cl-codec-kit,
      cl-json-kit,
      yaml-test-suite,
      treefmt-nix,
    }:
    cl-nix-forge.lib.aarch64-darwin.mkPackageFlake {
      inherit self nixpkgs;
      pname = "cl-yaml-kit";
      # The organisation standard narrowed this to x86_64-linux alone, on the
      # grounds that a platform CI does not exercise is not one to promise. The
      # maintainer develops on aarch64-darwin with no Linux builder configured,
      # so dropping darwin would leave the gate unrunnable, not narrower. The
      # cost is that CI, which runs ubuntu-latest alone, checks only the Linux
      # system and darwin is gated by `nix flake check` on the maintainer's
      # machine. Closing that needs either a Linux builder or a darwin runner.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      asd = ./cl-yaml-kit.asd;
      root = ./.;
      lispDependencies = ctx: [
        cl-regex-kit.packages.${ctx.system}.cl-regex-kit
        cl-codec-kit.packages.${ctx.system}.cl-codec-kit
      ];
      lispCheckDependencies = ctx: [
        cl-weave.packages.${ctx.system}.cl-weave
        cl-json-kit.packages.${ctx.system}.cl-json-kit
      ];
      packageArgs = _: { YAML_TEST_SUITE = yaml-test-suite; };
      timeoutSeconds = 600;
      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;
      extraOutputs = ctx: {
        apps.benchmark = ctx.cl.mkTestApp {
          pname = "cl-yaml-kit-benchmark";
          runner = "benchmark/run.lisp";
          src = ctx.lispDerivationArgs.src;
          lisp = ctx.lispDerivationArgs.lisp;
          lispDependencies =
            ctx.lispDerivationArgs.lispDependencies ++ ctx.lispDerivationArgs.lispCheckDependencies;
          timeoutSeconds = 1200;
          killAfterSeconds = 30;
        };
      };
    };
}
