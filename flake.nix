{
  description = "YAML 1.2.2 reader and writer for Common Lisp";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.4.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-regex-kit = {
      url = "github:nerima-lisp/cl-regex-kit/v2.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.6.0";
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
    cl-nix-forge.lib.x86_64-linux.mkPackageFlake {
      inherit self nixpkgs;
      pname = "cl-yaml-kit";
      # CI and release verification run only on ubuntu x86_64.
      systems = [ "x86_64-linux" ];
      asd = ./cl-yaml-kit.asd;
      root = ./.;
      meta = {
        description = "YAML 1.2.2 reader and writer for Common Lisp";
        homepage = "https://github.com/nerima-lisp/cl-yaml-kit";
        license = nixpkgs.lib.licenses.mit;
        platforms = nixpkgs.lib.platforms.unix;
      };
      lispDependencies = ctx: [
        cl-regex-kit.packages.${ctx.system}.cl-regex-kit
        cl-codec-kit.packages.${ctx.system}.cl-codec-kit
      ];
      lispCheckDependencies = ctx: [
        cl-weave.packages.${ctx.system}.cl-weave
        cl-json-kit.packages.${ctx.system}.cl-json-kit
      ];
      packageArgs = _: {
        YAML_TEST_SUITE = yaml-test-suite;
      };
      sourceInclude = [
        ./docs
        ./scripts
      ];
      timeoutSeconds = 1200;
      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;
      extraOutputs = ctx: {
        checks.coverage = ctx.cl.mkCoverageReport {
          drv = ctx.package;
          # run-coverage.lisp applies scripts/coverage-gate.lisp to cl-weave's
          # per-file statistics and exits non-zero when the gate fails.
          entryPoint = "scripts/run-coverage.lisp";
          timeoutSeconds = 1200;
          killAfterSeconds = 30;
        };
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
      overrideOutputs = ctx: {
        apps.test = {
          type = "app";
          program = "${
            ctx.pkgs.writeShellApplication {
              name = "cl-yaml-kit-test-with-fixtures";
              text = ''
                export YAML_TEST_SUITE=${yaml-test-suite}
                exec ${ctx.generated.apps.test.program} "$@"
              '';
            }
          }/bin/cl-yaml-kit-test-with-fixtures";
        };
      };
    };
}
