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
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.1.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-prolog-kit = {
      url = "github:nerima-lisp/cl-prolog-kit/v1.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.7.1";
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
      cl-parser-kit,
      cl-prolog-kit,
      cl-json-kit,
      paredit-cli,
      yaml-test-suite,
      treefmt-nix,
    }:
    let
      # CI and release jobs remain x86_64-linux; local checks also cover Darwin.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      timeLimit = command: "timeout --kill-after=30 300 ${command}";

      limitCommand =
        command: script:
        if nixpkgs.lib.hasInfix command script then
          builtins.replaceStrings [ command ] [ (timeLimit command) ] script
        else
          throw "cl-yaml-kit: `${command}` is missing from a generated check, so its time limit would not apply";
    in
    cl-nix-forge.lib.${builtins.head systems}.mkPackageFlake {
      inherit self systems nixpkgs;
      pname = "cl-yaml-kit";
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
        cl-parser-kit.packages.${ctx.system}.cl-parser-kit
      ];
      lispCheckDependencies = ctx: [
        cl-weave.packages.${ctx.system}.cl-weave
        cl-prolog-kit.packages.${ctx.system}.cl-prolog-kit
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
      killAfterSeconds = 30;
      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;
      extraOutputs = ctx: {
        checks.paredit-lint =
          ctx.pkgs.runCommand "cl-yaml-kit-paredit-lint"
            {
              nativeBuildInputs = [
                ctx.pkgs.coreutils
                paredit-cli.packages.${ctx.system}.lint
              ];
            }
            ''
              ${timeLimit "paredit-lint ${./.}"}
              touch "$out"
            '';
        checks.coverage = ctx.cl.mkCoverageReport {
          drv = ctx.package;
          # run-coverage.lisp applies scripts/coverage-gate.lisp to cl-weave's
          # per-file statistics and exits non-zero when the gate fails.
          entryPoint = "scripts/run-coverage.lisp";
          timeoutSeconds = 1200;
          killAfterSeconds = 30;
        };
        apps.mutation =
          let
            mutationApp = ctx.cl.mkTestApp {
              pname = "cl-yaml-kit-mutation";
              runner = "scripts/run-mutation.lisp";
              description = "Run mutation testing over the selected cl-yaml-kit functions";
              src = ctx.lispDerivationArgs.src;
              lisp = ctx.lispDerivationArgs.lisp;
              lispDependencies =
                ctx.lispDerivationArgs.lispDependencies ++ ctx.lispDerivationArgs.lispCheckDependencies;
              timeoutSeconds = 3600;
              killAfterSeconds = 30;
            };
          in
          {
            type = "app";
            program = "${
              ctx.pkgs.writeShellApplication {
                name = "cl-yaml-kit-mutation-with-cache";
                runtimeInputs = [ ctx.pkgs.coreutils ];
                text = ''
                  export YAML_TEST_SUITE=${yaml-test-suite}
                  cache_dir="$HOME/.cache/cl-yaml-kit-mutation"
                  mkdir -p "$cache_dir"
                  export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t \"$cache_dir/\") :inherit-configuration)"
                  exec ${mutationApp.program} "$@"
                '';
              }
            }/bin/cl-yaml-kit-mutation-with-cache";
            meta = mutationApp.meta;
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
      overrideOutputs =
        ctx:
        let
          docsSite = ctx.generated.packages.docs.overrideAttrs (old: {
            buildPhase = limitCommand "mkdocs build" old.buildPhase;
          });
        in
        {
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
          packages.docs = docsSite;
          checks.docs = ctx.generated.checks.docs.overrideAttrs { site = docsSite; };
          checks.formatting = ctx.generated.checks.formatting.overrideAttrs (old: {
            buildCommand = limitCommand "treefmt --no-cache" old.buildCommand;
          });
        };
    };
}
