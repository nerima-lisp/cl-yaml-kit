# cl-yaml-kit

YAML 1.2.2 reader and writer for Common Lisp. The repository currently
contains the package, event/node contracts, and test harness; reader and
writer pipeline stages are being implemented incrementally.

## Development

```sh
nix develop
nix flake check
```

With SBCL and cl-weave available, run `sbcl --non-interactive --load
run-tests.lisp`.
