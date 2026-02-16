# Musikell

Deterministic, real-time, graph-based audio runtime.

Musikell executes audio node graphs in fixed blocks, streams via Unix pipelines, and supports interchangeable backends through a stable C ABI.

## Usage

```bash
musikell run graph.mkl
```

## Build

See [docs/development/build.mdx](docs/development/build.mdx) for:

- Local development (Nix, Cabal)
- CI pipeline (GitLab)
- Checks, formatting, coverage

Quick start:

```bash
nix build '.#dev'
./result/bin/musikell run graph.mkl
```

- `nix build '.#dev'` — build, unit tests, pre-commit (treefmt, nil, haskell-header)
- `nix build '.#stg'` — same as dev + tests with coverage and coverage output

## Documentation

- [docs/index.mdx](docs/index.mdx) — Architecture and module index
- [docs/ffi/ABI.mdx](docs/ffi/ABI.mdx) — C ABI specification for backends

## License

Apache-2.0 — see [LICENSE](LICENSE).
