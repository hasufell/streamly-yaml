# YAML (streamly) Haskell Library

This project contains two haskell packages: `yaml-streamly` for higher-level parsing and writing of yaml documents, and `libyaml-streamly`
for lower-level event-based streaming.

The reasoning and migration from conduit is explained in this blog post:
[From conduit to streamly](https://hasufell.github.io/posts/2021-10-22-conduit-to-streamly.html).

This is a fork of `yaml`/`libyaml` that replaces [conduit](https://hackage.haskell.org/package/conduit) with [streamly](https://hackage.haskell.org/package/streamly).

## `yaml-streamly` Package

`yaml-streamly` provides a high-level interface based around the JSON datatypes provided by the `aeson` package. This allows
using JSON and YAML interchangeably in your code with the `aeson` typeclasses. See the
[yaml README](./yaml-streamly/README.md) for more details.

## `libyaml-streamly` Package

`libyaml-streamly` is a wrapper over the libyaml C library (and includes the source so no external library is needed). It is an
event-based streaming API. See the [libyaml README](./libyaml-streamly/README.md) for
more details.

## License

This project is licensed with the BSD 3-Clause license. Copies of the license can be found in both the `yaml-streamly` and
`libyaml-streamly` subdirectories
