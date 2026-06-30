# kotoba-lang/fs

[![CI](https://github.com/kotoba-lang/fs/actions/workflows/ci.yml/badge.svg)](https://github.com/kotoba-lang/fs/actions/workflows/ci.yml)

**Layer 3 (I/O) of the kotoba foundational stdlib** — pure path manipulation
plus an `IFilesystem` **protocol** the host injects. A capability-confined cell
never touches the OS directly; it only sees a filesystem handle the host
granted. Zero third-party runtime deps; every namespace is `.cljc` (JVM / SCI /
ClojureScript / GraalVM / kotoba-WASM). See
[`docs/adr/ADR-kotoba-lang-foundational-stdlib.md`](https://github.com/kotoba-lang/kotoba-lang/blob/main/docs/adr/ADR-kotoba-lang-foundational-stdlib.md).

## Why a protocol, not the OS

The kotoba-WASM premise forbids direct OS access. `fs` splits cleanly: the
**path ops** (`join`, `split`, `basename`, …) are pure functions anyone can
call; the **effectful ops** (`read`, `write`, `list`, `exists?`, `delete`) live
behind `IFilesystem`, which the host implements and injects. An in-memory
`mem-filesystem` is provided for tests and OSS-standalone use — same seam as
`kotobase.store/IStore` and `num.protocol/IBackend`.

## Current surface

`kotoba.lang.fs`:

- Pure path ops: `join`, `split`, `basename`, `dirname`, `normalize`, `ext`,
  `relative?`, `absolute?`
- `IFilesystem` protocol: `read`, `write`, `list`, `exists?`, `delete`
- `mem-filesystem` — atom-backed in-memory impl (OSS standalone / tests)

## Install

```clojure
io.github.kotoba-lang/fs {:git/sha "<sha>"}
```

## Use

```clojure
(require '[kotoba.lang.fs :as fs])

(fs/join "a" "b" "c.txt")              ;=> "a/b/c.txt"
(fs/ext "a/b/c.txt")                   ;=> "txt"
(let [m (fs/mem-filesystem)]
  (fs/write m "dir/x.txt" "hello")
  (fs/read m "dir/x.txt")              ;=> "hello"
  (fs/list m "dir")                    ;=> ["x.txt"])
```

## Verify

```sh
clojure -M:test
```
