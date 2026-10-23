# Building Virtual Moon Atlas for macOS (Apple Silicon)

This branch (`macos-cocoa-port`) is a native arm64 / Cocoa port of the whole
eight-program VMA suite: `atlun`, `calclun`, `datlun`, `notelun`, `photlun`,
`weblun`, `cclun`, `catlun`. Two scripts take a clean checkout to a
distributable app.

Verified with FPC 3.2.2 + Lazarus 3.6 on macOS 26 (M1 Pro). Also builds and
runs clean with FPC 3.2.2 + Lazarus **4.8** on macOS 27 (M1) — fpcupdeluxe's
`stable.gitlab` target as of 2026-10, no patch changes needed.

## 0. Build workspace

Everything that isn't the source checkout - the fpcupdeluxe toolchain (it
unpacks thousands of files) and the packaging output - goes in one **build
workspace**, so it stays out of your home dir and out of the git checkout.
(The Moon texture/database data is separate again - see step 3 - and isn't
part of this workspace at all.)

Both scripts prompt for it and default to **`~/vma-build`**. To skip the
prompt, `export VMA_BUILD=~/vma-build` (or any path). Use the same value for
both scripts.

```
~/vma-build/
  fpcupdeluxe/    the toolchain (you install this - step 1)
  dist/           packaging output, incl. Virtual-Moon-Atlas-arm64.dmg (step 4)
```

The Moon texture/database set is not part of this workspace at all -- see
step 3, it goes straight to `~/Library/Application Support/Virtual Moon
Atlas/` instead, independent of the `.app` build entirely.

## 1. Toolchain

Xcode Command Line Tools:

```
xcode-select --install
```

FPC + Lazarus with the Cocoa widgetset. The no-sudo route is **fpcupdeluxe**
(<https://github.com/LongDirtyAnimAlf/fpcupdeluxe>): install FPC **3.2.2
(stable)** and Lazarus **3.6 (stable)**, with install dir **`~/vma-build/fpcupdeluxe`**.
Homebrew's `fpc` / `lazarus` do not work on current macOS.

> If a future Xcode drops the classic linker, FPC 3.2.2 will not link Cocoa
> code (`ld: malformed method list atom ...`). Move fpcupdeluxe to FPC `fixes`
> (3.2.4) / trunk and Lazarus trunk, rebuild, and drop `-k-ld_classic` from the
> `.lpi` CustomOptions.

### Homebrew packages

These scripts assume **Homebrew** (<https://brew.sh>) is installed - it is on
essentially every Mac dev machine.

```
brew install openssl@3      # REQUIRED - calclun/weblun bundle it for TLS (step 4)
```

If `openssl@3` is genuinely unavailable, put a fixed `libssl.3.dylib` /
`libcrypto.3.dylib` pair (see step 4) in `<workspace>/dist/libs/` by hand and
`package-app.sh` will use those instead.
For convenience the files are also available in Installer/Macos/data/openssl-mac-arm64.tgz

## 2. Build the suite

```
tools/macos/build-suite.sh
```

Asks for the workspace, then: registers the vendored Lazarus packages, builds
`libplan404` and CSPICE, and `lazbuild`s all eight programs. It expects the
toolchain at `<workspace>/fpcupdeluxe/...`; override with `FPC=`, `LB=`, `PCP=`
env vars. Allow 10-20 min the first time (CSPICE is ~2200 C files).

Output binaries (these land in the checkout - that's normal for `lazbuild`):

| program | path |
|---|---|
| atlun (3D globe, main) | `virtualmoon/units/aarch64-darwin-cocoa/atlun` |
| calclun (ephemeris) | `calclun/calclun` |
| datlun (formations DB) | `datlun/units/aarch64-darwin-cocoa/datlun` |
| notelun (notebook) | `notelun/notelun` |
| photlun (photo library) | `photlun/photlun` |
| weblun (data updater) | `weblun/weblun` |
| cclun (launcher / help) | `cclun/cclun` |
| catlun (catalog editor) | `catlun/units/aarch64-darwin-cocoa/catlun` |

Plus `virtualmoon/library/plan404/libplan404.dylib` and
`calclun/cspice/libcspice.dylib`.

## 3. Base data

The ~200 MB-to-several-GB Moon texture / database set is **not bundled into
the `.app` at all**, and `package-app.sh` doesn't need it present to build
or package -- it goes straight to
`~/Library/Application Support/Virtual Moon Atlas/` instead, so updating
or reinstalling the app later never touches it, and no add-on data pack
ever needs to go anywhere under `/Applications`, not even temporarily.

The base data are installed directly from Internet or from a local directory the 
first time the program run.
More data can be installed later using the menu File / Install additional packages.

## 4. Package

```
tools/macos/package-app.sh
```

Asks for the same workspace, then assembles
`<workspace>/dist/pkg-stage/Virtual Moon Atlas.app` (all eight binaries +
helper dylibs + bundled OpenSSL 3 + one shared `Contents/Resources` data set),
adhoc-signs it, emits the `VMA Menu` (`cclun.app`) Spotlight sidecar, and
writes **`<workspace>/dist/Virtual-Moon-Atlas-arm64.dmg`**.

If `<workspace>/dist/libs/libssl.3.dylib` / `libcrypto.3.dylib` are absent the
script derives them from `brew --prefix openssl@3` - copies the pair, rewrites
their install names to `@loader_path`, re-signs. Drop your own fixed pair in
`<workspace>/dist/libs/` to skip that.

## Distribution

The `.dmg` is **adhoc-signed, not notarized**. First launch on another Mac:
right-click -> *Open*, or

```
xattr -dr com.apple.quarantine "/Applications/Virtual Moon Atlas.app"
```

Install `Virtual Moon Atlas.app` and `VMA Menu` into the **same** folder.

For a real release: sign with a Developer ID certificate and notarize the
`.dmg`; `component/glscene/Source/GLScene.inc` should read
`{.$DEFINE GLS_LOGGING}` (off - its state on this branch; flip it on only for
local diagnostics, it writes `atlun.log`).

## What's what

| file | role |
|---|---|
| `build-suite.sh` | toolchain -> eight binaries + libplan404 + libcspice |
| `package-app.sh` | binaries -> `.app` + `.dmg` (output under the workspace; no data bundled, see step 3) |
| `vmamenu.c` | compiled arm64 stub the `VMA Menu` sidecar re-`exec()`s `cclun` with (a script there trips Gatekeeper's "install Rosetta" on Rosetta-less Macs) |

Full rationale for every code change is in `PORT_REPORT.md` (attached to Mantis
issue 0002948, <https://www.ap-i.net/mantis/view.php?id=2948>) - or ask there.
