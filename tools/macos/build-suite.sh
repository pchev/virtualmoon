#!/bin/bash
# build-suite.sh - build the whole VMA suite as native arm64 (Apple Silicon)
# from a clean checkout of this branch. Produces the 8 program binaries plus the
# two helper C libraries (libplan404, libcspice) that atlun and calclun need.
#
# Prereq: FPC 3.2.2 + Lazarus 3.6 with the Cocoa widgetset, via fpcupdeluxe
#   (https://github.com/LongDirtyAnimAlf/fpcupdeluxe), installed into
#   <workspace>/fpcupdeluxe (see the prompt below). Override the toolchain
#   paths with FPC= LB= PCP= env vars.
#
# Usage:
#   tools/macos/build-suite.sh                 # prompts for the workspace dir
#   VMA_BUILD=~/vma-build tools/macos/build-suite.sh   # non-interactive
#
# Run from anywhere - it locates the repo as the grandparent of this script.
set -euo pipefail

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$REPO"

# --- build workspace ------------------------------------------------------
# Everything that is not the source checkout - the fpcupdeluxe toolchain (which
# unpacks a lot of files), the extracted base data, and the packaging output -
# lives here, out of your home dir and out of the git checkout.
default_workdir="$HOME/vma-build"
if   [ -n "${VMA_BUILD:-}" ]; then WORKDIR="$VMA_BUILD"
elif [ -t 0 ]; then
  read -r -p "Build workspace [$default_workdir]: " WORKDIR
  WORKDIR="${WORKDIR:-$default_workdir}"
else
  WORKDIR="$default_workdir"
  echo "==> build workspace: $WORKDIR  (set VMA_BUILD to change)"
fi
WORKDIR="${WORKDIR/#\~/$HOME}"
mkdir -p "$WORKDIR"
WORKDIR="$(cd "$WORKDIR" && pwd)"

# fpcupdeluxe wraps the compiler in fpc.sh ("fpc -n @<dir>/fpc.cfg $@") - it
# forces the right config and ignores any system fpc.cfg. The bare `fpc` binary
# needs a discoverable fpc.cfg and can't find fpcupdeluxe's on a clean machine
# ("Can't find unit system"), so prefer fpc.sh - it's also what fpcupdeluxe
# puts in the Lazarus config as CompilerFilename.
_fpcdir="$WORKDIR/fpcupdeluxe/fpc/bin/aarch64-darwin"
if   [ -n "${FPC:-}" ];            then :
elif [ -x "$_fpcdir/fpc.sh" ];     then FPC="$_fpcdir/fpc.sh"
else                                    FPC="$_fpcdir/fpc"
fi
LB="${LB:-$WORKDIR/fpcupdeluxe/lazarus/lazbuild}"
PCP="${PCP:-$WORKDIR/fpcupdeluxe/config_lazarus}"

for t in "$FPC" "$LB"; do
  [ -x "$t" ] || { echo "not found / not executable: $t" >&2
    echo "install FPC 3.2.2 + Lazarus 3.6 via fpcupdeluxe into $WORKDIR/fpcupdeluxe," >&2
    echo "or set FPC= LB= PCP= to an existing toolchain." >&2
    exit 1; }
done
[ -d "$PCP" ] || { echo "Lazarus primary config dir not found: $PCP" >&2; exit 1; }

echo "==> FPC       $("$FPC" -iV 2>/dev/null || echo '?')   ($FPC)"
echo "==> lazbuild  $LB"
echo "==> repo      $REPO"
echo "==> workspace $WORKDIR"

# --- linker preflight ------------------------------------------------------
# The .lpi files pass -k-ld_classic: FPC 3.2.2's prebuilt Obj-C method-list
# layout trips the modern ld, and the classic linker (-ld_classic) is the
# workaround. Two ways it bites:
#   * the active linker genuinely lacks -ld_classic (very new toolchain), or
#   * fpcupdeluxe pinned -FD<Command Line Tools bin> in fpc.cfg and that ld is a
#     pre-2023 relic that never had the flag (xcode-select won't move it).
# Probe by linking a Pascal program the way lazbuild will; if -FD/usr/bin (the
# xcode-select shim dir) fixes it, route FPC through a wrapper that adds it.
_fpc_link_ok() {  # $1=compiler ; $2..=extra fpc args
  local cc0="$1"; shift
  local t rc=0
  t="$(mktemp -d)" || return 2
  printf 'begin end.\n' > "$t/p.pas"
  ( cd "$t" && "$cc0" -Paarch64 -Tdarwin -k-ld_classic "$@" p.pas ) >/dev/null 2>&1 || rc=1
  [ -x "$t/p" ] || rc=1
  rm -rf "$t"
  return $rc
}
if ! _fpc_link_ok "$FPC"; then
  if _fpc_link_ok "$FPC" -FD/usr/bin; then
    printf '#!/bin/sh\nexec "%s" -FD/usr/bin "$@"\n' "$FPC" > "$WORKDIR/fpc-ldshim.sh"
    chmod +x "$WORKDIR/fpc-ldshim.sh"
    echo "==> fpc.cfg pins a stale linker dir; routing FPC through -FD/usr/bin"
    echo "    ($WORKDIR/fpc-ldshim.sh)"
    FPC="$WORKDIR/fpc-ldshim.sh"
  else
    cat >&2 <<EOF

Linking with -k-ld_classic fails, and -FD/usr/bin does not fix it - no linker
on this machine supports -ld_classic. Fix one of:
  * refresh Command Line Tools:  sudo xcode-select --install
  * point xcode-select at a current Xcode.app:
      sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
  * move fpcupdeluxe to FPC fixes (3.2.4) / trunk and delete every
    "-k-ld_classic" from the .lpi CustomOptions
Active dev dir: $(xcode-select -p 2>/dev/null)
EOF
    exit 1
  fi
fi

# --- vendored Lazarus packages ----------------------------------------------
echo "==> registering vendored packages"
LPKS=(
  virtualmoon/component/synapse/laz_synapse.lpk
  virtualmoon/component/bgrabitmap/bgrabitmappack.lpk
  virtualmoon/component/downloaddialog/downldialog.lpk
  virtualmoon/component/uniqueinstance/uniqueinstance_package.lpk
  virtualmoon/component/enhedits/enhedit.lpk
  virtualmoon/component/indiclient/indiclient.lpk
  virtualmoon/component/libsql/libsql.lpk
  virtualmoon/component/glscene/Packages/GLScene_RunTime.lpk
  virtualmoon/component/glscene/Packages/GLScene_DesignTime.lpk
  virtualmoon/component/vmacomponents.lpk
)
for lpk in "${LPKS[@]}"; do
  "$LB" --pcp="$PCP" --add-package-link "$lpk" >/dev/null
done

# --- libplan404 (atlun) ---------------------------------------------------
echo "==> libplan404.dylib"
( cd virtualmoon/library/plan404
  mkdir -p obj
  make CFLAGS="-O3 -fPIC -std=gnu89 -Wno-implicit-function-declaration -Wno-deprecated-non-prototype" )

# --- CSPICE + getcell (calclun) ----------------------------------------
echo "==> libcspice.dylib + getcell.o  (~2200 C files - give it a few minutes)"
( cd calclun/cspice
  make -f Makefile.cspice all
  install_name_tool -id @executable_path/libcspice.dylib libcspice.dylib )
( cd calclun
  clang -c -O3 -w -fPIC -I./cspice getcell.c -o getcell.o )

# --- the 8 programs ----------------------------------------------------------
LB_COMMON=( --pcp="$PCP" --compiler="$FPC"
           --cpu=aarch64 --operating-system=darwin --ws=cocoa --recursive )

build() {  # <lpi> [extra lazbuild args...]
  local lpi="$1"; shift
  echo "==> lazbuild $lpi ${*:+($*)}"
  "$LB" "${LB_COMMON[@]}" "$@" "$lpi"
}

build virtualmoon/atlun.lpi --build-mode=macos
build calclun/calclun.lpi --build-mode=macos
build datlun/datlun.lpi --build-mode=macos
build notelun/notelun.lpi --build-mode=macos
build photlun/photlun.lpi --build-mode=macos
build weblun/weblun.lpi --build-mode=macos
build cclun/cclun.lpi --build-mode=macos
#build catlun/catlun.lpi --build-mode=macos

# --- report ----------------------------------------------------------------
echo
echo "==> results"
ok=1
check() { if [ -f "$REPO/$1" ]; then echo "    OK   $1"; else echo "    MISS $1"; ok=0; fi; }
check virtualmoon/units/aarch64-darwin-cocoa/atlun
check calclun/calclun
check datlun/units/aarch64-darwin-cocoa/datlun
check notelun/notelun
check photlun/photlun
check weblun/weblun
check cclun/cclun
#check catlun/units/aarch64-darwin-cocoa/catlun
check virtualmoon/library/plan404/libplan404.dylib
check calclun/cspice/libcspice.dylib
echo
if [ "$ok" = 1 ]; then
  echo "    all present."
  echo "    next: VMA_BUILD=$WORKDIR tools/macos/package-app.sh"
  echo "          (no base data needed for this step -- see README step 3:"
  echo "          tools/macos/get-virtmoon-data.sh, run after install, fetches"
  echo "          it straight to ~/Library/Application Support/ instead)"
else
  echo "    some targets missing - see the lazbuild output above" >&2
  exit 1
fi
