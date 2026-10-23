#!/bin/bash
# package-app.sh - assemble one self-contained "Virtual Moon Atlas.app" (arm64)
# holding the whole 8-program suite, then wrap it in a compressed .dmg.
#
# Run tools/macos/build-suite.sh first (builds the binaries + helper dylibs).
#
# Layout:
#   Contents/MacOS/        atlun calclun datlun notelun photlun weblun
#                          cclun  +  libplan404.dylib libcspice.dylib
#   Contents/Frameworks/   libssl.3.dylib libcrypto.3.dylib   (calclun / weblun
#                          Synapse dlopen()s these from ../Frameworks)
#   Contents/Resources/    language  atlun.icns
#
# No Moon texture/database data is bundled -- it lives in
# ~/Library/Application Support/Virtual Moon Atlas/ instead, so updating or
# reinstalling the app never touches it (and no add-on data pack ever goes
# anywhere under /Applications, not even transiently).
#
# atlun is the main executable; its toolbar / menus reach the others. cclun is
# an alternate launcher; catlun is the catalog editor. Cross-launch works
# because every sibling is a bare name in Contents/MacOS/ next to the running
# binary (11-combined-bundle-macos.patch); each program's GetAppDir probe
# checks ~/Library/Application Support/Virtual Moon Atlas/ first, falling
# back to Contents/Resources/ for a dev-build layout with no external data yet.
#
# Also emitted: "VMA Menu" (cclun.app), a few-KB wrapper .app whose compiled
# arm64 stub (vmamenu.c) re-exec()s cclun inside the fat bundle so the hub is
# independently Spotlight-launchable. Both .apps must install into one folder.
#
# Output (staging tree, libs, .dmg) goes under the build workspace, not the
# checkout - default ~/vma-build, prompted, or set VMA_BUILD.
set -euo pipefail

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
HERE="$(cd "$(dirname "$0")" && pwd)"

# --- build workspace ------------------------------------------------------
# Staging, bundled libs and the .dmg land here - out of your home dir and out
# of the git checkout. Use the same dir you gave build-suite.sh.
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

# source root: a bare clone keeps the programs at $REPO; an overlay layout that
# clones the VMA tree under upstream/ keeps them there
if   [ -f "$REPO/virtualmoon/atlun.lpi" ];          then SRC="$REPO"
elif [ -f "$REPO/upstream/virtualmoon/atlun.lpi" ]; then SRC="$REPO/upstream"
else echo "cannot find the VMA source (no virtualmoon/atlun.lpi under $REPO)" >&2; exit 1
fi

# prog : binary path relative to $SRC
BINS=(
  "atlun:virtualmoon/units/aarch64-darwin-cocoa/atlun"
  "calclun:calclun/calclun"
  "datlun:datlun/units/aarch64-darwin-cocoa/datlun"
  "notelun:notelun/notelun"
  "photlun:photlun/photlun"
  "weblun:weblun/weblun"
  "cclun:cclun/cclun"
)
PLAN404="$SRC/virtualmoon/library/plan404/libplan404.dylib"
CSPICE="$SRC/calclun/cspice/libcspice.dylib"
LANG_DIR="$SRC/language"
ICO="$SRC/virtualmoon/atlun.ico"

DIST="$WORKDIR/dist"
LIBS="$DIST/libs"
SSL="$LIBS/libssl.3.dylib"
CRYPTO="$LIBS/libcrypto.3.dylib"

STAGE="$DIST/pkg-stage"
APP="$STAGE/Virtual Moon Atlas.app"
DMG="$DIST/Virtual-Moon-Atlas-arm64.dmg"
VERSION="9.2"

# --- binaries / helper dylibs present? ------------------------------------
miss=0
for kv in "${BINS[@]}"; do
  [ -f "$SRC/${kv#*:}" ] || { echo "missing binary: ${kv#*:}" >&2; miss=1; }
done
for f in "$PLAN404" "$CSPICE"; do
  [ -f "$f" ] || { echo "missing: ${f#"$SRC"/}" >&2; miss=1; }
done
[ "$miss" = 0 ] || { echo "-> run tools/macos/build-suite.sh first" >&2; exit 1; }

for f in "$LANG_DIR"; do
  [ -e "$f" ] || { echo "missing (should be in the checkout): $f" >&2; exit 1; }
done

# --- OpenSSL 3: derive the bundled pair if not supplied ----------------
if [ ! -f "$SSL" ] || [ ! -f "$CRYPTO" ]; then
  echo "==> deriving libssl/libcrypto from Homebrew openssl@3"
  command -v brew >/dev/null || { echo "need 'brew install openssl@3' (or drop a fixed pair in $LIBS/)" >&2; exit 1; }
  KEG="$(brew --prefix openssl@3 2>/dev/null || true)/lib"
  [ -f "$KEG/libssl.3.dylib" ] || { echo "openssl@3 not installed: brew install openssl@3" >&2; exit 1; }
  mkdir -p "$LIBS"
  cp "$KEG/libssl.3.dylib" "$KEG/libcrypto.3.dylib" "$LIBS/"
  chmod u+w "$SSL" "$CRYPTO"
  install_name_tool -id @loader_path/libssl.3.dylib    "$SSL"
  install_name_tool -id @loader_path/libcrypto.3.dylib "$CRYPTO"
  old_crypto="$(otool -L "$SSL" | awk '/libcrypto\.3\.dylib/{print $1; exit}')"
  install_name_tool -change "$old_crypto" @loader_path/libcrypto.3.dylib "$SSL"
  codesign -s - -f "$CRYPTO" "$SSL"
fi

echo "==> clean stage"
rm -rf "$STAGE"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Frameworks" "$APP/Contents/Resources"

echo "==> 8 binaries -> Contents/MacOS"
for kv in "${BINS[@]}"; do
  name="${kv%%:*}"
  cp "$SRC/${kv#*:}" "$APP/Contents/MacOS/$name"
  chmod +x "$APP/Contents/MacOS/$name"
done

echo "==> dylibs"
cp "$PLAN404" "$APP/Contents/MacOS/libplan404.dylib"
cp "$CSPICE"  "$APP/Contents/MacOS/libcspice.dylib"
chmod +w "$APP/Contents/MacOS/libplan404.dylib" "$APP/Contents/MacOS/libcspice.dylib"
install_name_tool -id @executable_path/libplan404.dylib "$APP/Contents/MacOS/libplan404.dylib"
install_name_tool -id @executable_path/libcspice.dylib  "$APP/Contents/MacOS/libcspice.dylib"
cp "$SSL"    "$APP/Contents/Frameworks/libssl.3.dylib"
cp "$CRYPTO" "$APP/Contents/Frameworks/libcrypto.3.dylib"

echo "==> icon"
if command -v magick >/dev/null && [ -f "$ICO" ]; then
  magick "$ICO" -resize 512x512 "$STAGE/icon.png" 2>/dev/null || magick "${ICO}[0]" "$STAGE/icon.png"
  sips -s format icns "$STAGE/icon.png" --out "$APP/Contents/Resources/atlun.icns" >/dev/null 2>&1 || true
  rm -f "$STAGE/icon.png"
else
  echo "    (ImageMagick 'magick' not found - skipping icon, app gets the generic one)"
fi

echo "==> Info.plist"
cat > "$APP/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>            <string>Virtual Moon Atlas</string>
  <key>CFBundleDisplayName</key>     <string>Virtual Moon Atlas</string>
  <key>CFBundleIdentifier</key>      <string>org.virtualmoon.suite</string>
  <key>CFBundleVersion</key>         <string>${VERSION}</string>
  <key>CFBundleShortVersionString</key><string>${VERSION}</string>
  <key>CFBundlePackageType</key>     <string>APPL</string>
  <key>CFBundleSignature</key>       <string>????</string>
  <key>CFBundleExecutable</key>      <string>atlun</string>
  <key>CFBundleIconFile</key>        <string>atlun.icns</string>
  <key>NSHighResolutionCapable</key> <true/>
  <key>LSMinimumSystemVersion</key>  <string>11.0</string>
  <key>LSApplicationCategoryType</key><string>public.app-category.education</string>
  <key>NSSupportsAutomaticGraphicsSwitching</key><true/>
  <key>NSHumanReadableCopyright</key> <string>Virtual Moon Atlas by Christian Legrand and Patrick Chevalley. GPL.</string>
</dict>
</plist>
PLIST
printf 'APPL????' > "$APP/Contents/PkgInfo"

echo "==> language -> Contents/Resources"
cp -R "$LANG_DIR" "$APP/Contents/Resources/language"

echo "==> adhoc sign"
codesign --force --deep --sign - "$APP" >/dev/null
codesign --verify --deep "$APP" && echo "    signature OK"

echo "==> sidecar: cclun.app  (Spotlight: \"VMA Menu\")"
SIDE="$STAGE/cclun.app"
mkdir -p "$SIDE/Contents/MacOS" "$SIDE/Contents/Resources"
cc -arch arm64 -O2 -Wall -o "$SIDE/Contents/MacOS/vmamenu" "$HERE/vmamenu.c"
[ -f "$APP/Contents/Resources/atlun.icns" ] && cp "$APP/Contents/Resources/atlun.icns" "$SIDE/Contents/Resources/atlun.icns"
cat > "$SIDE/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>            <string>VMA Menu</string>
  <key>CFBundleDisplayName</key>     <string>VMA Menu</string>
  <key>CFBundleIdentifier</key>      <string>org.virtualmoon.vmamenu</string>
  <key>CFBundleVersion</key>         <string>${VERSION}</string>
  <key>CFBundleShortVersionString</key><string>${VERSION}</string>
  <key>CFBundlePackageType</key>     <string>APPL</string>
  <key>CFBundleSignature</key>       <string>????</string>
  <key>CFBundleExecutable</key>      <string>vmamenu</string>
  <key>CFBundleIconFile</key>        <string>atlun.icns</string>
  <key>NSHighResolutionCapable</key> <true/>
  <key>LSMinimumSystemVersion</key>  <string>11.0</string>
  <key>LSApplicationCategoryType</key><string>public.app-category.education</string>
</dict>
</plist>
PLIST
printf 'APPL????' > "$SIDE/Contents/PkgInfo"
codesign --force --deep --sign - "$SIDE" >/dev/null
codesign --verify --deep "$SIDE" && echo "    sidecar signature OK"

echo "==> drop-to-install layout + README"
ln -s /Applications "$STAGE/Applications"

cat > "$STAGE/READ ME FIRST.txt" <<'TXT'
Virtual Moon Atlas - native macOS (Apple Silicon)

One app, the whole suite: atlun (3D Moon globe), plus calclun, datlun,
notelun, photlun, weblun, cclun. Open atlun and reach the
others from its toolbar and menus.

Install:  drag BOTH "Virtual Moon Atlas.app" and "VMA Menu" (cclun.app)
onto the Applications folder, keeping them together.

First launch:  this build is ad-hoc signed, not notarized, so macOS
Gatekeeper will refuse the first open. Either:
  * Right-click (Control-click) the app -> Open -> Open, once, OR
  * Terminal:  xattr -dr com.apple.quarantine "/Applications/Virtual Moon Atlas.app"
  
The first time it run the program ask to install the required base data 
directly from the Internet or from a local folder you already download the files. 

Requires Apple Silicon and macOS 11 or later.
TXT

echo "==> build dmg"
rm -f "$DMG"
hdiutil create -volname "Virtual Moon Atlas" -srcfolder "$STAGE" \
  -fs HFS+ -format UDZO -imagekey zlib-level=9 -ov "$DMG" >/dev/null

echo "==> done"
du -sh "$APP"
ls -lh "$DMG"
echo
echo "    run in place:  open \"$APP\""
echo "    staging tree (keep or delete):  $STAGE"
