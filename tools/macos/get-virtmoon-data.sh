#!/bin/zsh
# Fetch one of upstream's data .deb packages from SourceForge and extract
# straight into ~/Library/Application Support/Virtual Moon Atlas/ -- the
# .app bundle carries no data at all (see tools/macos/package-app.sh),
# so this is how data actually gets there now, the same way
# install_data*.sh does it on Linux (destdir, just external instead of
# inside the .app).
#
#   1. Package URLs are resolved from SourceForge's project RSS feed
#      (https://sourceforge.net/projects/virtualmoon/rss) instead of a
#      hardcoded "Version 9.0" folder path -- picks the newest file whose
#      name matches each package's prefix, by pubDate, so this keeps
#      working if upstream ever publishes a newer one without the script
#      needing an update. (The feed also carries an MD5 hash per file,
#      which this script verifies the download against.) SourceForge's
#      own "latest version" shortcut points at an unrelated file on this
#      project, and there's no reliable per-folder "latest" otherwise.
#   2. A multi-select menu when run with no args, instead of a yes/all
#      prompt -- pick several at once, comma-separated (e.g. 1,3,4). If
#      "all" is mixed into the same list as individual picks, that's a
#      conflict (all already includes everything the other picks would
#      have fetched) -- the script says so and asks yY/nN whether to just
#      do "all" instead, rather than silently double-fetching anything.
#      Content-level overlap between DIFFERENT packages (basedata and data
#      both ship Textures/Change, /Historical, /Overlay, /WAC_LOWSUN) is
#      still handled the way it always was: rsync --ignore-existing never
#      overwrites what an earlier run already placed.
#
# Usage:
#   tools/macos/get-virtmoon-data.sh                        # interactive menu (see below)
#   tools/macos/get-virtmoon-data.sh basedata                # the ~200 MB package -- (base functional application)
#   tools/macos/get-virtmoon-data.sh data                    # the ~1.9 GB package (real WAC, Clementine, picture libraries, ...)
#   tools/macos/get-virtmoon-data.sh picture                 # the picture-library add-on
#   tools/macos/get-virtmoon-data.sh artemis2                # the artemis2 add-on
#   tools/macos/get-virtmoon-data.sh translation             # extra language files
#   tools/macos/get-virtmoon-data.sh all                     # every package above, in order
#   tools/macos/get-virtmoon-data.sh basedata ~/some/other/dir   # override the target dir (2nd arg always the target)
#   tools/macos/get-virtmoon-data.sh -k data                 # -k/--keep: persist the downloaded .deb(s),
#   tools/macos/get-virtmoon-data.sh --keep=~/vma-debs all    #   default location $PWD, or give one with -k=DIR/--keep=DIR
#
# No args, interactive terminal: shows the menu below. No args, non-interactive
# (no tty): defaults to basedata, no prompt (same safe default as before).
#
# Never overwrites anything already in the target directory and strips any
# symlinks the upstream package ships (e.g. Textures/WAC -> WAC_LOWSUN)
# before merging in -- no symlinks in the data, anywhere, full stop.
#
# A .deb matching the resolved filename already sitting in $PWD (or, with
# -k/--keep, in the keep location) is used as-is instead of downloading
# again -- still checked against the feed's MD5 either way, so a stale or
# corrupt local copy gets caught, not silently extracted. Without -k, a
# freshly-downloaded .deb is still deleted after use (scratch dir only, as
# before) -- -k is what makes it persist for a future run to find.
set -euo pipefail

# Cleans up scratch dirs/temp files on any exit -- normal, error, Ctrl-C, or
# the interactive "q" quit below -- so stopping mid-download or mid-extract
# never leaves /tmp/vma-fetch.* or /tmp/vma-rss.*.xml behind. (A -k/--keep
# .deb is already copied to KEEP_DIR before this could ever run, so quitting
# never loses a kept download.)
cleanup() {
  # nullglob: an unmatched /tmp/vma-fetch.* (nothing to clean up) should
  # just expand to nothing, not make zsh error with "no matches found".
  setopt local_options nullglob
  rm -rf /tmp/vma-fetch.* 2>/dev/null
  rm -f /tmp/vma-rss.*.xml 2>/dev/null
}
trap cleanup EXIT INT TERM

# Also sweep on the way in, before this run creates its own temp files --
# SIGKILL (and a crash) skip the trap above, so a prior run's leftovers
# would otherwise just sit in /tmp forever.
cleanup

# Pull -k/--keep (and -k=DIR/--keep=DIR) out of the args before anything
# else looks at them, so the rest of the script sees the same positional
# args ($1=package or flag, $2=target dir) it always did.
KEEP_DIR=""
typeset -a REMAINING_ARGS
REMAINING_ARGS=()
for arg in "$@"; do
  case "$arg" in
    -k|--keep)      KEEP_DIR="$PWD" ;;
    -k=*)           KEEP_DIR="${arg#-k=}"; KEEP_DIR="${KEEP_DIR/#\~/$HOME}" ;;
    --keep=*)       KEEP_DIR="${arg#--keep=}"; KEEP_DIR="${KEEP_DIR/#\~/$HOME}" ;;
    *)              REMAINING_ARGS+=("$arg") ;;
  esac
done
set -- "${REMAINING_ARGS[@]}"

ALL_PKGS=(basedata data picture artemis2 translation)
NPKGS=${#ALL_PKGS[@]}
ALL_IDX=$((NPKGS + 1))
RSS_URL="https://sourceforge.net/projects/virtualmoon/rss"

prefix_for() {
  case "$1" in
    basedata)    echo "virtualmoon-basedata_" ;;
    data)        echo "virtualmoon-data_" ;;
    picture)     echo "virtualmoon-picture_" ;;
    artemis2)    echo "virtualmoon-artemis2_" ;;
    translation) echo "virtualmoon-translation_" ;;
    *)           echo "unknown package: $1 (try ${ALL_PKGS[*]}, or all)" >&2; exit 1 ;;
  esac
}

label_for() {
  case "$1" in
    basedata)    echo "base - functional application - minimum need to work" ;;
    data)        echo "real WAC, Clementine, picture libraries" ;;
    picture)     echo "picture-library add-on" ;;
    artemis2)    echo "artemis2 add-on" ;;
    translation) echo "extra language files" ;;
  esac
}

# Formats a byte count like "1.81 GB" / "189.5 MB" -- same convention used
# when we eyeballed these sizes by hand earlier (2026-10-12).
human_size() {
  local bytes="$1"
  if [ "$bytes" -ge $((1024 * 1024 * 1024)) ]; then
    printf "%.2f GB" "$(( bytes / 1024.0 / 1024.0 / 1024.0 ))"
  else
    printf "%.1f MB" "$(( bytes / 1024.0 / 1024.0 ))"
  fi
}

# Finds the newest .deb in the RSS feed whose filename starts with the given
# prefix, verifying it's actually a .deb (not the similarly-named .exe
# Windows installer, which uses a dash instead of an underscore after the
# package name). Prints: url<TAB>filesize_bytes<TAB>pubdate<TAB>md5
resolve_latest() {  # <feed-file> <prefix>
  python3 - "$1" "$2" <<'PY'
import sys, xml.etree.ElementTree as ET
from email.utils import parsedate_to_datetime

feed_path, prefix = sys.argv[1], sys.argv[2]
ns = {"media": "http://video.search.yahoo.com/mrss/"}
root = ET.parse(feed_path).getroot()

best = None
for item in root.iter("item"):
    title = (item.findtext("title") or "").strip()
    name = title.rsplit("/", 1)[-1]
    if not (name.startswith(prefix) and name.endswith(".deb")):
        continue
    link = (item.findtext("link") or "").strip()
    pub = item.findtext("pubDate")
    date = parsedate_to_datetime(pub) if pub else None
    media = item.find("media:content", ns)
    size = media.get("filesize") if media is not None else ""
    md5 = ""
    if media is not None:
        h = media.find("media:hash", ns)
        if h is not None:
            md5 = h.text or ""
    if best is None or (date and (best[0] is None or date > best[0])):
        best = (date, link, size, md5, name)

if best is None:
    sys.exit(1)
_, link, size, md5, name = best
print(f"{link}\t{size}\t{name}\t{md5}")
PY
}

fetch_and_merge() {  # <pkg> <target> <feed-file>
  local pkg="$1" target="$2" feed="$3"
  local prefix url size name md5 zipname scratch datatar src computed_md5
  local check_dir="${KEEP_DIR:-$PWD}"
  local use_zip=0

  prefix="$(prefix_for "$pkg")"
  if ! resolved="$(resolve_latest "$feed" "$prefix")"; then
    echo "no package found in the RSS feed matching '$prefix*' -- check tools/macos/get-virtmoon-data.sh's prefix_for()" >&2
    exit 1
  fi
  IFS=$'\t' read -r url size name md5 <<< "$resolved"
  zipname="${name%.deb}.zip"

  scratch="$(mktemp -d /tmp/vma-fetch.XXXXXX)"

  if [ -f "$check_dir/$zipname" ]; then
    echo "==> found $zipname in $check_dir -- using it as a pre-unpacked override (no download, no MD5 check)"
    use_zip=1
    mkdir -p "$scratch/payload"
    unzip -q "$check_dir/$zipname" -d "$scratch/payload"
  elif [ -f "$check_dir/$name" ]; then
    echo "==> found $name already in $check_dir -- using it instead of downloading"
    cp "$check_dir/$name" "$scratch/$name"
  else
    echo "==> downloading $name ($pkg, $(( size / 1024 / 1024 )) MB)"
    curl -L -C - -o "$scratch/$name" "$url"
  fi

  if [ "$use_zip" = 0 ]; then
    if [ -n "$md5" ]; then
      computed_md5="$(md5 -q "$scratch/$name")"
      if [ "$computed_md5" != "$md5" ]; then
        echo "MD5 mismatch for $name: expected $md5, got $computed_md5 -- not extracting" >&2
        exit 1
      fi
      echo "    MD5 OK"
    fi

    if [ -n "$KEEP_DIR" ] && [ ! -f "$KEEP_DIR/$name" ]; then
      mkdir -p "$KEEP_DIR"
      cp "$scratch/$name" "$KEEP_DIR/$name"
      echo "    kept: $KEEP_DIR/$name"
    fi

    echo "==> extracting"
    ( cd "$scratch" && ar x "$name" )

    datatar="$(ls "$scratch"/data.tar.* 2>/dev/null | head -1)"
    [ -n "$datatar" ] || { echo "no data.tar.* found after ar x -- bad download?" >&2; exit 1; }

    mkdir -p "$scratch/payload"
    # bsdtar auto-detects gz/xz/bz2/plain; a .tar.zst needs the zstd CLI on
    # PATH (brew install zstd if this errors with "unable to run program zstd").
    ( cd "$scratch/payload" && tar xf "$datatar" )

    src="$scratch/payload/usr/share/virtualmoon"
    [ -d "$src" ] || { echo "expected $src not found in $name" >&2; exit 1; }
  else
    src="$scratch/payload"
  fi

  echo "==> stripping any symlinks (no symlinks in the data, ever)"
  find "$src" -type l -print -delete

  echo "==> merging into: $target  (never overwrites anything already there)"
  mkdir -p "$target"
  rsync -a --ignore-existing "$src"/ "$target"/

  rm -rf "$scratch"
}

do_all() {  # <target> <feed-file>
  local target="$1" feed="$2"
  for pkg in "${ALL_PKGS[@]}"; do
    fetch_and_merge "$pkg" "$target" "$feed"
  done
}

show_menu() {  # <feed-file>
  local feed="$1"
  local pkg prefix resolved size sz_human total=0
  echo "Which data do you want? Enter one or more numbers, comma-separated"
  echo "(e.g. 1,3,4), or q to quit without downloading anything."
  local i=1
  for pkg in "${ALL_PKGS[@]}"; do
    prefix="$(prefix_for "$pkg")"
    sz_human="?"
    if resolved="$(resolve_latest "$feed" "$prefix" 2>/dev/null)"; then
      size="${resolved#*$'\t'}"; size="${size%%$'\t'*}"
      sz_human="$(human_size "$size")"
      total=$((total + size))
    fi
    echo "  $i) $pkg [$sz_human]   ($(label_for "$pkg"))"
    i=$((i + 1))
  done
  echo "  $i) all [$(human_size "$total")]   (everything above -- choose alone, not combined with other picks)"
}

# Parses a comma-separated list of menu numbers (e.g. "1,3,4") into the
# global array PKGS_TO_FETCH. If the "all" number is mixed in with other
# picks, that's a conflict -- confirm with the user (yY/nN) whether to just
# do "all" instead, since "all" already includes everything the other picks
# would have fetched individually.
resolve_choices() {  # <comma-separated choice string>
  local choice="${1:-}"
  [ -n "$choice" ] || choice="1"

  typeset -a picks
  picks=("${(@s/,/)choice}")
  local i p
  for i in {1..${#picks[@]}}; do
    picks[$i]="${picks[$i]// /}"
  done

  local has_all=0
  typeset -a valid_pkgs
  for p in "${picks[@]}"; do
    if [ "$p" = "$ALL_IDX" ]; then
      has_all=1
    elif [[ "$p" =~ ^[0-9]+$ ]] && [ "$p" -ge 1 ] && [ "$p" -le "$NPKGS" ]; then
      valid_pkgs+=("${ALL_PKGS[$p]}")
    else
      echo "ignoring unrecognized choice '$p'" >&2
    fi
  done

  if [ "$has_all" = 1 ] && [ "${#valid_pkgs[@]}" -gt 0 ]; then
    echo "$ALL_IDX) is 'all' -- that already includes ${valid_pkgs[*]}, so" >&2
    echo "combining it with individual picks doesn't do anything extra." >&2
    if [ -t 0 ]; then
      read "confirm?Just download $ALL_IDX) all instead? [yY/nN]: "
    else
      confirm="y"
    fi
    case "$confirm" in
      [yY]|"") PKGS_TO_FETCH=("${ALL_PKGS[@]}") ;;
      *) echo "Nothing downloaded -- re-run and choose again." >&2; exit 1 ;;
    esac
  elif [ "$has_all" = 1 ]; then
    PKGS_TO_FETCH=("${ALL_PKGS[@]}")
  elif [ "${#valid_pkgs[@]}" -eq 0 ]; then
    echo "no valid choices -- defaulting to basedata" >&2
    PKGS_TO_FETCH=(basedata)
  else
    typeset -A seen
    PKGS_TO_FETCH=()
    for p in "${valid_pkgs[@]}"; do
      if [ -z "${seen[$p]:-}" ]; then
        PKGS_TO_FETCH+=("$p")
        seen[$p]=1
      fi
    done
  fi
}

TARGET_DEFAULT="$HOME/Library/Application Support/Virtual Moon Atlas"

FEED="$(mktemp /tmp/vma-rss.XXXXXX.xml)"
curl -sL "$RSS_URL" -o "$FEED"

case "${1:-}" in
  -a|--all|all)
    TARGET="${2:-$TARGET_DEFAULT}"
    do_all "$TARGET" "$FEED"
    ;;
  "")
    TARGET="$TARGET_DEFAULT"
    if [ -t 0 ]; then
      show_menu "$FEED"
      read "choice?Enter choice(s) [1]: "
    else
      choice=""
    fi
    case "$choice" in
      q|Q) echo "Quitting -- nothing downloaded." >&2; exit 0 ;;
    esac
    resolve_choices "$choice"
    for PKG in "${PKGS_TO_FETCH[@]}"; do
      fetch_and_merge "$PKG" "$TARGET" "$FEED"
    done
    ;;
  *)
    PKG="$1"
    TARGET="${2:-$TARGET_DEFAULT}"
    fetch_and_merge "$PKG" "$TARGET" "$FEED"
    ;;
esac

rm -f "$FEED"
echo "==> done"
du -sh "${TARGET:-$TARGET_DEFAULT}"
