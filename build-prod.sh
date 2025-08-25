#!/bin/sh
set -e

# --- paths & files ------------------------------------------------------------
js="main.js"
min="main.min.js"
template="index.html"           # HTML template containing <script src="main.js">
html="dm6-elm.html"             # final standalone output
tools_js_src="scripts/localstorage-tools.js"

logfile="src/Log.elm"
backup="$logfile.bak"

# --- flip Log.elm to PROD (no Debug) -----------------------------------------
set -euo pipefail

logfile="src/Log.elm"

# Ensure we're in a git worktree (so we can restore cleanly later)
in_git() { git rev-parse --is-inside-work-tree >/dev/null 2>&1; }

if ! in_git; then
  echo "ERROR: build-prod.sh expects to run inside a git worktree." >&2
  exit 1
fi

# We rely on HEAD having the DEV version for src/Log.elm.
# Keep both variants in the repo:
#   - src/Log.dev.elm  (dev logger)
#   - src/Log.prod.elm (no-op prod logger)
if [ ! -f src/Log.prod.elm ] || [ ! -f src/Log.dev.elm ]; then
  echo "ERROR: Missing src/Log.prod.elm or src/Log.dev.elm." >&2
  exit 1
fi

# Safety net: always restore Log.elm back to HEAD (worktree + index) on exit.
restore_log() {
  # Discard any changes to src/Log.elm in both index and worktree
  git restore --staged --worktree -- "$logfile" 2>/dev/null || \
  git checkout -- "$logfile" 2>/dev/null || true
}
trap restore_log EXIT

# 1) Put PROD logger in place for the build
cp -f src/Log.prod.elm "$logfile"

# (optional) show what changed
# git -c color.ui=always diff -- "$logfile" | sed -n '1,80p'

# --- build & minify -----------------------------------------------------------
elm make src/Main.elm --optimize --output="$js"

uglifyjs "$js" \
  --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" \
  | uglifyjs --mangle --output "$min"

echo "Initial size: $(wc -c < "$js") bytes ($js)"
echo "Minified size: $(wc -c < "$min") bytes ($min)"

# --- IMPORTANT: restore src/Log.elm now (so your dev server sees the original) ---
restore_log

# (keep the EXIT trap; it will no-op if already restored)

# --- prep inline payloads -----------------------------------------------------
# Escape any literal </script> so inline blocks are safe
tmp_js_escaped="$(mktemp)"
sed 's#</script>#<\\/script>#g' "$min" > "$tmp_js_escaped"

tmp_tools_escaped="$(mktemp)"
if [ -f "$tools_js_src" ]; then
  sed 's#</script>#<\\/script>#g' "$tools_js_src" > "$tmp_tools_escaped"
else
  echo "ERROR: $tools_js_src not found" >&2
  rm -f "$tmp_js_escaped"
  exit 1
fi

# --- transform template -> standalone HTML -----------------------------------
awk -v jsfile="$tmp_js_escaped" -v toolsfile="$tmp_tools_escaped" '
  BEGIN {
    while ((getline l < jsfile)    > 0) js = js l ORS;     close(jsfile)
    while ((getline t < toolsfile) > 0) tools = tools t ORS; close(toolsfile)
  }
  {
    # Replace <script src="main.js">… with inline bundle
    if ($0 ~ /<script[[:space:]]+src=["'\''"]main\.js["'\''"][^>]*><\/script>[[:space:]]*$/ \
     || $0 ~ /<script[[:space:]]+src=["'\''"]main\.js["'\''"][^>]*>[[:space:]]*$/) {
      print "  <script>"
      printf "%s", js
      print "  </script>"
      replaced = 1
      next
    }

    # Inject toolbar + tools before </body>, pinned bottom-right
    if ($0 ~ /<\/body>/ && !injected) {
      print "  <div id=\"dm6-dev-tools\""
      print "       style=\"position:fixed;bottom:10px;right:10px;"
      print "              display:flex;gap:.5em;z-index:9999;"
      print "              background:#fff;border:1px solid #ddd;border-radius:8px;"
      print "              padding:.4em .6em;box-shadow:0 2px 10px rgba(0,0,0,.08);\">"
      print "    <button type=\"button\" onclick='\''exportLS()'\'' title=\"Export model\">⤓ Export</button>"
      print "    <button type=\"button\" onclick='\''importLSFile()'\'' title=\"Import model\">📂 Import</button>"
      print "  </div>"
      print "  <script>"
      printf "%s", tools
      print "  </script>"
      injected = 1
      print
      next
    }

    print
  }
  END {
    if (!replaced)  { print "ERROR: Could not find <script src=\"main.js\"> in template to inline." > "/dev/stderr"; exit 42 }
    if (!injected)  { print "ERROR: Could not inject tools (no </body> seen)." > "/dev/stderr"; exit 43 }
  }
' "$template" > "$html"

rm -f "$tmp_js_escaped" "$tmp_tools_escaped"

# --- sanity & sizes -----------------------------------------------------------
grep -q "</script>" "$html" || { echo "ERROR: Missing </script> in $html" >&2; exit 1; }
printf "Standalone written to: %s\n" "$html"
printf "Size: %s bytes (gzipped: %s bytes)\n" "$(wc -c < "$html")" "$(gzip -c "$html" | wc -c)"

# --- auto-open in browser -----------------------------------------------------
if command -v open >/dev/null 2>&1; then
  open "$html"
elif command -v xdg-open >/dev/null 2>&1; then
  xdg-open "$html"
else
  echo "Open $html manually in your browser."
fi
