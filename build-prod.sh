#!/bin/sh
set -euo pipefail

# --- paths & files ------------------------------------------------------------
js="main.js"
min="main.min.js"
template="index.html"           # HTML template containing <script src="main.js">
html="dm6-elm.html"             # final standalone output
tools_js_src="scripts/localstorage-tools.js"

log_dst="src/Log.elm"
log_prod_src="src/Log/Prod.elm"
log_dev_src="src/Log/Dev.elm"   # just to assert it exists

# --- guards -------------------------------------------------------------------
in_git() { git rev-parse --is-inside-work-tree >/dev/null 2>&1; }

if ! in_git; then
  echo "ERROR: build-prod.sh expects to run inside a git worktree." >&2
  exit 1
fi

[ -f "$log_prod_src" ] || { echo "ERROR: $log_prod_src missing." >&2; exit 1; }
[ -f "$log_dev_src" ]  || { echo "ERROR: $log_dev_src missing."  >&2; exit 1; }
[ -f "$template" ]     || { echo "ERROR: $template missing."     >&2; exit 1; }
command -v uglifyjs >/dev/null 2>&1 || { echo "ERROR: uglify-js not found (npm i -D uglify-js)." >&2; exit 1; }

# --- restore helper -----------------------------------------------------------
restore_file() {
  # Try to restore from git; if not tracked (or restore fails), fall back to Dev copy.
  local f="$1"
  git restore --staged --worktree -- "$f" >/dev/null 2>&1 || \
  git checkout -- "$f"               >/dev/null 2>&1 || \
  { [ -f "$log_dev_src" ] && cp -f "$log_dev_src" "$f"; } || true
}

cleanup() {
  # Never fail during cleanup
  set +e
  restore_file "$log_dst"
  set -e
}
trap cleanup EXIT

# --- put PROD logger in place -------------------------------------------------
cp -f "$log_prod_src" "$log_dst"

# Ensure module header matches file path: "module Log exposing (...)"
# (Accepts any exposing list, preserves it.)
awk 'NR==1 {
        if ($0 ~ /^module[[:space:]]+Log\.Prod[[:space:]]+exposing[[:space:]]*\(.*\)/) {
            sub(/^module[[:space:]]+Log\.Prod/, "module Log")
        } else if ($0 !~ /^module[[:space:]]+Log[[:space:]]+exposing[[:space:]]*\(.*\)/) {
            # if someone changed the header to something else, force it
            sub(/^module[[:space:]]+.*/, "module Log exposing (debug, info, warn, withConsole, log)")
        }
     } { print }' "$log_dst" > "$log_dst.tmp" && mv "$log_dst.tmp" "$log_dst"

# Optional: ensure old imports that used `log` still work by defining alias.
# (No-op if already present.)
grep -q '^log[[:space:]]*:' "$log_dst" 2>/dev/null || cat >> "$log_dst" <<'EOF'

-- Back-compat alias (older code imports `Log.log`)
log : String -> a -> a
log =
    debug
EOF

# --- build & minify -----------------------------------------------------------
elm make src/Main.elm --optimize --output="$js"

uglifyjs "$js" \
  --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" \
  | uglifyjs --mangle --output "$min"

echo "Initial size: $(wc -c < "$js") bytes ($js)"
echo "Minified size: $(wc -c < "$min") bytes ($min)"

# --- inline bundle into template ---------------------------------------------
tmp_js_escaped="$(mktemp)"
sed 's#</script>#<\\/script>#g' "$min" > "$tmp_js_escaped"

tmp_tools_escaped="$(mktemp)"
sed 's#</script>#<\\/script>#g' "$tools_js_src" > "$tmp_tools_escaped"

awk -v jsfile="$tmp_js_escaped" -v toolsfile="$tmp_tools_escaped" '
  BEGIN {
    while ((getline l < jsfile)    > 0) js = js l ORS;     close(jsfile)
    while ((getline t < toolsfile) > 0) tools = tools t ORS; close(toolsfile)
  }
  {
    if ($0 ~ /<script[[:space:]]+src=["'\''"]main\.js["'\''"][^>]*><\/script>[[:space:]]*$/ \
     || $0 ~ /<script[[:space:]]+src=["'\''"]main\.js["'\''"][^>]*>[[:space:]]*$/) {
      print "  <script>"
      printf "%s", js
      print "  </script>"
      replaced = 1
      next
    }

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

      # >>> add this block to subscribe to Console.log port in PROD output <<<
      print "  <script>"
      print "    (function(){"
      print "      try {"
      print "        if (window.app && window.app.ports && window.app.ports.log && window.app.ports.log.subscribe) {"
      print "          window.app.ports.log.subscribe(function(line){ console.log(line); });"
      print "        }"
      print "      } catch (e) { /* ignore */ }"
      print "    })();"
      print "  </script>"
      # <<< end added block >>>

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

grep -q "</script>" "$html" || { echo "ERROR: Missing </script> in $html" >&2; exit 1; }
printf "Standalone written to: %s\n" "$html"
printf "Size: %s bytes (gzipped: %s bytes)\n" "$(wc -c < "$html")" "$(gzip -c "$html" | wc -c)"

# --- open in browser ----------------------------------------------------------
if command -v open >/dev/null 2>&1; then
  open "$html"
elif command -v xdg-open >/dev/null 2>&1; then
  xdg-open "$html"
else
  echo "Open $html manually in your browser."
fi
