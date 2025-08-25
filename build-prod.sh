#!/bin/sh
set -e

js="main.js"
min="main.min.js"
html="dm6-elm.html"
logfile="src/Log.elm"
backup="$logfile.bak"

# --- flip Log.elm to PROD (no Debug) -----------------------------------------
cp "$logfile" "$backup"

restore_log() {
  mv -f "$backup" "$logfile" 2>/dev/null || true
}
trap restore_log EXIT

# Comment import Debug (if present)
sed -i.bak 's/^[[:space:]]*import[[:space:]]\+Debug/-- import Debug/' "$logfile"

# Comment the Debug implementation
#   log =
#       Debug.log
# → -- log =
#   --     Debug.log
sed -i.bak 's/^[[:space:]]*log[[:space:]]*=[[:space:]]*$/-- log =/' "$logfile"
sed -i.bak 's/^[[:space:]]*Debug\.log/--     Debug.log/' "$logfile"

# Uncomment the no-op lines:
#   -- log _ x =
#   --     x
# → log _ x =
#   x
sed -i.bak 's/^[[:space:]]*--[[:space:]]*log[[:space:]]*_[[:space:]]*x[[:space:]]*=/log _ x =/' "$logfile"
sed -i.bak 's/^[[:space:]]*--[[:space:]]*x[[:space:]]*$/    x/' "$logfile"

# Clean sed temp
rm -f "$logfile.bak"

# --- build -------------------------------------------------------------------
elm make src/Main.elm --optimize --output="$js"

uglifyjs "$js" \
  --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" \
  | uglifyjs --mangle --output "$min"

echo "Initial size: $(wc -c < "$js") bytes ($js)"
echo "Minified size:$(wc -c < "$min") bytes ($min)"

./inline-script.js

echo "Standalone:   $(wc -c < "$html") bytes ($html) (gzipped:$(gzip -c "$html" | wc -c) bytes)\n"
