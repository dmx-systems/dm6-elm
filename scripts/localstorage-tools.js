// scripts/localstorage-tools.js
// Utilities to export and import DM6 Elm model to/from localStorage
// Safari-friendly (no default params, no template strings, no arrow functions)

let PROD_KEY = 'dm6-elm';
let DEV_KEY  = 'dm6-elm-model';

function _currentKey() {
  if (localStorage.getItem(PROD_KEY) != null) return PROD_KEY;
  if (localStorage.getItem(DEV_KEY)  != null) return DEV_KEY;
  return PROD_KEY; // default
}

function _otherKey(key) {
  return key === PROD_KEY ? DEV_KEY : PROD_KEY;
}

/**
 * Export a localStorage key to a JSON file.
 * - If key is omitted/undefined, picks whichever key exists (_currentKey()).
 * - If requested key is missing, automatically falls back to the other key.
 *   (and logs which one it used)
 *
 * Usage:
 *   exportLS();             // auto-pick
 *   exportLS('dm6-elm');    // explicit
 *   exportLS('dm6-elm-model');
 */
function exportLS(key) {
  if (typeof key === 'undefined' || key === null) key = _currentKey();

  let value = localStorage.getItem(key);
  if (value == null) {
    let alt = _otherKey(key);
    let altValue = localStorage.getItem(alt);
    if (altValue == null) {
      console.warn('No "' + key + '" or "' + alt + '" in localStorage.');
      return;
    }
    console.warn('"' + key + '" missing; exporting "' + alt + '" instead.');
    key = alt;
    value = altValue;
  }

  let data = {}; data[key] = value;

  let blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
  let url  = URL.createObjectURL(blob);
  let a    = document.createElement('a');
  a.href     = url;
  a.download = key + '.json';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);

  console.log('✅ Exported "' + key + '" → ' + key + '.json');
}

/**
 * Import JSON into localStorage from a file picker.
 * Accepts either "dm6-elm" (prod) or "dm6-elm-model" (dev).
 *
 * Usage:
 *   importLSFile();     // toolbar / user click (no fallback button)
 *   importLSFile(true); // allow fallback button if picker blocked
 */
function importLSFile(fallback) {
  if (typeof fallback === 'undefined') fallback = false;

  function _setLS(k, v) {
    localStorage.setItem(k, typeof v === 'string' ? v : JSON.stringify(v));
  }

  function _doImport(file) {
    file.text().then(function (text) {
      let data;
      try {
        data = JSON.parse(text);
      } catch (e) {
        console.error('❌ Failed to parse JSON:', e);
        alert('Import failed. See console for details.');
        return;
      }

      let foundKey = null;
      if (Object.hasOwn(data, PROD_KEY)) foundKey = PROD_KEY;
      else if (Object.hasOwn(data, DEV_KEY)) foundKey = DEV_KEY;

      if (!foundKey) {
        console.error('❌ JSON must contain either "' + PROD_KEY + '" or "' + DEV_KEY + '"');
        return;
      }

      _setLS(foundKey, data[foundKey]);
      console.log('✅ Imported "' + foundKey + '" from ' + file.name);

      if (window.confirm('Reload page to apply imported model?')) {
        window.location.reload();
      }
    });
  }

  function _pickViaInput() {
  return new Promise((resolve) => {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.json,application/json';

    input.onchange = (e) => {
      const file = e.target?.files?.[0] ?? null;
      resolve(file);
    };

    document.body.appendChild(input);
    input.click();

    const cleanup = () => {
      input.remove();
    };

    input.addEventListener('change', cleanup, { once: true });
    window.addEventListener('focus', () => setTimeout(cleanup, 0), { once: true });
  });
}


  // Suppress fallback by default (toolbar is a user gesture)
  let changed = !fallback;

  let picking = _pickViaInput();
  picking.then(function (file) {
    changed = true;
    if (file) _doImport(file);
  });

  if (fallback) {
    setTimeout(function () {
      if (changed) return;

      let BTN_ID = '__dm6_import_once__';
      if (document.getElementById(BTN_ID)) return;

      let btn = document.createElement('button');
      btn.id = BTN_ID;
      btn.textContent = '📂 Pick JSON to import';
      btn.title = 'Click to import JSON into localStorage';

      let style = btn.style;
      style.position = 'fixed';
      style.bottom = '1em';
      style.right = '1em';
      style.padding = '0.6em 1em';
      style.font = '14px system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif';
      style.zIndex = 9999;
      style.border = '1px solid #ccc';
      style.borderRadius = '6px';
      style.background = '#fff';
      style.cursor = 'pointer';
      style.boxShadow = '0 2px 8px rgba(0,0,0,.08)';

      btn.onclick = function () {
        _pickViaInput().then(function (file) {
          if (file) _doImport(file);
          btn.remove();
        });
      };

      document.body.appendChild(btn);
      console.warn('ℹ️ Browser blocked programmatic picker. Click the “📂 Pick JSON to import” button that appeared.');
    }, 700);
  }
}

// Expose globally for toolbar & devtools
window.exportLS = exportLS;
window.importLSFile = importLSFile;
