// scripts/localstorage-tools.js
// Utilities to export and import DM6 Elm model to/from localStorage

/**
 * Export a localStorage key to a JSON file.
 * Usage:
 *   exportLS('dm6-elm-model')
 */
function exportLS(key) {
  const value = localStorage.getItem(key);
  if (value == null) {
    console.warn(`No “${key}” in localStorage.`);
    return;
  }

  const data = { [key]: value };
  const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement('a');
  a.href     = url;
  a.download = `${key}.json`;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);

  console.log(`✅ Exported “${key}” → ${key}.json`);
}

/**
 * Import JSON into localStorage from file picker.
 * Works around browser restrictions by falling back to a button if needed.
 * Usage:
 *   importLSFile('dm6-elm-model')
 */
function importLSFile(expectedKey = 'dm6-elm-model') {
  const _setLS = (k, v) =>
    localStorage.setItem(k, typeof v === 'string' ? v : JSON.stringify(v));

  const _doImport = async (file) => {
    const text = await file.text();
    const data = JSON.parse(text);

    if (!(expectedKey in data)) {
      console.error(`❌ JSON does not contain key "${expectedKey}"`);
      return;
    }

    const val = data[expectedKey];
    _setLS(expectedKey, val);
    console.log(`✅ Imported "${expectedKey}" from ${file.name}`);
    if (confirm('Reload page to apply imported model?')) location.reload();
  };

  const _pickViaInput = () => new Promise((resolve) => {
    const input = Object.assign(document.createElement('input'), {
      type: 'file',
      accept: '.json,application/json'
    });
    input.onchange = (e) => resolve(e.target.files && e.target.files[0]);
    document.body.appendChild(input);
    input.click();
    const cleanup = () => { if (input && input.parentNode) input.parentNode.removeChild(input); };
    input.addEventListener('change', cleanup, { once: true });
  });

  let changed = false;
  _pickViaInput().then(file => {
    changed = true;
    if (file) _doImport(file);
  });

  setTimeout(() => {
    if (changed) return;
    const BTN_ID = '__dm6_import_once__';
    if (document.getElementById(BTN_ID)) return;

    const btn = Object.assign(document.createElement('button'), {
      id: BTN_ID,
      textContent: '📂 Pick JSON to import',
      title: 'Click to import JSON into localStorage'
    });
    Object.assign(btn.style, {
      position: 'fixed',
      bottom: '1em',
      right: '1em',
      padding: '0.6em 1em',
      font: '14px system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif',
      zIndex: 9999,
      border: '1px solid #ccc',
      borderRadius: '6px',
      background: '#fff',
      cursor: 'pointer'
    });

    btn.onclick = async () => {
      const file = await _pickViaInput();
      if (file) await _doImport(file);
      btn.remove();
    };

    document.body.appendChild(btn);
    console.warn('ℹ️ Browser blocked programmatic picker. Click the “📂 Pick JSON to import” button that appeared.');
  }, 700);
}

// Expose globally for devtools usage
window.exportLS = exportLS;
window.importLSFile = importLSFile;
