const PROD_KEY = 'dm6-elm';
const DEV_KEY  = 'dm6-elm-model';

function isLikelyDmx(obj) {
  return obj && typeof obj === 'object'
    && typeof obj.id === 'number'
    && typeof obj.typeUri === 'string';
}

function isLikelyDm6(obj) {
  return obj && typeof obj === 'object'
    && ('items' in obj || 'maps' in obj) && 'mapPath' in obj && 'nextId' in obj;
}

function importLSFile(fallback) {
  if (typeof fallback === 'undefined') fallback = false;

  function _setLS(k, v) {
    localStorage.setItem(k, typeof v === 'string' ? v : JSON.stringify(v));
  }

  function _doImport(file) {
    file.text().then(function (text) {
      let data;
      try { data = JSON.parse(text); }
      catch (e) {
        console.error('❌ Failed to parse JSON:', e);
        alert('Import failed. See console for details.');
        return;
      }

      if (Object.hasOwn(data, PROD_KEY) || Object.hasOwn(data, DEV_KEY)) {
        const key = Object.hasOwn(data, PROD_KEY) ? PROD_KEY : DEV_KEY;
        _setLS(key, data[key]);
        console.log('✅ Imported wrapper "%s" from %s', key, file.name);
      } else if (isLikelyDm6(data)) {
        _setLS(DEV_KEY, data);
        console.log('✅ Imported raw DM6 blob into "%s" from %s', DEV_KEY, file.name);
      } else if (isLikelyDmx(data)) {
        _setLS(DEV_KEY, data); // Elm modelDecoder will take the DMX branch
        console.log('✅ Imported raw DMX blob into "%s" from %s', DEV_KEY, file.name);
      } else {
        console.error('❌ Unrecognized JSON shape. Expect { "%s" | "%s" } or raw DM6/DMX.', PROD_KEY, DEV_KEY);
        alert('Import failed. See console for details.');
        return;
      }

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

      const cleanup = () => input.remove();
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
      console.warn('ℹ️ Browser blocked the picker. Click the button to import JSON.');
    }, 700);
  }
}

// Expose globally for toolbar & DevTools
window.importLSFile = importLSFile;
