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