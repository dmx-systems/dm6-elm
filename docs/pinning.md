
# DM6 Elm — Pinned Visibility (Per-Map)

This document explains the new **`pinned : Bool`** flag on `MapItem` and how it changes visibility behavior during **Boxing** (BlackBox/WhiteBox) and **Unboxing**. It also provides demo steps you can follow in the running dev server.

---

## What is `pinned`?

Each map-specific item (`MapItem`) now has two visibility dimensions:

- `hidden : Bool` — whether the item is rendered in that map.
- `pinned : Bool` — whether the item is **protected from auto-hiding** by Boxing in that map.

This separation fixes cases where topics *unexpectedly disappeared* from the top map after re-boxing, even though the user had explicitly revealed them there.

---

## Rules

- **Reveal (explicit user intent):** when a user reveals an item in a map (e.g., Search → click), the item becomes `hidden=False` and **`pinned=True`** in that map.
- **Boxing (BlackBox/WhiteBox):** auto-hides only **non-pinned** items in the parent map (`pinned=False`). **Pinned items remain visible**.
- **Unboxing:** reveals items as before. (Optional rule: if an item was already visible in the parent map during unbox, keep/set `pinned=True` there.)
- **Hide:** when the user explicitly hides an item, it becomes `hidden=True` and **`pinned=False`** in that map (the protection is removed).

> Persistence: `pinned` is serialized in `Storage.elm`. Older stored models default `pinned=False` on load.

---

## Mental model

```
Per-map MapItem state: [hidden] / [pinned]

[hidden=False, pinned=True]   → visible AND protected from Boxing in this map
[hidden=False, pinned=False]  → visible but may be auto-hidden by Boxing later
[hidden=True,  pinned=False]  → not visible
```

**Boxing rule:** Only items with `pinned=False` are auto-hidden in the parent map.  
**Hide rule:** Hiding an item also clears `pinned` (set to False).

---

## Demo scenarios

> Run the dev server (`npm run dev`) and open http://localhost:8000.  
> Use **Search** to reveal topics; use **Display** to switch containers between WhiteBox/BlackBox/Unboxed.

### 1) Reveal → Box (fixed disappearance)

1. In the **top map**, use **Search** to find a topic not currently visible and click it (Reveal).
2. Switch a related container to **BlackBox** and back to **WhiteBox** a few times.
3. **Expected:** the revealed topic **stays visible** in the top map.  
   (It is now `pinned=True` and Boxing skips it.)

### 2) Already visible in parent → Unbox → Box

1. Ensure **Topic A** is visible in the top map.
2. Unbox a container that also contains **Topic A** (WhiteBox → Unboxed).
3. Box it again (Unboxed → WhiteBox/BlackBox).
4. **Expected:** Topic A remains visible in the top map.  
   (Either it was previously pinned by Reveal, or the unboxing path preserved pin.)

### 3) Hide overrides pin

1. Pick a topic visible in top map (pinned).
2. Use **Hide**.
3. **Expected:** the topic disappears and becomes eligible for auto-hiding (state `[hidden=True, pinned=False]`).  
   Re-boxing later will not preserve it.

---

## Code changes summary

- `src/Model.elm`
  - `type alias MapItem = { id, hidden, props, parentAssocId, pinned }`
  - `addItemToMap` now constructs `MapItem … False` (pinned default False).
  - `hideItem_` sets `{ hidden = True, pinned = False }` on hide.

- `src/Storage.elm`
  - Encoder/decoder include `pinned` (decoder defaults to False for older saves).

- `src/Boxing.elm` *(Step 2)*
  - When **boxing**, skip hiding items where `viewItem.pinned == True`.
  - `targetAssocItem` constructs with `… False` for pinned.

- `src/Search.elm` *(Step 3)*
  - On **reveal**, set `pinned=True` in that map (explicit intent).

> If you are rolling changes incrementally, ensure Steps 2 and 3 are applied to see the behavior above.

---

## Seeding a demo model

You can seed localStorage with a tiny model that shows a single topic on the top map.

### Console one-liner

Open the app, DevTools → Console, then paste:

```js
(function () {
  var KEY = 'dm6-elm-model';
  var value = {
    "items": {
      "1": { "topic": { "id": 1, "text": "Ralf Barkow", "iconName": "user" } },
      "2": { "assoc": { "id": 2, "itemType": "dmx.composition", "player1": 1, "role1": "dmx.child", "player2": 0, "role2": "dmx.parent" } }
    },
    "maps": {
      "0": {
        "id": 0,
        "items": {
          "1": {
            "id": 1,
            "hidden": false,
            "topicProps": {
              "pos": { "x": 372, "y": 277 },
              "size": { "w": 128, "h": 37.5 },
              "displayMode": "Detail"
            },
            "parentAssocId": 2
          }
        },
        "rect": { "x1": 0, "y1": 0, "x2": 0, "y2": 0 },
        "parentMapId": -1
      }
    },
    "mapPath": [0],
    "nextId": 3
  };
  localStorage.setItem(KEY, JSON.stringify(value));
  location.reload();
})();
```

### Helper scripts

To export the current model from localStorage, use the helper in `scripts/localstorage-tools.js`.

Usage:
```js
exportLS('dm6-elm-model')
importLSFile('dm6-elm-model')
```
---

## Example resource

An example model JSON file is included in the repo:
examples/dm6-elm-model.json

You can import it via the browser console using the helper:

```js
importLSFile('dm6-elm-model')
```

Or export your own edits with:
```js
exportLS('dm6-elm-model')
```

This makes it easy to share or restore pinned-visibility demo states.

---

## FAQ

**Q:** Why not just rely on `hidden`?  
**A:** Because `hidden` is about *visibility*, not *intent*. `pinned` captures intent (“keep this visible here”) so Boxing does not undo explicit reveals.

**Q:** Does `pinned` affect child maps?  
**A:** No, it is **per map**. Pinning a topic in the top map does not pin it elsewhere.

**Q:** Is `pinned` persisted?  
**A:** Yes. Older saves default `pinned=False` when decoded.

---

Happy mapping!
