[…] connect the failed “cross” behavior to a missing selection. Here’s what upstream’s change does and why it matters for Cross:

### What upstream changed

* They added `resetSelection : Model -> Model` in `ModelAPI`, and started using it in **`hide`** and **`delete`**:

  ```elm
  newModel
      |> autoSize
      |> resetSelection
  ```

  So after hiding or deleting, the model’s `selection` is explicitly cleared.

### Why that breaks Cross (and a couple tests)

* **Feature.Cross** (and the toolbar “Cross” button) depend on there being **exactly one selection**.

  * If there’s **no selection**, the button isn’t rendered/enabled and the Cross flow returns something like `Cross: { reason = "no selection" }` (you saw that in the logs).

* Your logs also show `@autoSize: []`. That’s because the **new `MapAutoSize`** uses `getSingleSelection` to find the path to resize. With **no selection**, it finds no paths, and prints `[]`.

Put together:

1. An earlier step in the test or flow (often a hide/delete or a flow that calls those) clears the selection via `resetSelection`.

2. When Cross tries to run, there is **no active selection**, so:

   * The toolbar **doesn’t show** a Cross button → `Feature.OpenDoor.ButtonTest` can’t find `#btn-Cross`.
   * The Cross request bails → `ℹ️ Cross: { reason = "no selection" }`.
   * `autoSize` has nothing to anchor its path-calculation on → `@autoSize: []`.

### What’s **not** caused by `resetSelection`

* The OpenDoor “move into/out of container” failures you saw earlier were primarily due to the `moveTopicToMap` refactor (map id/props lookup changed) — that’s separate. But Cross failing due to “no selection” is directly tied to the new selection clearing.

### How to make Cross work again (minimal options)

Pick any one (or a combo) that fits your intent:

1. **Re-establish selection before Cross**

   * Wherever you trigger Cross (in tests or UI flow), ensure you call `ModelAPI.select` right before, so `getSingleSelection` sees something.

2. **Don’t clear selection for ops that will immediately need it**

   * Move `resetSelection` later (or conditionally) if a subsequent action depends on selection.
   * For example, keep `autoSize` but delay `resetSelection` until after Cross-related actions complete.

3. **Make Cross independent of selection**

   * Change `Feature.Cross` (or your `Channel.cross` entrypoint) to take explicit `topicId`/`mapPath` from the event rather than reading `getSingleSelection`. That way it can operate even if selection is empty.

4. **Re-select after Cross/move**

   * After performing the cross/move, explicitly `select` the relevant item (moved topic or target container) before calling `autoSize`, so follow-up autosizes have a path anchor (and the button reappears if tests inspect the toolbar again).

In short: upstream’s `resetSelection` is the reason your Cross button vanishes and the Cross flow says “no selection.” Either reintroduce (or delay) a selection at the right moment, or make Cross not depend on selection.

  –– ChatGPT 5