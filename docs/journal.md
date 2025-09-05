Love this direction. Here’s a compact, “add-and-see-it” plan to get a **meta view of moves inside DM6**, with a tiny in-app **Journal** (list + mini path sketch), plus hooks to **record explicit `topicId` / `mapPath` from the event**, and a foundation for **replay/undo**.

---

# 1) Data: add a lightweight Journal

### `src/Journal.elm`

```elm
module Journal exposing (Entry(..), Path, record, viewList, viewSketch)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (Id, MapId, Point)
import ModelAPI exposing (MapPath)

type alias Path =
    { selection : Maybe ( Id, MapPath )  -- what UI focused on (anchor)
    , computed  : List MapPath           -- what findPaths returned
    }

type Entry
    = CrossIn
        { topicId : Id, from : MapId, to : MapId, pos : Point, path : Path, note : String }
    | CrossOut
        { topicId : Id, from : MapId, to : MapId, pos : Point, path : Path, note : String }
    | CrossNoop { reason : String, topicId : Id, parent : MapId }
    | ErrorText String

record : Entry -> List Entry -> List Entry
record e acc =
    e :: acc |> List.take 200  -- keep it bounded

viewList : List Entry -> Html msg
viewList entries =
    div [ style "font-family" "monospace", style "font-size" "12px"
        , style "max-height" "220px", style "overflow" "auto"
        , style "border" "1px solid #ddd", style "padding" "6px" ]
        (entries |> List.map viewLine)

viewLine : Entry -> Html msg
viewLine e =
    case e of
        CrossIn r ->
            div [] [ text <| "IN: topic " ++ String.fromInt r.topicId
                         ++ " " ++ fromTo r.from r.to
                         ++ " pos=" ++ showPt r.pos
                         ++ note r.note ]

        CrossOut r ->
            div [] [ text <| "OUT: topic " ++ String.fromInt r.topicId
                         ++ " " ++ fromTo r.from r.to
                         ++ " pos=" ++ showPt r.pos
                         ++ note r.note ]

        CrossNoop r ->
            div [] [ text <| "NO-OP: topic " ++ String.fromInt r.topicId
                         ++ " parent=" ++ String.fromInt r.parent
                         ++ " reason=" ++ r.reason ]

        ErrorText s ->
            div [ style "color" "#a00" ] [ text ("ERR: " ++ s) ]

fromTo : MapId -> MapId -> String
fromTo a b = "(from " ++ String.fromInt a ++ " → " ++ String.fromInt b ++ ")"

showPt : Point -> String
showPt p = "(" ++ String.fromFloat p.x ++ "," ++ String.fromFloat p.y ++ ")"

note : String -> String
note s = if s == "" then "" else "  · " ++ s

-- very small, schematic path sketch: each MapPath becomes a dot row
viewSketch : List Entry -> Html msg
viewSketch entries =
    let
        dots =
            entries
                |> List.take 1 -- sketch most recent only (cheap & legible)
                |> List.concatMap
                    (\e ->
                        case e of
                            CrossIn r  -> r.path.computed
                            CrossOut r -> r.path.computed
                            _          -> []
                    )
    in
    svg [ width "160", height "60", viewBox "0 0 160 60", style "border" "1px solid #eee" ]
        (dots
            |> List.indexedMap
                (\row path ->
                    let
                        y = 20 + toFloat row * 16
                        xs = List.indexedMap (\i _ -> 10 + toFloat i * 18) path
                    in
                    List.concat
                        [ [ Svg.text_ [ x "4", y (String.fromFloat (y - 7)), fontSize "8" ] [ Svg.text "path" ] ]
                        , xs
                            |> List.map
                                (\xv -> circle [ cx (String.fromFloat xv), cy (String.fromFloat y), r "3", fill "#333" ] [])
                        ]
                )
            |> List.concat
        )
```

### Extend `AppModel.Model`

Add a journal buffer and a dev overlay toggle:

```elm
-- in AppModel.elm (or Model.elm if that’s your core)
type alias Model =
    { ... -- existing fields
    , journal : List Journal.Entry
    , showJournal : Bool
    }
```

Initialize to:

```elm
initModel =
    { ... -- existing
    , journal = []
    , showJournal = True  -- default on; you can wire it to a toolbar toggle later
    }
```

---

# 2) Recording from Channel/Update (no selection needed)

**Key idea**: Cross doesn’t rely on `selection`. We *explicitly* record the `topicId`, computed `fromMap`/`toMap`, drop `pos`, and the **path context**:

* `selection` part of `Path` = the anchor you just “decided” (e.g., moved topic for OUT, target container for IN).
* `computed` part = the list from `findPaths model'` immediately **after** you do the move and **after** a quick `select anchor`.

### In `Main.update` (or wherever you dispatch `Channel.cross`)

Right after a successful cross:

```elm
import Journal
import MapAutoSize exposing (findPaths) -- you have this already
import ModelAPI exposing (select)       -- explicit select to set an anchor

-- ... inside the Ok branch where you currently return (model1, cmd)
let
    -- choose anchor explicitly:
    -- IN: anchor on target container; OUT: anchor on moved topic
    anchor : ( Id, MapPath )
    anchor =
        case ( req.from, req.to ) of
            ( Channel.Root, Channel.Container containerId ) ->
                ( containerId, [ containerId ] )

            ( Channel.Container _, Channel.Root ) ->
                ( req.topicId, [ activeMap model1 ] ) -- or build better path if you have it

            _ ->
                ( req.topicId, [ activeMap model1 ] )

    model2 =
        model1
            |> select (Tuple.first anchor) (Tuple.second anchor)

    paths =
        findPaths model2

    entry =
        case ( req.from, req.to ) of
            ( Channel.Root, Channel.Container containerId ) ->
                Journal.CrossIn
                    { topicId = req.topicId
                    , from = 0
                    , to = containerId
                    , pos = req.pos
                    , path = { selection = Just anchor, computed = paths }
                    , note = "explicit select before autoSize"
                    }

            ( Channel.Container c, Channel.Root ) ->
                Journal.CrossOut
                    { topicId = req.topicId
                    , from = c
                    , to = 0
                    , pos = req.pos
                    , path = { selection = Just anchor, computed = paths }
                    , note = "explicit select before autoSize"
                    }

            _ ->
                Journal.ErrorText "unexpected cross request"

    model3 =
        { model2 | journal = Journal.record entry model2.journal }
in
( MapAutoSize.autoSize model3, cmd )
```

### On failures / no-op

When Channel decides **no-op** (guards or missing preconditions), append:

```elm
{ model
    | journal =
        Journal.record
            (Journal.CrossNoop { topicId = req.topicId, parent = ???, reason = "…" })
            model.journal
}
```

…and return the unchanged model as you already do.

---

# 3) Show it in the app (inline dev overlay)

Add a tiny **overlay** that you can leave on during tests and builds:

```elm
-- in Main.view (root)
div []
    [ ... existing UI ...
    , if model.showJournal then
        div [ style "position" "fixed", style "right" "10px", style "bottom" "10px"
            , style "background" "white", style "box-shadow" "0 2px 10px rgba(0,0,0,0.15)"
            , style "padding" "8px", style "border-radius" "8px", style "z-index" "9999"
            , style "width" "380px" ]
            [ h4 [ style "margin" "0 0 6px 0", style "font-family" "sans-serif" ] [ text "Cross Journal" ]
            , Journal.viewSketch model.journal
            , Journal.viewList model.journal
            ]
      else
        text ""
    ]
```

(You can add a `Msg.ToggleJournal` to turn it on/off; tests won’t care since it’s just rendering.)

---

# 4) Why this works with “no selection”

* **Cross** doesn’t *read* `model.selection` at all.
* We **explicitly `select` an anchor** (moved topic for OUT, target container for IN) **before** `autoSize`.
* `findPaths` uses that selection to compute the resizing path; if tests ask the toolbar again, the anchor selection also makes the button state consistent.
* The **Journal entry stores both**: the selection anchor (what we chose) and the computed paths (what the sizing code uses). That’s the “location in the network of message paths” captured and visible.

---

# 5) Seeds for Replay / Undo

You now have stable entries with:

* `topicId`, `from`, `to`, `pos`
* `path.selection` (anchor) + `path.computed` (for sizing)

Add two simple `Msg`s and handlers later:

```elm
type Msg
    = ReplayLast
    | UndoLast
    | ...

-- ReplayLast: re-run Channel.cross with the stored fields (deterministic)
-- UndoLast: implement inverse move (swap from/to); or keep a pre-move snapshot in Entry if you prefer snapshot-based undo.
```

Because you **don’t depend on ambient selection**, replay is robust: each entry contains everything needed to act and re-size predictably.

---

## TL;DR

* Add `Journal` module (entries + simple views).
* On **every Cross** success/failure: **append an Entry**, and **explicitly select** an anchor before `autoSize`.
* Render a tiny **in-app overlay** (list + tiny SVG path sketch).
* You’ve now got an immediate, visual “meta view” of moves, and a clean foundation for **FedWiki-style journal / replay / undo**.
