module Feature.Connection.Channel exposing
    ( Boundary(..)
    , CrossError(..)
    , CrossPlan
    , CrossRequest
    , Effect(..)
    , cross
    , crossIn
    , crossOut
    , defaultPermit
    )

{-| Minimal compile-baseline of Connection Channel.

NOTE: Guards and actual move semantics are intentionally NO-OP to avoid
dependency mismatches. This gets the project compiling again. Reintroduce
real checks and ModelAPI mutations incrementally once stable.

-}

import AppModel as AM exposing (Model)
import Model as M exposing (Id, MapId, Point)



-- BOUNDARIES


type Boundary
    = Root
    | Container Id



-- ERRORS


type CrossError
    = Other String



-- PERMIT (placeholder)


type alias CrossPermit =
    {}



-- placeholder; keep shape for Main compatibility


defaultPermit : CrossPermit
defaultPermit =
    {}



-- REQUEST / PLAN


type alias CrossRequest =
    { topicId : Id
    , from : Boundary
    , to : Boundary
    , pos : Point
    , permit : CrossPermit
    }


type alias CrossPlan =
    { topicId : Id
    , fromMap : MapId
    , toMap : MapId
    , pos : Point
    }



-- EFFECTS (for future ports)


type Effect
    = None
    | Out_Crossed { topicId : Int, fromMap : Int, toMap : Int }



-- PUBLIC API


crossIn : Id -> Id -> Point -> AM.Model -> Result CrossError ( AM.Model, CrossPlan, Effect )
crossIn topicId containerId pos model =
    cross
        { topicId = topicId
        , from = Root
        , to = Container containerId
        , pos = pos
        , permit = defaultPermit
        }
        model


crossOut : Id -> Id -> Point -> AM.Model -> Result CrossError ( AM.Model, CrossPlan, Effect )
crossOut topicId containerId pos model =
    cross
        { topicId = topicId
        , from = Container containerId
        , to = Root
        , pos = pos
        , permit = defaultPermit
        }
        model


cross : CrossRequest -> AM.Model -> Result CrossError ( AM.Model, CrossPlan, Effect )
cross req model0 =
    case plan req of
        Ok plan_ ->
            let
                -- APPLY IS A NO-OP FOR NOW: return model unchanged, but emit effect
                eff =
                    Out_Crossed
                        { topicId = plan_.topicId
                        , fromMap = plan_.fromMap
                        , toMap = plan_.toMap
                        }
            in
            Ok ( model0, plan_, eff )

        Err e ->
            Err e



-- PLANNING


plan : CrossRequest -> Result CrossError CrossPlan
plan req =
    case ( boundaryToMap req.from, boundaryToMap req.to ) of
        ( Just fromMapId, Just toMapId ) ->
            Ok
                { topicId = req.topicId
                , fromMap = fromMapId
                , toMap = toMapId
                , pos = req.pos
                }

        ( Nothing, _ ) ->
            Err (Other "Unknown source boundary")

        ( _, Nothing ) ->
            Err (Other "Unknown target boundary")



-- HELPERS


boundaryToMap : Boundary -> Maybe MapId
boundaryToMap b =
    case b of
        Root ->
            Just 0

        Container tid ->
            Just tid
