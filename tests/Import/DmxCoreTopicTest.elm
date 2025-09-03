module Import.DmxCoreTopicTest exposing (tests)

import Compat.CoreModel as Core
import Compat.DmxImport as Import
import Dict
import Expect
import Generated.Fixtures as Fx
import Json.Decode as D
import Model exposing (..)
import Set
import Test exposing (..)


tests : Test
tests =
    describe "DMX core topic importer"
        [ test "830082 imports into a coherent CoreModel" <|
            \_ ->
                case
                    D.decodeString D.value Fx.dmxCoreTopic830082
                        |> Result.andThen Import.decodeCoreTopicToCore
                of
                    Ok core ->
                        Expect.all
                            [ \_ -> Expect.equal True (hasNoIllegalMapIds core)
                            , \_ -> Expect.equal True (mapItemsBelongToItems core)
                            ]
                            ()

                    Err e ->
                        Expect.fail (D.errorToString e)
        ]


hasNoIllegalMapIds : Core.CoreModel -> Bool
hasNoIllegalMapIds c =
    List.all (\mid -> Dict.member mid c.maps) c.mapPath


mapItemsBelongToItems : Core.CoreModel -> Bool
mapItemsBelongToItems c =
    c.maps
        |> Dict.values
        |> List.concatMap (\mp -> Dict.values mp.items)
        |> List.all (\mi -> Dict.member mi.id c.items)


mapParentIdsExist : Core.CoreModel -> Bool
mapParentIdsExist c =
    -- With the old parentMapId field removed, assert a stable invariant:
    -- each map's internal id matches its key in the dict.
    c.maps
        |> Dict.toList
        |> List.all (\( id, m ) -> m.id == id)


mapItemsHaveValidParentAssoc : Core.CoreModel -> Bool
mapItemsHaveValidParentAssoc c =
    let
        assocIds =
            c.items
                |> Dict.filter
                    (\_ it ->
                        case it.info of
                            Assoc _ ->
                                True

                            _ ->
                                False
                    )
                |> Dict.keys
                |> Set.fromList

        -- Flatten all MapItems across all maps
        allMapItems : List MapItem
        allMapItems =
            c.maps
                |> Dict.values
                |> List.concatMap (\m -> Dict.values m.items)
    in
    -- parentAssocId = 0 means “no parent assoc”; otherwise it must reference an existing assoc
    allMapItems
        |> List.all (\mi -> mi.parentAssocId == 0 || Set.member mi.parentAssocId assocIds)
