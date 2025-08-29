module Import.DmxCoreTopicTest exposing (tests)

import Compat.CoreModel as Core
import Compat.DmxImport as Import
import Dict
import Expect
import Generated.Fixtures as Fx
import Json.Decode as D
import Model exposing (..)
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
