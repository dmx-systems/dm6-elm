module FedWiki exposing
    ( JournalEntry(..)
    , Page
    , StoryItem(..)
    , encodeJournalEntry
    , encodePage
    , mkCrossJournalEntry
    , modelToPage
    , pageDecoder
    , pageToModel
    )

import AppModel as AM
import Config
import Defaults as Def
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Model exposing (..)



-- TYPES
-- =====


type alias Page =
    { title : String
    , story : List StoryItem
    , journal : List JournalEntry
    }


type StoryItem
    = StoryParagraph { id : String, text : String }
    | StoryUnknown E.Value


type JournalEntry
    = JournalCreate { item : E.Value, date : Int }
    | JournalEdit { id : String, item : E.Value, date : Int }
    | JournalUnknown E.Value



-- DECODE
-- ======


pageDecoder : D.Decoder Page
pageDecoder =
    D.map3 Page
        (D.field "title" D.string)
        (D.field "story" (D.list storyItemDecoder))
        (D.field "journal" (D.list journalEntryDecoder))


storyItemDecoder : D.Decoder StoryItem
storyItemDecoder =
    D.field "type" D.string
        |> D.andThen
            (\t ->
                case t of
                    "paragraph" ->
                        let
                            idDecoder : D.Decoder String
                            idDecoder =
                                -- prefer: maybe id, default to ""
                                D.maybe (D.field "id" D.string)
                                    |> D.map (Maybe.withDefault "")
                        in
                        D.map2
                            (\id text -> StoryParagraph { id = id, text = text })
                            idDecoder
                            (D.field "text" D.string)

                    _ ->
                        D.value |> D.map StoryUnknown
            )


journalEntryDecoder : D.Decoder JournalEntry
journalEntryDecoder =
    D.field "type" D.string
        |> D.andThen
            (\t ->
                case t of
                    "create" ->
                        D.map2
                            (\item date -> JournalCreate { item = item, date = date })
                            (D.field "item" D.value)
                            (D.field "date" D.int)

                    "edit" ->
                        D.map3
                            (\id item date -> JournalEdit { id = id, item = item, date = date })
                            (D.field "id" D.string)
                            (D.field "item" D.value)
                            (D.field "date" D.int)

                    _ ->
                        D.value |> D.map JournalUnknown
            )



-- ENCODE
-- ======


encodePage : Page -> E.Value
encodePage page =
    E.object
        [ ( "title", E.string page.title )
        , ( "story", E.list encodeStoryItem page.story )
        , ( "journal", E.list encodeJournalEntry page.journal )
        ]


encodeStoryItem : StoryItem -> E.Value
encodeStoryItem item =
    case item of
        StoryParagraph p ->
            E.object
                [ ( "type", E.string "paragraph" )
                , ( "id", E.string p.id )
                , ( "text", E.string p.text )
                ]

        StoryUnknown v ->
            v


encodeJournalEntry : JournalEntry -> E.Value
encodeJournalEntry j =
    case j of
        JournalCreate r ->
            E.object
                [ ( "type", E.string "create" )
                , ( "item", r.item )
                , ( "date", E.int r.date )
                ]

        JournalEdit r ->
            E.object
                [ ( "type", E.string "edit" )
                , ( "id", E.string r.id )
                , ( "item", r.item )
                , ( "date", E.int r.date )
                ]

        JournalUnknown v ->
            v


mkCrossJournalEntry :
    { containerId : Id, topicId : Id, targetMapId : MapId }
    -> Int
    -> JournalEntry
mkCrossJournalEntry payload timestamp =
    JournalEdit
        { id = "dm6-cross"
        , item =
            E.object
                [ ( "type", E.string "dm6-cross" )
                , ( "containerId", E.int payload.containerId )
                , ( "topicId", E.int payload.topicId )
                , ( "targetMapId", E.int payload.targetMapId )
                ]
        , date = timestamp
        }



-- CONVERSIONS
-- ===========


{-| Build a minimal app model from a FedWiki page.

For now we take all `paragraph` story items and add them as Topics on the home map (id 0).
Positions start at (0,0). You can layer smarter layout later.

-}
pageToModel : Page -> AM.Model
pageToModel page =
    let
        base : AM.Model
        base =
            { items = Dict.empty
            , maps = Dict.singleton 0 (Map 0 Dict.empty (Rectangle 0 0 0 0) -1)
            , mapPath = [ 0 ]
            , nextId = 1
            , selection = Def.selection
            , editState = Def.editState
            , measureText = Def.measureText
            , mouse = Def.mouse
            , search = Def.search
            , iconMenu = Def.iconMenu
            , journal = List.map encodeJournalEntry page.journal
            }

        addParagraph : StoryItem -> AM.Model -> AM.Model
        addParagraph si model0 =
            case si of
                StoryParagraph p ->
                    addTopicToHome p.text model0

                StoryUnknown _ ->
                    model0
    in
    List.foldl addParagraph base page.story


{-| Extract a simple FedWiki page from the model.

We emit paragraphs for all Topics that are present on the home map (id 0).
Journal is left empty here; you can append actions elsewhere.

-}
modelToPage : String -> List E.Value -> AM.Model -> Page
modelToPage title extraJournal model =
    let
        homeMap : Maybe Map
        homeMap =
            Dict.get 0 model.maps

        topicIdsOnHome : List Id
        topicIdsOnHome =
            case homeMap of
                Just m ->
                    m.items
                        |> Dict.keys
                        |> List.sort

                Nothing ->
                    []

        toParagraph : Id -> Maybe StoryItem
        toParagraph tid =
            case Dict.get tid model.items of
                Just (Topic ti) ->
                    Just <| StoryParagraph { id = String.fromInt tid, text = ti.text }

                _ ->
                    Nothing

        storyItems : List StoryItem
        storyItems =
            topicIdsOnHome
                |> List.filterMap toParagraph

        journalItems : List JournalEntry
        journalItems =
            -- keep whatever you pass in as raw JSON journal entries
            List.map JournalUnknown extraJournal
    in
    { title = title
    , story = storyItems
    , journal = journalItems
    }



-- INTERNAL HELPERS
-- ================


addTopicToHome : String -> AM.Model -> AM.Model
addTopicToHome txt model0 =
    let
        newId : Id
        newId =
            model0.nextId

        topicInfo =
            { id = newId
            , text = txt
            , iconName = Nothing
            }

        items1 : Dict Id Item
        items1 =
            Dict.insert newId (Topic topicInfo) model0.items

        -- Map item props with a simple default placement
        props : MapProps
        props =
            MapTopic
                { pos = { x = 0, y = 0 }
                , size = Config.topicSize
                , displayMode = Monad LabelOnly
                }

        mi : MapItem
        mi =
            { id = newId
            , hidden = False
            , pinned = False
            , props = props
            , parentAssocId = 0
            }

        updateHome : Map -> Map
        updateHome m =
            { m | items = Dict.insert newId mi m.items }

        maps1 : Dict MapId Map
        maps1 =
            case Dict.get 0 model0.maps of
                Just m ->
                    Dict.insert 0 (updateHome m) model0.maps

                Nothing ->
                    -- if somehow the home map is missing, create it
                    Dict.insert 0 (updateHome (Map 0 Dict.empty (Rectangle 0 0 0 0) -1)) model0.maps
    in
    { model0
        | items = items1
        , maps = maps1
        , nextId = newId + 1
    }
