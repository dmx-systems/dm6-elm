port module Feature.Crdt exposing (update, sub, encodeDelta, setRootBoxId, createTopic,
  setTopicIcon, setTopicText, setTopicViewSize, setTopicEditorSize, removeAssocId, deleteTopic,
  createAssoc, deleteAssoc, createBox, createItemSet, addToItemSet, removeFromItemSet,
  addBoxTopic, setExpansion, setBoxRenderer, markUndoSingle, markUndoBegin, markUndoEnd, undo,
  redo, canUndo, canRedo, editResult)

import Console
import Env exposing (Env)
import Feature.CrdtDef as CrdtDef exposing (refs, topicIconRef, topicTextRef, topicViewSizeRef,
  topicEditorSizeRef, topicAssocIdsRef, boxTopicsRef, expansionRef, boxRendererRef,
  itemSetItemsRef)
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (..)
import Renderer exposing (Renderer)
import SyncModel exposing (SyncModel)

import Crdt.Doc as Doc exposing (Doc, Version)
import Crdt.Edit as Edit

import Json.Encode as E



-- PORTS


port incoming : (E.Value -> msg) -> Sub msg



-- SUBSCRIPTIONS


sub : Sub Msg
sub =
  incoming (Crdt << CrdtDef.OpsReceived)



-- UPDATE


update : CrdtDef.Msg -> Env -> Outcome
update msg {model} =
  case msg of
    CrdtDef.OpsReceived ops ->
      model.crdt.doc
        |> Doc.decodeInto ops
        |> result model
        |> Outcome.withDir (Directives StoreOnly NoUndo)



--


encodeDelta : Model -> (E.Value, Model)
encodeDelta ({crdt} as model) =
  ( Doc.encodeSince crdt.lastStored crdt.doc
  , setLastStored (Doc.version crdt.doc) model
  )


setRootBoxId : BoxId -> Model -> Model
setRootBoxId boxId model =
  model.crdt.doc
    |> Edit.set refs.rootBoxId boxId
    |> editResult model


-- Topic

createTopic : Topic -> Model -> Model
createTopic topic model =
  model.crdt.doc
    |> Edit.setKey refs.topics (toTopicId topic.id) topic
    |> editResult model


setTopicIcon : TopicId -> Maybe Icon -> Model -> Model
setTopicIcon (TopicId id) icon model =
  model.crdt.doc
    |> Edit.switch (topicIconRef id) icon
    |> editResult model


setTopicText : TopicId -> String -> Model -> Model
setTopicText (TopicId id) text model =
  model.crdt.doc
    |> Edit.set (topicTextRef id) text
    |> editResult model


setTopicViewSize : TopicId -> Size -> Model -> Model
setTopicViewSize (TopicId id) size model =
  model.crdt.doc
    |> Edit.set (topicViewSizeRef id) size
    |> editResult model


setTopicEditorSize : TopicId -> Size -> Model -> Model
setTopicEditorSize (TopicId id) size model =
  model.crdt.doc
    |> Edit.set (topicEditorSizeRef id) size
    |> editResult model


removeAssocId : Int -> TopicId -> Model -> Model
removeAssocId index (TopicId topicId) model =
  model.crdt.doc
    |> Edit.remove (topicAssocIdsRef topicId) index
    |> editResult model


deleteTopic : TopicId -> Model -> Model
deleteTopic (TopicId id) model =
  model.crdt.doc
    |> Edit.removeKey refs.topics id
    |> editResult model


-- Assoc

createAssoc : Assoc -> Model -> Model
createAssoc assoc model =
  model.crdt.doc
    |> Edit.setKey refs.assocs (toAssocId assoc.id) assoc
    |> Result.andThen (Edit.append (topicAssocIdsRef (toTopicId assoc.topicId1)) assoc.id)
    |> Result.andThen (Edit.append (topicAssocIdsRef (toTopicId assoc.topicId2)) assoc.id)
    |> editResult model


deleteAssoc : AssocId -> Model -> Model
deleteAssoc (AssocId id) model =
  model.crdt.doc
    |> Edit.removeKey refs.assocs id
    |> editResult model


-- Box

createBox : Box -> Model -> Model
createBox box model =
  model.crdt.doc
    |> Edit.setKey refs.boxes (toBoxId box.id) box
    |> editResult model


createItemSet : ItemSet -> Model -> Model
createItemSet itemSet model =
  model.crdt.doc
    |> Edit.setKey refs.itemSets itemSet.id itemSet
    |> editResult model


addToItemSet : Id -> SetItem -> Model -> Model
addToItemSet itemSetId item model =
  model.crdt.doc
    |> Edit.append (itemSetItemsRef itemSetId) item
    |> editResult model


removeFromItemSet : Id -> Int -> Model -> Model
removeFromItemSet itemSetId index model =
  model.crdt.doc
    |> Edit.remove (itemSetItemsRef itemSetId) index
    |> editResult model


addBoxTopic : BoxTopic -> BoxId -> Model -> Model
addBoxTopic topic (BoxId (TopicId id)) model =
  model.crdt.doc
    |> Edit.setKey (boxTopicsRef id) (toTopicId topic.id) topic
    |> editResult model


-- TODO: change to updateExpansion and receive transform function?
setExpansion : TopicId -> BoxId -> Expansion -> Model -> Model
setExpansion (TopicId topicId) (BoxId (TopicId boxId)) expansion model =
  model.crdt.doc
    |> Edit.switch (expansionRef topicId boxId) expansion
    |> editResult model


setBoxRenderer : BoxId -> Renderer -> Model -> Model
setBoxRenderer (BoxId (TopicId id)) renderer model =
  model.crdt.doc
    |> Edit.set (boxRendererRef id) renderer
    |> editResult model



-- UNDO / REDO


markUndoSingle : Model -> Model -> Model
markUndoSingle prevModel model =
  model.crdt.doc
    |> Doc.recordEdit (Doc.version prevModel.crdt.doc)
    |> setDoc model


{- Marks the begin of an undoable action:
remembers the previous model's document version in the given model.
-}
markUndoBegin : Model -> Model -> Model
markUndoBegin prevModel model =
  model
    |> setUndoBoundary (Just (Doc.version prevModel.crdt.doc))


markUndoEnd : Model -> Model
markUndoEnd model =
  case model.crdt.undoBoundary of
    Just version ->
      model.crdt.doc
        |> Doc.recordEdit version
        |> setDoc model
        |> setUndoBoundary Nothing
    Nothing ->
      let
        _ = Console.logError "Feature.Crdt.markUndoEnd" "Undo begin is not marked" Nothing
      in
      model


undo : Model -> Model
undo model =
  model.crdt.doc
    |> Doc.undo
    |> setDoc model
    |> updateCache


redo : Model -> Model
redo model =
  model.crdt.doc
    |> Doc.redo
    |> setDoc model
    |> updateCache


canUndo : Model -> Bool
canUndo model =
  model.crdt.doc
    |> Doc.canUndo


canRedo : Model -> Bool
canRedo model =
  model.crdt.doc
    |> Doc.canRedo



-- LOCAL HELPER


editResult : Model -> Result Edit.EditError (Doc SyncModel) -> Model
editResult model result_ =
  result model (result_ |> Result.mapError Edit.editErrorToString)


result : Model -> Result String (Doc SyncModel) -> Model
result model result_ =
  case result_ of
    Ok doc ->
      doc
        |> setDoc model
        |> updateCache
    Err err ->
      let
        _ = Console.logError "Feature.Crdt.result" "Edit error" err
      in
      model


updateCache : Model -> Model
updateCache model =
  case Doc.read model.crdt.doc of
    Ok docModel ->
      { model | cache = docModel }
    Err err ->
      let
        _ = Console.logError "Feature.Crdt.updateCache" "Schema read error"
          (Doc.readErrorToString err)
      in
      model


setDoc : Model -> Doc SyncModel -> Model
setDoc ({crdt} as model) doc =
  { model | crdt = { crdt | doc = doc }}


setLastStored : Version -> Model -> Model
setLastStored lastStored ({crdt} as model) =
  { model | crdt = { crdt | lastStored = lastStored }}


setUndoBoundary : Maybe Version -> Model -> Model
setUndoBoundary undoBoundary ({crdt} as model) =
  { model | crdt = { crdt | undoBoundary = undoBoundary }}
