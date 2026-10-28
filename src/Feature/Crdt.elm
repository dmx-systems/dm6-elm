module Feature.Crdt exposing (docModel, initDoc, encodeDoc,
  createTopic, createAssoc, setTopicIcon, setTopicText, setTopicViewSize, setTopicEditorSize,
  removeAssocId, deleteTopic, deleteAssoc,
  createBox, createItemSet, lookupBox, lookupItemSet, itemSets, addToItemSet, removeFromItemSet,
  addBoxTopic, lookupBoxTopic, setExpansion, setBoxRenderer,
  markSingleEdit, markEditStart, markEditEnd, undo, redo, canUndo, canRedo)

import Console
import Feature.CrdtDef exposing (refs, topicIconRef, topicTextRef, topicViewSizeRef,
  topicEditorSizeRef, topicAssocIdsRef, boxRef, boxTopicsRef, boxTopicRef, expansionRef,
  boxRendererRef, itemSetRef, itemSetItemsRef)
import Model exposing (Model)
import ModelBase exposing (..)
import Renderer exposing (Renderer)

import Crdt.Doc as Doc exposing (Doc, Version)
import Crdt.Edit as Edit

import Dict exposing (Dict)
import Json.Encode as E



docModel : Model -> Maybe DocModel
docModel model =
  case Doc.read model.crdt.doc of
    Ok docModel_ ->
      Just docModel_
    Err err ->
      let
        _ = Console.logError "Feature.Crdt.doc" "Schema read error" (Doc.readErrorToString err)
      in
      Nothing


initDoc : E.Value -> Model -> Model
initDoc ops model =
  Doc.decodeInto ops model.crdt.doc
    |> Result.map (setDoc model)
    |> Result.map updateLastStored
    |> Result.mapError (Console.logError "Feature.Crdt.initDoc" "Decode error")
    |> Result.withDefault model


encodeDoc : Model -> (Model, E.Value)
encodeDoc ({crdt} as model) =
  ( model |> setLastStored (Doc.version crdt.doc)
  , crdt.doc |> Doc.encodeSince crdt.lastStored
  )


-- Topic + Assoc

createTopic : Topic -> Model -> Model
createTopic topic model =
  Edit.setKey refs.topics (toTopicId topic.id) topic model.crdt.doc
    |> editResult model


createAssoc : Assoc -> Model -> Model
createAssoc assoc model =
  model.crdt.doc
    |> Edit.setKey refs.assocs (toAssocId assoc.id) assoc
    |> Result.andThen (Edit.append (topicAssocIdsRef (toTopicId assoc.topicId1)) assoc.id)
    |> Result.andThen (Edit.append (topicAssocIdsRef (toTopicId assoc.topicId2)) assoc.id)
    |> editResult model


setTopicIcon : TopicId -> Maybe Icon -> Model -> Model
setTopicIcon (TopicId id) icon model =
  model.crdt.doc
    |> Edit.set (topicIconRef id) icon -- TODO: use Edit.switch instead?
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


lookupBox : BoxId -> Model -> Maybe Box
lookupBox (BoxId (TopicId id)) model =
  model.crdt.doc
    |> Edit.readAt (Doc.version model.crdt.doc) (boxRef id)
    |> Result.map Just
    |> Result.mapError (Console.logError "Feature.Crdt.lookupBox" "Read error")
    |> Result.withDefault Nothing


lookupItemSet : Id -> Model -> Maybe ItemSet
lookupItemSet id model =
  model.crdt.doc
    |> Edit.readAt (Doc.version model.crdt.doc) (itemSetRef id)
    |> Result.map Just
    |> Result.mapError (Console.logError "Feature.Crdt.lookupItemSet" "Read error")
    |> Result.withDefault Nothing


itemSets : Model -> Maybe (Dict Id ItemSet)
itemSets model =
  model.crdt.doc
    |> Edit.readAt (Doc.version model.crdt.doc) refs.itemSets
    |> Result.map Just
    |> Result.mapError (Console.logError "Feature.Crdt.itemSets" "Read error")
    |> Result.withDefault Nothing


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


lookupBoxTopic : TopicId -> BoxId -> Model -> Maybe BoxTopic
lookupBoxTopic (TopicId topicId) (BoxId (TopicId boxId)) model =
  model.crdt.doc
    |> Edit.readAt (Doc.version model.crdt.doc) (boxTopicRef topicId boxId)
    |> Result.map Just
    |> Result.mapError (Console.logError "Feature.Crdt.lookupBoxTopic" "Read error")
    |> Result.withDefault Nothing


-- TODO: change to updateExpansion and receive transform function?
setExpansion : TopicId -> BoxId -> Expansion -> Model -> Model
setExpansion (TopicId topicId) (BoxId (TopicId boxId)) expansion model =
  model.crdt.doc
    |> Edit.set (expansionRef topicId boxId) expansion -- TODO: use Edit.switch instead?
    |> editResult model


setBoxRenderer : BoxId -> Renderer -> Model -> Model
setBoxRenderer (BoxId (TopicId id)) renderer model =
  model.crdt.doc
    |> Edit.set (boxRendererRef id) renderer
    |> editResult model



-- UNDO / REDO


markSingleEdit : Model -> Model -> Model
markSingleEdit oldModel model =
  Doc.recordEdit (Doc.version oldModel.crdt.doc) model.crdt.doc
    |> setDoc model


markEditStart : Model -> Model -> Model
markEditStart oldModel model =
  model
    |> setEditStartVersion (Just (Doc.version oldModel.crdt.doc))


markEditEnd : Model -> Model
markEditEnd model =
  case model.crdt.editStartVersion of
    Just version ->
      Doc.recordEdit version model.crdt.doc
        |> setDoc model
        |> setEditStartVersion Nothing
    Nothing ->
      let
        _ = Console.logError "Feature.Crdt.markEditEnd" "Edit start is not marked" Nothing
      in
      model


undo : Model -> Model
undo model =
  Doc.undo model.crdt.doc
    |> setDoc model


redo : Model -> Model
redo model =
  Doc.redo model.crdt.doc
    |> setDoc model


canUndo : Model -> Bool
canUndo model =
  Doc.canUndo model.crdt.doc


canRedo : Model -> Bool
canRedo model =
  Doc.canRedo model.crdt.doc



-- LOCAL HELPER


editResult : Model -> Result Edit.EditError (Doc DocModel) -> Model
editResult model result =
  case result of
    Ok doc -> setDoc model doc
    Err err ->
      let
        _ = Console.logError "Feature.Crdt.editResult" "Edit error"
          (Edit.editErrorToString err)
      in
      model


setDoc : Model -> Doc DocModel -> Model
setDoc ({crdt} as model) doc =
  { model | crdt = { crdt | doc = doc }}


updateLastStored : Model -> Model
updateLastStored model =
  model
    |> setLastStored (Doc.version model.crdt.doc)


setLastStored : Version -> Model -> Model
setLastStored lastStored ({crdt} as model) =
  { model | crdt = { crdt | lastStored = lastStored }}


setEditStartVersion : Maybe Version -> Model -> Model
setEditStartVersion editStartVersion ({crdt} as model) =
  { model | crdt = { crdt | editStartVersion = editStartVersion }}
