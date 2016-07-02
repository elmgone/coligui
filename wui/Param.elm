-- Copyright © 2016 ElmGone mrcs.elmgone@mailnull.com
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Param exposing (
    Model, Msg, Id
  , BoolP, StringP, BoolM, StringM
  , init, get, update, updateOne, viewList
  , viewOne, viewOneOpt, viewTR
  , inputBool, inputString
  --, viewBool
  , viewString
  -- , updateModel, view)
  )

import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Task
import Json.Decode as JD exposing ((:=))
import Json.Encode


{-- }
main =
  Html.App.program {
    init = ( init "To Be or Not To Be" ),
    view = view,
    update = update,
    subscriptions = subscriptions
  }
--}


-- MODEL

type alias BoolP   = Model Bool
type alias StringP = Model String
type alias BoolM   = Msg Bool
type alias StringM = Msg String

type alias Id = String

type alias Model valType =
  { id      : Id
  , label   : String
  , value   : valType
  , visible : Bool
  }

init : Id -> String -> valType -> Model valType
init id name val =
  Model id name val True

--init : String -> valType -> (Model valType, Cmd (Msg valType))
--init name val =
  --(Model "" name val True, Cmd.none)


get : List (Model vt) -> Id -> Maybe (Model vt)
get models id =
  List.head ( List.filter (\m -> m.id == id) models )

{-- }
getOne : Model vt -> Id -> Maybe (Model vt)
getOne model id =
  if model.id == id then
    Just model
  else
    Nothing
--}

-- UPDATE

type Msg valType =
    Edit valType
  | Visible Bool

{-- }
--  Set Bool
--    Save
--  | Edit
--  | EditPath String
--  | CreateSucceed Model
--  | CreateFail Http.Error
{ --}

update : Msg valType -> Model valType -> (Model valType, Cmd (Msg valType))
update msg model =
    case msg of
      Edit val ->
        ( { model | value = {--val  --}  (Debug.log model.label val)
          }, Cmd.none )

      Visible vis ->
        ( { model | visible = {--val  --}  (Debug.log (model.label ++ " visible") vis)
          }, Cmd.none )

updateOne : Id -> Msg valType -> Model valType -> (Model valType, Cmd (Msg valType))
updateOne id msg model =
  let
    mdl = updateModel id msg model
  in
    ( mdl, Cmd.none )

{--}

updateModel : Id -> Msg valType -> Model valType -> Model valType
updateModel id msg model =
  if id /= model.id then
    model
  else
    case msg of
      Edit val ->
        { model | value = {--val  --}  (Debug.log model.label val)
        }

      Visible vis ->
        { model | visible = {--vis  --}  (Debug.log (model.label ++ " visible") vis)
        }


-- VIEW

{-- }
view : Model valType -> Html (Msg valType)
view model =
  case model of
    Model Bool ->
      div [] (viewList model)
--}

{-- }
viewBool : Maybe BoolP -> Html (Msg Bool)
viewBool optModel =
  let
    dc =
      case optModel of
        Nothing ->
          []
        Just model ->
          -- viewList model [ input [ type' "checkbox", checked model.value, onCheck Edit ] [] ]
          viewList model [ inputBool model ]
  in
    div [] dc
--}

viewOne : ( Model valType -> Html (Msg valType) )
  -> Model valType
  -> Html (Msg valType)
viewOne inputOf model =
  let
    dc =
          -- viewList model [ input [ type' "checkbox", checked model.value, onCheck Edit ] [] ]
          --viewList model [ inputBool model ]
          viewList model [ inputOf model ]

  in
    div [] dc

viewOneOpt : ( Model valType -> Html (Msg valType) )
  -> Maybe (Model valType)
  -> Html (Msg valType)
viewOneOpt inputOf optModel =
      case optModel of
        Nothing ->
          div [] []
        Just model ->
          viewOne inputOf model
          --viewList model [ inputOf model ]


viewTR : ( Model valType -> Html (Msg valType) )
  -> Model valType
  -> Html (Msg valType)
viewTR inputOf paramP =
  let
    -- list = Param.viewList paramP [ Param.inputBool paramP ]
    list = viewList paramP [ inputOf paramP ]
    cells = List.map (\e -> td [] [ e ]) list
  in
    tr [] cells


inputBool : BoolP -> Html (Msg Bool)
inputBool model =
  input [ type' "checkbox", checked model.value, onCheck Edit ] []

inputString : StringP -> Html (Msg String)
inputString model =
  --input [ type' "checkbox", checked model.value, onCheck Edit ] []
  input [ type' "text", value model.value, onInput Edit ] []



{--}
viewString : (Model String) -> Html (Msg String)
viewString model =
  div []  -- (viewStringList model)
    ( viewList model [ input [ type' "text", value model.value, onInput Edit ] [] ] )

viewList : Model valType -> List (Html (Msg valType)) -> List (Html (Msg valType))
viewList model modelElems =
  ( label [] [ text model.label ] ) :: modelElems


{-- }
view : Model -> Html Msg
view model =
      if model.visible then
        visibleView model
      else
        invisibleView model

visibleView : Model -> Html Msg
visibleView model =
  div [] [
    label [] [ text model.label ]
  , input [ type' "checkbox", checked model.isTrue, onCheck Set ] []
  --, text (toString model.isTrue)
  ]

invisibleView : Model -> Html Msg
invisibleView model =
  div [] []
{ --}


-- SUBSCRIPTIONS

subscriptions : Model valType -> Sub Msg
subscriptions model =
  Sub.none

