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

-- module BoolV exposing (Model, Msg, init, update, view)
module Param exposing ( {-- BoolV,-- } StringV, { --} Model, Msg, init, updateModel, viewList, viewBool, viewString)  -- , view)

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



--type alias BoolV =
  --Model Bool

type alias StringV =
  Model String


-- MODEL

type alias Model valType =
  { id      : String
  , label   : String
  , value   : valType
  , visible : Bool
  }

init : String -> valType -> (Model valType, Cmd (Msg valType))
init name val =
  (Model "" name val True, Cmd.none)


-- UPDATE

type Msg valType =
  Edit valType

{-- }
--  Set Bool
--    Save
--  | Edit
--  | EditPath String
--  | CreateSucceed Model
--  | CreateFail Http.Error

update : Msg valType -> Model valType -> (Model valType, Cmd Msg valType)
update msg model =
    case msg of
      Edit val ->
        ( { model | value = {--val  --}  (Debug.log model.label val)
          }, Cmd.none )
{ --}

updateModel : Msg valType -> Model valType -> Model valType
updateModel msg model =
    case msg of
      Edit val ->
        { model | value = {--val  --}  (Debug.log model.label val)
        }



-- VIEW

{-- }
view : Model valType -> Html (Msg valType)
view model =
  case model of
    Model Bool ->
      div [] (viewList model)
--}

viewBool : (Model Bool) -> Html (Msg Bool)
viewBool model =
  div [] --(viewBoolList model)
    (viewList model [ input [ type' "checkbox", checked model.value, onCheck Edit ] [] ])

{--}
viewString : StringV -> Html (Msg String)
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

