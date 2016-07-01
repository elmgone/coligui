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

module RSync exposing (Model, Msg, init, update, view)

import BoolV
import StringV

import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Task
import Json.Decode as JD exposing ((:=))
import Json.Encode

main =
  Html.App.program {
    init = init,  -- ( init "To Be or Not To Be" ),
    view = view,
    update = update,
    subscriptions = subscriptions
  }


-- MODEL

type alias Model =
  { id      : String
  --, label   : String
  , verbose : BoolV.Model
  , visible : Bool
  }

-- init : String -> (Model, Cmd Msg)
init : (Model, Cmd Msg)
init =
  let
    ( verbV, verbC ) = BoolV.init "verbose"
  in
    (Model "" verbV True, Cmd.map (VerboseMsg verbV) verbC)  -- Cmd.map (TagMsg model.nextIdx) tCmd


-- UPDATE

type Msg =
  VerboseMsg BoolV.Model BoolV.Msg
--    Save
--  | Edit
--  | EditPath String
--  | CreateSucceed Model
--  | CreateFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
--  let
--    e = toString model.editing
--    d = toString model.dirty
--  in
    case msg of
      VerboseMsg verbBV bMsg ->
        let
          (newVerb, vCmd) = BoolV.update bMsg verbBV
        in
        ( { model | verbose = newVerb -- (Debug.log model.label nVal)
--          }, Cmd.none )
          }, Cmd.map (VerboseMsg newVerb) vCmd )

{-- }
      Save ->
        let
          newModel = { model
                     | path = model.editPath
                     , dirty = True
                     , editing = False
                     , errMsg = "(saving tag ..., e=F, d=T)"
                     }
        in
          ( Debug.log "Ok" newModel, saveTag newModel )
{ --}


-- VIEW

view : Model -> Html Msg
view model =
  let
    vv = Debug.log "verbose" model.verbose
    verbView = Html.App.map (VerboseMsg model.verbose) (BoolV.view vv)
  in
    div [] [
      verbView
    ]

{-- }
      if model.visible then
        visibleView model
      else
        invisibleView model

visibleView : Model -> Html Msg
visibleView model =
  let
    verbHM =
  in
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

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

