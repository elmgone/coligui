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

module ComboBox exposing (..)

import Widget as W -- exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Cmd.Extra
-- import Json.Encode                               --as JE
--import Json.Decode exposing ((:=))  --, (|:)) -- as JD
--import Json.Decode.Extra exposing ((|:)) -- as JD
--import Regex as RX   -- exposing (regex) as RX
import String exposing (..)
--import Dict   -- as Di  --  exposing (..)


main =
  Html.App.program {
    init = ( init "Choose", Cmd.none ),
    view = view,
    update = update,
    subscriptions = \_ -> Sub.none
  }


-- MODEL

{----------------------------------------------
----------------------------------------------}
type alias Model =
    {
      label      : String
    , field      : String
    , tmpField   : String
    , entries    : List Entry
    }

type alias Entry
  = {
      name : String
    , ref  : String
    }

init : String -> Model
init label =
  Model label "" "default" []


-- UPDATE

type Msg
  = NoOp
  | UpdateField String
  | Action
  --| AddEntry Entry
  --| SetField String


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
      -- ( model, Cmd.none )

    UpdateField str ->
      { model | tmpField = str }
        ! []

    Action ->
      let
        nField =
          if model.field == "" then
            model.tmpField
          else
            model.field
      in
        { model | field = nField, tmpField = "" } ! []


-- VIEW

view : Model -> Html Msg
view model =
  ---viewInput model.field

    {--------------------}
  table [] [ tr [] ( [
    td [] [ label [] [ text model.label ] ]
  --  td [] [ label [] [ text "Configuration" ] ]
  , td [] [ select []
      ( List.map (\ j -> option [] [ text j.name ] ) model.entries ) ]
  ] ++ ( viewField model ) )
   ]
    --------------------}

viewField : Model -> List ( Html Msg )
viewField model =
  let
      cfgNameIsEmpty = String.isEmpty ( String.trim model.field )
      tmpCfgNameIsEmpty = String.isEmpty ( String.trim model.tmpField )
      enableSave = ---allowToSave &&
      ( not (
        cfgNameIsEmpty && tmpCfgNameIsEmpty
      ) )
  in
    --  table [] [ tr []
      [
        td [] [ button [
                  onClick Action
--                , disabled cfgNameIsEmpty
              --  , disabled (not enableSave)
                ] [ text "Save" ] ]
      , td [] [ label [] [ text "Enter new" ] ]
      , td [] [ input [
                  type' "text"
                , value model.tmpField
                , onInput UpdateField
                ] [] ]
      ]


    {--------------------
viewInput : String -> Html Msg
viewInput str =
  div
    []  -- class "header" ]
    [ label [] [ text model.label ]
    , input
        [ -- class "new-todo"
        --, placeholder "What needs to be done?"
          autofocus True
        , value str
        --, name "newTodo"
        --, onInput UpdateField
        --, onEnter Add
        ]
        []
    ]
    --------------------}


