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
import String exposing (..)


{----------------------------------------------
main =
  Html.App.program {
    -- init = ( init "Choose" Success, Cmd.none ),
    init = ( init "Choose", Cmd.none ),
    view = view identity Action,
    update = update,
    subscriptions = \_ -> Sub.none
  }
----------------------------------------------}


-- MODEL

type alias Model =
    { field      : String
    , tmpField   : String
    , entries    : List String
    , debug      : Bool
    }

{------------------------------
------------------------------}

init : Model
init =
  Model "" "default" [""] False



{------------------------------
{-| 
    The configuration for the ComboBox you embed.
    The `actionMessage` is an optional `Signal.Message` we will send when the user
    clicks the action button.
-}
{-
    The `header`, `body` and `footer` are all `Maybe (Html msg)` blocks.
    Those `(Html msg)` blocks can be as simple or as complex as any other view function.
    Use only the ones you want and set the others to `Nothing`.
-}
type alias Config msg =
    { actionMessage : msg
    --, header : Maybe (Html msg)
    --, body : Maybe (Html msg)
    --, footer : Maybe (Html msg)
    }
------------------------------}





-- UPDATE

type Msg
  = NoOp
  | UpdateField String
  | Action
  | Select String
  | Success String
  | ToggleDebug Bool


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    UpdateField str ->
      { model | tmpField = str, field = "" }
        ! []

      {-------------------------------------}
    Action ->
      let
        nField =
          if model.field == "" then
            model.tmpField
          else
            model.field
      in
        { model | field = nField, tmpField = "" } ! []
      {-------------------------------------}

    Select str ->
      let
        nEntries = model.entries
          --- str :: List.filter (\ e -> e /= str ) model.entries
      in
        { model
        | field = str
        , tmpField = ""
        , entries = nEntries
        } ! []

    Success str ->
      let
        nEntries =
          str :: List.filter (\ e -> e /= str ) model.entries
      in
        { model
        | field = str
        , tmpField = ""
        , entries = nEntries
        } ! []

    ToggleDebug dbg ->
      { model | debug = dbg } ! []


-- VIEW

{------------------------------------------------------------------
------------------------------------------------------------------}

isEmptyString : String -> Bool
isEmptyString s =
  String.isEmpty ( String.trim s )

viewButton : (String -> Html msg) -> (String -> msg) -> Model -> Html msg
viewButton labeller actionMsg model =
  let
      fieldIsEmpty    = isEmptyString model.field
      tmpFieldIsEmpty = isEmptyString model.tmpField

{-------------------------------------------------------------------
-------------------------------------------------------------------}
      isBlocked =
        fieldIsEmpty && tmpFieldIsEmpty
      actionStr =
        if fieldIsEmpty then
          String.trim model.tmpField
        else
          String.trim model.field
  in
      button [ onClick ( actionMsg actionStr )
             , disabled isBlocked
             ] [ labeller actionStr ]
{-------------------------------------------------------------------
-------------------------------------------------------------------}

viewOption : String -> (String -> msg) -> Model -> Html msg
viewOption neutralEntry selectMsg model =
  let
    txt s =
      if s == "" then
        neutralEntry
      else
        s
    optAttrs s =
      ( selected ( s == model.field ) ) :: (
        -- if isEmptyString s || s == model.field then
        if s == model.field then
          []
        else
          [ onClick (selectMsg s) ]
      )
    opt s =
      option ( optAttrs s ) [ text (txt s) ]
  in
    select []
        ( List.map opt model.entries )


viewField : Model -> Html Msg
viewField model =
  let
      fieldIsEmpty = String.isEmpty ( String.trim model.field )
{-------------------------------------------------------------------
      tmpFieldNameIsEmpty = String.isEmpty ( String.trim model.tmpField )
      enableSave =
      ( not (
        fieldIsEmpty && tmpFieldNameIsEmpty
      ) )
-------------------------------------------------------------------}
  in
{-------------------------------------------------------------------
-------------------------------------------------------------------}
                input [
                  type' "text"
                , value model.tmpField
                , onInput UpdateField
                --, disabled ( not fieldIsEmpty )
                
        --, placeholder "What needs to be done?"
        --, name "newTodo"
        --, onInput UpdateField
        --, onEnter Add
          , autofocus True
                ] []



{--------------------------------------------------------------}
viewDbg : Model -> Html Msg
viewDbg model =
  let
    dbgInfo =
      if model.debug then
        text ( toString model )
      else
        div [] []
  in
      div [] [
        label [] [ text "debug" ]
      , input [
          type' "checkbox"
          , checked model.debug
          , onCheck ToggleDebug
        ] []
      , dbgInfo
      ]

{--------------------------------------------------------------
------------------------------------------------------------}
