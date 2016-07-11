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

type alias Model =  -- onSuccess_F =
    {
--      label      : String
--    , 
      field      : String
    , tmpField   : String
    --, entries    : List Entry
    , entries    : List String
    --, neutralEntry : String
    , debug      : Bool
    }

{------------------------------
type alias Entry
  = {
      name : String
    , ref  : String
    }
------------------------------}

--init : String -> Model
--init label =
--  Model label "" "default" [] True
init : Model
init =
  Model "" "default" [""]   -- ["-- new --"]    -- "-- new --"
    False   -- True



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





-- UPDATE

type Msg
  = NoOp
  | UpdateField String
  | Action
  | Success String
  | ToggleDebug Bool
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

    Success str ->
      let
      {-------------------------------------
        nField =
          if model.field == "" then
            model.tmpField
            str
          else
            model.field
      -------------------------------------}
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

viewButton : Html msg -> (String -> msg) -> Model -> Html msg
viewButton label actionMsg model =
  let
      fieldIsEmpty = String.isEmpty ( String.trim model.field )
      tmpFieldNameIsEmpty = String.isEmpty ( String.trim model.tmpField )
      enableSave = ---allowToSave &&
      ( not (
        fieldIsEmpty && tmpFieldNameIsEmpty
      ) )
      actionStr =
        if fieldIsEmpty then
          String.trim model.tmpField
        else
          String.trim model.field
  in
      button [
                  -- onClick Action
                  onClick ( actionMsg actionStr )
--                  onClick ( model.onSuccess model.tmpField )
--                , disabled cfgNameIsEmpty
              --  , disabled (not enableSave)
                ] [ label ]
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
    opt s =
      option [ onClick (selectMsg s) ] [ text (txt s) ]
  in
    select []
        -- ( List.map (\ s -> option [ onClick (selectMsg s) ] [ text s ] ) model.entries )
        ( List.map opt model.entries )

{-------------------------------------------------------------------
  let
      fieldIsEmpty = String.isEmpty ( String.trim model.field )
      tmpFieldNameIsEmpty = String.isEmpty ( String.trim model.tmpField )
      enableSave = ---allowToSave &&
      ( not (
        fieldIsEmpty && tmpFieldNameIsEmpty
      ) )
  in
      button [
                  -- onClick Action
                  onClick ( actionMsg model.field )
--                  onClick ( model.onSuccess model.tmpField )
--                , disabled cfgNameIsEmpty
              --  , disabled (not enableSave)
                ] [ label ]
-------------------------------------------------------------------}

viewField : Model -> Html Msg
viewField model =
  let
      fieldIsEmpty = String.isEmpty ( String.trim model.field )
      tmpFieldNameIsEmpty = String.isEmpty ( String.trim model.tmpField )
      enableSave = ---allowToSave &&
      ( not (
        fieldIsEmpty && tmpFieldNameIsEmpty
      ) )
  in
{-------------------------------------------------------------------
-------------------------------------------------------------------}
                input [
                  type' "text"
                , value model.tmpField
                , onInput UpdateField
                , disabled ( not fieldIsEmpty )
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
