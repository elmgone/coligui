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

-- import Widget as W -- exposing (..)

import Html -- exposing (..)
import Html.Events -- exposing (..)
import Html.Attributes -- exposing (..)
-- import Html.App
import String


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
  = UpdateField String
  | Select String
  | Success String
  | ToggleDebug Bool


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateField str ->
      { model | tmpField = str, field = "" }
        ! []

    Select str ->
      updateWith False str model
      {-------------------------------------------------------
      let
        nEntries = model.entries
      in
        { model
        | field = str
        , tmpField = ""
        --, entries = nEntries
        } ! []
      -------------------------------------------------------}

    Success str ->
      updateWith True str model
      {-------------------------------------------------------
      let
        nEntries =
          str :: List.filter (\ e -> e /= str ) model.entries
      in
        { model
        | field = str
        , tmpField = ""
        , entries = nEntries
        } ! []
      -------------------------------------------------------}

    ToggleDebug dbg ->
      { model | debug = dbg } ! []


updateWith : Bool -> String -> Model -> ( Model, Cmd Msg )
updateWith updateEntries str model =
      let
        m =
          if updateEntries then "Success" else "Select"
        s =
          if model.debug then Debug.log ( "CB." ++ m ) str
          else str
        nEntries =
          if updateEntries then
            str :: List.filter (\ e -> e /= s ) model.entries
          else
            model.entries
      in
        { model
        | field = str
        , tmpField = ""
        , entries = nEntries
        } ! []


-- VIEW

{------------------------------------------------------------------
------------------------------------------------------------------}

isEmptyString : String -> Bool
isEmptyString s =
  String.isEmpty ( String.trim s )

viewButton : (String -> Html.Html msg) -> (String -> msg) -> Model -> Html.Html msg
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
      Html.button [ Html.Events.onClick ( actionMsg actionStr )
             , Html.Attributes.disabled isBlocked
             ] [ labeller actionStr ]
{-------------------------------------------------------------------
-------------------------------------------------------------------}

viewOption : String -> (String -> msg) -> Model -> Html.Html msg
viewOption neutralEntry selectMsg model =
  let
    txt s =
      if s == "" then
        neutralEntry
      else
        s
    optAttrs s =
      ( Html.Attributes.selected ( s == model.field ) ) :: (
        -- if isEmptyString s || s == model.field then
        if s == model.field then
          []
        else
          [ Html.Events.onClick (selectMsg s) ]
      )
    opt s =
      Html.option ( optAttrs s ) [ Html.text (txt s) ]
  in
    Html.select []
        ( List.map opt model.entries )


viewField : Model -> Html.Html Msg
viewField model =
  let
      fieldIsEmpty = String.isEmpty ( String.trim model.field )
  in
{-------------------------------------------------------------------
-------------------------------------------------------------------}
                Html.input [
                  Html.Attributes.type' "text"
                , Html.Attributes.value model.tmpField
                , Html.Events.onInput UpdateField
                --, disabled ( not fieldIsEmpty )
                
        --, placeholder "What needs to be done?"
        --, name "newTodo"
        --, onInput UpdateField
        --, onEnter Add
          , Html.Attributes.autofocus True
                ] []



{--------------------------------------------------------------}
viewDbg : Model -> Html.Html Msg
viewDbg model =
  let
    dbgInfo =
      if model.debug then
        Html.text ( toString model )
      else
        Html.div [] []
  in
      Html.div [] [
        Html.label [] [ Html.text "debug" ]
      , Html.input [
          Html.Attributes.type' "checkbox"
          , Html.Attributes.checked model.debug
          , Html.Events.onCheck ToggleDebug
        ] []
      , dbgInfo
      ]

{--------------------------------------------------------------
------------------------------------------------------------}
