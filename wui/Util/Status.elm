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

module Util.Status exposing (..)

import Html            exposing (..)
import Html.Events  -- exposing (..)
import Html.Attributes -- exposing (..)
import String

-- MODEL

type alias Model =
    { debug      : Bool
    }

{------------------------------
------------------------------}

init : Model
init =
  Model False


-- UPDATE

type Msg
  = ToggleDebug Bool


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleDebug dbg ->
      { model | debug = dbg } ! []



-- VIEW

{------------------------------------------------------------------
------------------------------------------------------------------}

viewDbgStr : String -> Model -> Html.Html Msg
viewDbgStr errStr model =
  let
    dbgInfoHtml =
      if model.debug then
          Html.text errStr
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
      , dbgInfoHtml
      ]

{------------------------------------------------------------------
viewDbgOptStr : Model -> Maybe String -> Html.Html Msg
viewDbgOptStr model optErrStr =
  let
    dbgInfoHtml =
      case optErrStr of
        Just errStr ->
          Html.text errStr
        Nothing ->
          Html.div [] []
  in
      Html.div [] [
        Html.label [] [ Html.text "debug" ]
      , Html.input [
          Html.Attributes.type' "checkbox"
          , Html.Attributes.checked model.debug
          , Html.Events.onCheck ToggleDebug
        ] []
      , dbgInfoHtml
      ]
------------------------------------------------------------------}
