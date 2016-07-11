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

module ComboBox_test exposing (..)

import ComboBox

import Widget as W -- exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Cmd.Extra
import String exposing (..)


main =
  Html.App.program {
    init = init,
    view = view,
    update = update,
    subscriptions = \_ -> Sub.none
  }


-- MODEL

{----------------------------------------------
----------------------------------------------}
type alias Model =
    {
      combo : ComboBox.Model
    }

init : ( Model, Cmd Msg )
init =
    ( Model ComboBox.init, Cmd.none )


-- UPDATE

type Msg
  = ComboMsg ComboBox.Msg
  | Success String
  | Select String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ComboMsg cbMsg ->
      let
        ( nCombo, nCbMsg ) = ComboBox.update cbMsg model.combo
      in
        { model | combo = nCombo } ! [ Cmd.map ComboMsg nCbMsg ]

    Success str ->
      let
        msgStr = Debug.log "CB.Test.Success" str
        ( nCombo, nCbMsg ) = ComboBox.update (ComboBox.Success msgStr) model.combo
      in
        { model | combo = nCombo } ! [ Cmd.map ComboMsg nCbMsg ]

    Select str ->
      let
        msgStr = Debug.log "CB.Test.Select" str
        ( nCombo, nCbMsg ) = ComboBox.update (ComboBox.Select msgStr) model.combo
      in
        { model | combo = nCombo } ! [ Cmd.map ComboMsg nCbMsg ]

{----------------------------------------
onSuccess : String -> ComboBox.Msg
onSuccess chosen =
  ComboBox.Success ( Debug.log "CBT success" chosen )
----------------------------------------}

-- VIEW

view : Model -> Html Msg
view model =
  table [] [ tr [] [
    td [] [ ComboBox.viewOption "--" select model.combo ]
  , td [] [ ComboBox.viewButton (\ s -> text ( "Save '" ++ s ++ "' !" ) ) success model.combo ]
  , td [] [ label [] [ text "Test: Pick new" ] ]
  , td [] [ Html.App.map ComboMsg ( ComboBox.viewField model.combo ) ]
  , td [] [ Html.App.map ComboMsg ( ComboBox.viewDbg model.combo ) ]
  ] ]

select : String -> Msg
select str =
  Select str

success : String -> Msg
success str =
  Success str
