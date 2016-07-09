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

import Widget as W exposing (
    aRoot, aVertical, aHorizontal, aSwitch, aBool, aString
  , fmtList   --, fmtById
  )

import RSyncConfig exposing (..)

import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Task
import String
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE

main =
  Html.App.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }


-- MODEL

type alias Model =
  { id        : String
  , cfgName   : String
  , output    : String
  , debug     : Bool

  -- widgets
  , root      : W.Node
  }


--   Local:  rsync [OPTION...] SRC... [DEST]
--
--   Access via remote shell:
--     Pull: rsync [OPTION...] [USER@]HOST:SRC... [DEST]
--     Push: rsync [OPTION...] SRC... [USER@]HOST:DEST
--
--   Access via rsync daemon:
--     Pull: rsync [OPTION...] [USER@]HOST::SRC... [DEST]
--           rsync [OPTION...] rsync://[USER@]HOST[:PORT]/SRC... [DEST]
--     Push: rsync [OPTION...] SRC... [USER@]HOST::DEST
--           rsync [OPTION...] SRC... rsync://[USER@]HOST[:PORT]/DEST
--
--   Usages with just one SRC arg and no DEST arg will list the source files instead of copying.

init : (Model, Cmd Msg)
init =
  let
    ( root, nodes ) = aRoot "RSync" [
      RSyncConfig.init
    ] (fmtList "rsync {{}} # ..." " ")
  in
    ( Model "" "default" "" False root
    , Cmd.none )


-- UPDATE

type Msg =
    CallWidget W.Msg
    | Save
    | SaveSucceed SaveResult
    | SaveFail Http.Error
    | ToggleDebug Bool
    | EditCfgName String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      CallWidget wMsg ->
        let
          ( newRoot, cmd ) = W.update wMsg model.root
        in
          ( { model | root = newRoot }
          , Cmd.map CallWidget cmd
          )

{--------------------------------------}
      Save ->
        let
          data = W.treeToJson 2 model.root
        in
            ( { model | output = data }
            , saveJob model
            )
--------------------------------------}

      SaveSucceed sRes ->  -- String
            ( { model | output = toString sRes }
            , Cmd.none
            )

      SaveFail err ->  -- Http.Error
            ( { model | output = toString err }
            , Cmd.none
            )

      ToggleDebug dbg ->
            ( { model | debug = dbg }, Cmd.none )

      EditCfgName cName ->
            ( { model | cfgName = cName }
            , Cmd.none
            )


type alias SaveResult =
--  { id  : String
  { jid  : String
  , yid  : String
  , cmd  : String
  }

decodeSaved : JD.Decoder SaveResult
decodeSaved =
  JD.object3 SaveResult
--    ("id"  := JD.string)
    ("jid"  := JD.string)
    ("yid"  := JD.string)
    ("cmd" := JD.string)

saveJob : Model -> Cmd Msg
saveJob model =
  let
    url = "/job/RSync"
    --body_s = W.treeToJson 2 node
    body_s = W.jobAsJson 2 model.cfgName model.root
    postCall = Http.post decodeSaved url (Http.string body_s)
  in
    Task.perform SaveFail SaveSucceed postCall



-- VIEW

view : Model -> Html Msg
view model =
  let
    wTreeLI w =
      if model.debug then
        Html.App.map CallWidget (W.nodeAsHtmlLI w)
      else
        div [] []
    dbg =
      div [] [
        h4 [] [ text "debug" ]
      , ul [] ( [
          li [] [ text (W.cmdOf model.root) ]
        , li [] [
            label [] [ text "extensive" ]
          , input [ type' "checkbox", onCheck ToggleDebug ] []
          ]
        ] ++ [ wTreeLI model.root ] )
      ]
    ( rootName, rootView ) = W.viewRoot model.root
    jobName = model.cfgName
    jobNameEmpty = String.isEmpty ( String.trim jobName )
  in
    div [] [
--      Html.App.map CallWidget (W.view model.root)
      h2 [] [ text rootName ]
    , table [] [ tr [] [
        td [] [ label [] [ text "Configuration" ] ]
      , td [] [ input [
                  type' "text"
                , value model.cfgName
                , onInput EditCfgName
                ] [] ]
      , td [] [ button [ onClick Save, disabled jobNameEmpty ] [ text "Save" ] ]
      ] ]
    , Html.App.map CallWidget rootView
    , h3 [] [ text "Output" ]
    , text model.output
    , dbg
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

