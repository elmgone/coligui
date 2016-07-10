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

module Main exposing (Model, Msg, init, update, view)

import Widget          as W  exposing (..)
import JobType           --  exposing (..)
import RSync                 exposing (..)

import Html                  exposing (..)
import Html.App
import Html.Events           exposing (..)
import Html.Attributes       exposing (..)
import Http                  exposing (..)
import Task
import String
import Json.Decode     as JD exposing ((:=))
import Json.Encode     as JE

main =
  Html.App.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }


-- MODEL

type alias Model =
  {
  --id        : String
  --, cfgName   : String
    output    : String
  , debug     : Bool

  -- widgets
  , jobTypes  : JobType.Model
--  , rootNode  : W.Node
  , rsync     : RSync.Model
  , allowSave : Bool
  }

init : (Model, Cmd Msg)
init =
  let
--    ( root, nodes ) = aRoot "RSync" [
--      fst RSync.init
--    ] (fmtList "rsync {{}} # ..." " ")
    jt = JobType.init
  in
--    ( Model "" "default" "" False root
    ( Model "" False
      jt
      --( fst RSync.init ).root
      (fst RSync.init)
      True
    , Cmd.none )


-- UPDATE

type Msg =
    CallRSync   RSync.Msg
  | CallJobType JobType.Msg
--    | Save
--    | SaveSucceed SaveResult
--    | SaveFail Http.Error
    | ToggleDebug Bool
--    | EditCfgName String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      CallRSync rsMsg ->
        let
          ( newRoot, cmd ) = RSync.update rsMsg model.rsync
        in
          ( { model | rsync = newRoot }
          , Cmd.map CallRSync cmd
          )
      CallJobType jtMsg ->
        let
          ( newJT, cmd ) = JobType.update jtMsg model.jobTypes
        in
          ( { model | jobTypes = newJT }
          , Cmd.map CallJobType cmd
          )
      ToggleDebug dbg ->
            ( { model | debug = dbg }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  let
    --( rootName, rootView ) = W.viewRoot model.rootNode
    jt = JobType.view model.jobTypes

    rsyncHead = RSync.viewHead "Pick Name" model.rsync model.allowSave

    wTreeLI w =
      {----------------------------------------------------------
      if model.debug then
        Html.App.map CallRSync (W.nodeAsHtmlLI w)
      else
      ----------------------------------------------------------}
        div [] []

    dbg =
      div [] [
      {----------------------------------------------------------
        h4 [] [ text "debug" ]
      , ul [] ( [
          li [] [ text (W.cmdOf model.rootNode) ]
        , li [] [
            label [] [ text "extensive" ]
          , input [ type' "checkbox", onCheck ToggleDebug ] []
          ]
        ] ++ [ wTreeLI model.rootNode ] )
      ----------------------------------------------------------}
      ]
  in
    div [] [
      h2 [] [ text "RSync" ]
    , table [] [ tr [] [
          td [] [ Html.App.map CallJobType jt ]
        --, td [] [ Html.App.map CallRSync (RSync.viewHead "Pick Name" model.rsync) ]
        , td [] [ Html.App.map CallRSync rsyncHead ]
        ]
      ]
    , Html.App.map CallRSync (RSync.viewBody model.rsync)
    , dbg
    ]

  {-----------------------------------------------------------------
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
    {-----------------------------------------------------------------
    , table [] [ tr [] [
        td [] [ label [] [ text "Configuration" ] ]
      , td [] [ input [
                  type' "text"
                , value model.cfgName
                , onInput EditCfgName
                ] [] ]
      , td [] [ button [ onClick Save, disabled jobNameEmpty ] [ text "Save" ] ]
      ] ]
    -----------------------------------------------------------------}
    , Html.App.map CallWidget rootView
    , h3 [] [ text "Output" ]
    , text model.output
    , dbg
    ]
  -----------------------------------------------------------------}


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

