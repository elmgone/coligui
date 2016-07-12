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

module RSync exposing (Model, Msg, init, update, viewHead, viewBody)

import Widget as W
{----------------------------------------------------------
  exposing (
    aRoot, aVertical, aHorizontal, aSwitch, aBool, aString
  , fmtList   --, fmtById
  )
----------------------------------------------------------}

import RSyncConfig exposing (..)

import ComboBox -- as CB
import Util
import JobType

import Html              exposing (..)
import Html.App
import Html.Events       exposing (..)
import Html.Attributes   exposing (..)
import Http              exposing (..)
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
  { id           : String
--  , cfgName      : String
--  , tmpCfgName   : String

  , combo        : ComboBox.Model
  , saveErr      : Maybe Http.Error

  , output       : String
  , debug        : Util.Model

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
    ( root, nodes ) = W.aRoot "RSync" [
      RSyncConfig.init
    ] (W.fmtList "rsync {{}} # ..." " ")
  in
    ( Model "" ComboBox.init Nothing "" Util.init root
    , Cmd.none )


-- UPDATE

type Msg =
    CallWidget W.Msg
  | ComboMsg ComboBox.Msg
  | DebugMsg Util.Msg
  | JobSelect String
  | JobSaveRequested String
  | SaveSucceed SaveJobResult
  | SaveFail Http.Error
  | JobsLoadRequested
  | LoadJobsFail Http.Error
  | LoadJobsSucceed JobType.LoadJobsResult

--    | Save
--    | ToggleDebug Bool
--    | EditCfgName String
--    | SetCfgName String

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

      ComboMsg cbMsg ->
        let
          ( newCombo, nCbMsg ) = ComboBox.update cbMsg model.combo
        in
          ( { model | combo = newCombo }
          , Cmd.map ComboMsg nCbMsg
          )

      DebugMsg dbgMsg ->
        let
          ( newDebug, nDbgMsg ) = Util.update dbgMsg model.debug
        in
          ( { model | debug = newDebug }
          , Cmd.map DebugMsg nDbgMsg
          )

      JobSelect str ->
        let
          msgStr = Debug.log "RSync.JobSelect" str
          ( nCombo, nCbMsg ) = ComboBox.update (ComboBox.Select msgStr) model.combo
        in
          { model | combo = nCombo } ! [ Cmd.map ComboMsg nCbMsg ]

      JobSaveRequested str ->
        let
          msgStr = Debug.log "RSync.JobSaveRequested" str

          saveCmdMsg =
            saveJob msgStr model

          -- ( nCombo, nCbMsg ) = ComboBox.update (ComboBox.Success msgStr) model.combo
          -- xCmdMsg =
          --  Cmd.map ComboMsg nCbMsg
        in
          -- { model | combo = nCombo } ! [ saveCmdMsg ]   -- Cmd.batch [ saveCmdMsg, xCmdMsg ] ]
          { model | output = "RSync.JobSaveRequested '" ++ str ++ "' ..." }
          ! [ saveCmdMsg ]   -- Cmd.batch [ saveCmdMsg, xCmdMsg ] ]

      SaveSucceed saveResult ->
        let
          ( nCombo, nCbMsg ) =
            ComboBox.update (ComboBox.Success saveResult.jobName) model.combo
          -- xCmdMsg =
            -- Cmd.map ComboMsg nCbMsg
        in
          { model
          | combo = nCombo
          , output = toString saveResult
          , saveErr = Nothing
          } ! [ Cmd.map ComboMsg nCbMsg ]
{--------------------------------------
            ( { model
              | output = toString sRes
              , saveErr = Nothing
              }
            , Cmd.none
            )
--------------------------------------}

      SaveFail err ->  -- Http.Error
            ( { model
              | output = toString err
              , saveErr = Just err
              }
            , Cmd.none
            )
      
      JobsLoadRequested ->
        let
          loadJobsCmdMsg =
            loadJobs model
        in
          { model | output = "RSync.JobsLoadRequested ..." }
          ! [ loadJobsCmdMsg ]   -- Cmd.batch [ saveCmdMsg, xCmdMsg ] ]

      LoadJobsSucceed loadedJobs ->
        let
          jobNames = List.map .name loadedJobs.jobType.jobs
          ( nCombo, nCbMsg ) =
            ComboBox.update (ComboBox.NewOptions jobNames) model.combo
        in
          { model
          | combo = nCombo
          , output = toString loadedJobs
          , saveErr = Nothing
          } ! [ Cmd.map ComboMsg nCbMsg ]

      LoadJobsFail err ->
        model ! []

{--------------------------------------}
loadJobs : Model -> Cmd Msg
loadJobs model =
  let
    url = "/jobs/RSync"
    -- body_s = W.jobAsJson 2 jobName model.root
    -- httpCall = Http.get decodeJobsLoaded url -- (Http.string body_s)
    httpCall = Http.get JobType.decodeLoadedJobs url -- (Http.string body_s)
  in
    Task.perform LoadJobsFail LoadJobsSucceed httpCall
--------------------------------------}

{--------------------------------------}
saveJob : String -> Model -> Cmd Msg
saveJob jobName model =
  let
    url = "/jobs/RSync"
    body_s = W.jobAsJson 2 jobName model.root
    postCall = Http.post decodeJobSaved url (Http.string body_s)
  in
    Task.perform SaveFail SaveSucceed postCall
--------------------------------------}



type alias SaveJobResult =
  { jsonId  : String
  , yamlId  : String
  , jobName : String
  , cmd     : String
  }

decodeJobSaved : JD.Decoder SaveJobResult
decodeJobSaved =
  JD.object4 SaveJobResult
    ("json-id"  := JD.string)
    ("yaml-id"  := JD.string)
    ("job-name" := JD.string)
    ("cmd"      := JD.string)

{--------------------------------------
type alias LoadJobsResult =
  { jsonId  : String
  , yamlId  : String
  , jobName : String
  , cmd     : String
  }

decodeJobsLoaded : JD.Decoder LoadJobsResult
decodeJobsLoaded =
  JD.object3 SaveJobResult
    ("json-id"  := JD.string)
    ("yaml-id"  := JD.string)
    ("job-name" := JD.string)
    ("cmd"      := JD.string)

decodeJobType : Json.Decode.Decoder Model
decodeJobType =
    Json.Decode.succeed Model
        |: ("jobs" := Json.Decode.list decodeJob)
        |: ("id" := Json.Decode.string)
        |: ("name" := Json.Decode.string)
--------------------------------------}


-- VIEW

view : Model -> Html.Html Msg
view model =
  let
    v = ""
  in
    Html.div [] [
{------------------------------------------------------------}
      Html.h2 [] [ Html.text "RSync" ]
    , viewHead "XxX" model True
    , viewBody model
    ]

viewBody : Model -> Html.Html Msg
viewBody model =
  let
    (n, v) = W.viewRoot model.root
  in
    Html.div [] [
      Html.App.map CallWidget v
    , Html.App.map DebugMsg ( Util.viewDbgStr model.output model.debug )
    ]

{------------------------------------------------------------
  let
    ( rootName, rootView ) = W.viewRoot model.root
    jobName = model.cfgName
    jobNameEmpty = String.isEmpty ( String.trim jobName )
  in
    div [] [
      table [] [ tr [] [
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
--    , dbg
    ]
------------------------------------------------------------}


selectJob : String -> Msg
selectJob str =
  JobSelect str

requestJobSave : String -> Msg
requestJobSave str =
  JobSaveRequested str


viewHead : String -> Model -> Bool -> Html.Html Msg
viewHead labelText model allowToSave =
  let
    errHtml =
      case model.saveErr of
        Just err ->
          Html.b [ Html.Attributes.style [
            --("backgroundColor", "green")
          --,
            ("color", "red")
--        , ("height", "90px")
--        , ("width", "100%")
          ] ] [
            Html.text ( "   !! " ++ ( toString err ) ++ " !!" )
          ]
        
        Nothing ->
          Html.div [] []

    saveJobButtonText selection =
      Html.div [] [
        Html.text ( "Save " )
      , Html.em [] [ Html.text ( selection ) ]
      , errHtml
      ]
  in
    Html.table [] [ Html.tr [] [
      Html.td [] [ Html.label [] [ Html.text "Job" ] ]
    , Html.td [] [ button [
                     onClick JobsLoadRequested
                   ] [ text "Load All" ] ]
    , Html.td [] [ ComboBox.viewOption "--" selectJob model.combo ]
    , Html.td [] [ ComboBox.viewButton saveJobButtonText requestJobSave model.combo ]
    , Html.td [] [ Html.label [] [ Html.text "New job name" ] ]
    , Html.td [] [ Html.App.map ComboMsg ( ComboBox.viewField model.combo ) ]
    , Html.td [] [ Html.App.map ComboMsg ( ComboBox.viewDbg model.combo ) ]
    ] ]

{------------------------------------------------------------
viewDbgStr : Maybe String -> Html.Html Msg
viewDbg optErrStr =
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
------------------------------------------------------------}


{------------------------------------------------------------
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
{------------------------------------------------------------
      h2 [] [ text rootName ]
    ,--}
      table [] [ tr [] [
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
--    , dbg
    ]
------------------------------------------------------------}



{------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
{------------------------------------------------------------
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
------------------------------------------------------------}
    ( rootName, rootView ) = W.viewRoot model.root
    jobName = model.cfgName
    jobNameEmpty = String.isEmpty ( String.trim jobName )
  in
    div [] [
{------------------------------------------------------------
      h2 [] [ text rootName ]
    ,--}
      table [] [ tr [] [
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
--    , dbg
    ]
------------------------------------------------------------}


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

