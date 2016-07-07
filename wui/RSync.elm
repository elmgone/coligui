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
  --, gKidsFmt
  , gKidsSeq, gKidsById
  )

import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Task
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
  , output    : String

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
{------------------------------------------------------------
    folder id = W.aString (id ++ "-F") "Folder" "%s"
    host   id = W.aString (id ++ "-H") "Host"   "%s"
    user   id = W.aString (id ++ "-U") "User"   "%s@"
    nwport id = W.aString (id ++ "-P") "Port"   ":%s"

    localFolder id =
      --aVertical (id ++ "-L") "Local" "" [
      aVertical (id ++ "-L") "Local" [
        folder (id ++ "-L")
      ] (gKidsFmt ("vg({{" ++ id ++ "-L" ++ "}})"))

    remoteShell id =
      let
        sid = id ++ "-RS"
      in
        aVertical sid "Remote Shell" "" [
          user    sid
        , host    sid
        , folder  sid
        ]

    remoteDaemon id =
      let
        did = id ++ "-RD"
      in
        aVertical did "Remote Daemon" "" [
          user    did
        , host    did
        , nwport  did
        , folder  did
        ]

    location id name =
      aSwitch id name [
        localFolder  id
      , remoteShell  id
      , remoteDaemon id
      ]

    srcLocation = location "src" "Source"
    tgtLocation = location "tgt" "Target"

    locations =
      aHorizontal "loc" "Location" "" [ srcLocation, tgtLocation ]
------------------------------------------------------------}

    verbose whose = aBool ("1verbose" ++ whose) "Verbose" False "--verbose" "--quiet"
    -- name whose = aString ("2name" ++ whose) (whose ++ " Name") (whose ++ "Name='{{}}'")
    fullname whose = aString ("2name" ++ whose) (whose ++ " Name") (whose ++ "Name='{{}}'")

    -- gKidsSeq, gKidsById
    fperson =
      aVertical "ps" "Person" [fullname "Her", verbose "Her"] (gKidsSeq ("seq({{}})") ", ")
    mperson =
      aVertical "ps" "Person" [fullname "His", verbose "his"] (gKidsById ("byId({{}})") ", ")

    ( root, nodes ) = aRoot "RSync" "rsync {{}}" [ fperson, mperson ]

  {-----------------------------------------------------
    werbose = aBool "w" "Werbose" False
    srcF    = aString "srcF" "Source Folder"
    verG1   = aVertical "verG1" [ werbose, srcF ]

    recursive = aBool "r" "Recursive" False
    tgtF    = aString "tgtF" "Target Folder"
    horG1   = aHorizontal "horG1" [ recursive, tgtF ]

    switch1   = aSwitch "switch1" [verG1, horG1]

    ( root, nodes ) = aRoot "RSync" [switch1]
  -----------------------------------------------------}

    --( root, nodes ) = aRoot "RSync" "rsync %s" [ verbose, locations ]
    --( root, nodes ) = aRoot "RSync" "rsync %s" [ verbose, name ]
  in
    ( Model "" "" root
    , Cmd.none )


-- UPDATE

type Msg =
    CallWidget W.Msg
    | Save
    | SaveSucceed SaveResult
    | SaveFail Http.Error


--    Save
--  | CreateSucceed Model
--  | CreateFail Http.Error



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      CallWidget wMsg ->
        let
          --updateNode = W.update wMsg
          --( newRoot, cmd ) = W.mapUpdate updateNode model.root
          ( newRoot, cmd ) = W.update wMsg model.root
        in
          ( { model | root = newRoot }
          , Cmd.map CallWidget cmd
          )

{--------------------------------------}
      Save ->
        let
          --data = JE.encode 2 ( W.jsonValue model.root )
          data = W.treeToJson 2 model.root
        in
            ( { model | output = data }
            , saveJob model.root
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



type alias SaveResult =
  { id  : String
  , cmd : String
  }

decodeSaved : JD.Decoder SaveResult
decodeSaved =
  JD.object2 SaveResult
    ("id"  := JD.string)
    ("cmd" := JD.string)

saveJob : W.Node -> Cmd Msg
saveJob node =
  let
    url = "/job/RSync"
    --body_s = JE.encode 2 (W.jsonValue node)
    body_s = W.treeToJson 2 node
    postCall = Http.post decodeSaved url (Http.string body_s)
  in
    Task.perform SaveFail SaveSucceed postCall



-- VIEW

view : Model -> Html Msg
view model =
  let
    dbg =
      div [] [
        h4 [] [ text "debug" ]
      , ul [] [ W.nodeAsLI model.root ]
      ]
      
  in
    table [] [
      Html.App.map CallWidget (W.viewTR ".." model.root)
    , tr [] [ td [] [
        button [ onClick Save ] [ text "Save" ]
      , h3 [] [ text "Output" ]
      , text model.output
      --, h4 [] [ text "debug" ]
      --, text ( W.toJson 2 model.root )
      --, text ("root = " ++ dbg)
      , Html.App.map CallWidget dbg
      ] ]

  {----------------------------------------- }
  , ul [] [
      li [] [ text (JE.encode 2 ( W.jsonValue model.root )) ]
    ]
  -----------------------------------------}

    ]


{----------------------------------------- }
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
---------------------------------------------}

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

