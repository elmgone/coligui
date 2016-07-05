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

import Widget as W

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
  --, visible   : Bool
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
    folder id name =
      W.initString (id ++ "F") (name ++ " Folder")
    host id name =
      --W.initString (destId++"H") (destName ++ " Host")
      W.initString (id ++ "H") (name ++ " Host")
    user id name =
      --W.initString (destId++"U") (destName ++ " User")
      W.initString (id ++ "U") (name ++ " User")
    nwport id name =
      --W.initString (destId++"P") (destName ++ " Port")
      W.initString (id ++ "P") (name ++ " Port")

    localFolder id name =
      folder (id ++ "L") (name ++ " Local")

    remoteShell id name =
      W.initVer (id ++ "RS") (name ++ " Remote Shell") [
        user    (id ++ "RS") (name ++ " Remote Shell")
      , host    (id ++ "RS") (name ++ " Remote Shell")
      , folder  (id ++ "RS") (name ++ " Remote Shell")
      ]
    
    srcLocation =
      W.initSwitch "src" [
        localFolder "src" "Source"
      , remoteShell "src" "Source"
      ]

  {-----------------------------------------------------
    werbose = W.initBool "w" "Werbose" False
    srcF    = W.initString "srcF" "Source Folder"
    verG1   = W.initVer "verG1" [ werbose, srcF ]
    
    recursive = W.initBool "r" "Recursive" False
    tgtF    = W.initString "tgtF" "Target Folder"
    horG1   = W.initHor "horG1" [ recursive, tgtF ]
    
    switch1   = W.initSwitch "switch1" [verG1, horG1]

    ( root, nodes ) = W.initRoot "RSync" [switch1]
  -----------------------------------------------------}
    
    ( root, nodes ) = W.initRoot "RSync" [srcLocation]
  in
    ( Model "" "" root
    , Cmd.none )


-- UPDATE

type Msg =
    CallWidget W.Msg
    | Run


--    Save
--  | CreateSucceed Model
--  | CreateFail Http.Error



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      CallWidget wMsg ->
        let
          updateNode = W.update wMsg
          ( newRoot, cmd ) = W.mapUpdate updateNode model.root
        in
          ( { model | root = newRoot }
          , Cmd.map CallWidget cmd
          )

{--------------------------------------}
      Run ->
        let
          data = JE.encode 2 ( W.jsonValue model.root )
        in
            ( { model | output = data }
            , Cmd.none
            )
--------------------------------------}


-- VIEW

view : Model -> Html Msg
view model =
  table [] [
    Html.App.map CallWidget (W.viewTR ".." model.root)
  , tr [] [ td [] [
      button [ onClick Run ] [ text "Run" ]
    , h3 [] [ text "Output" ]
    , text model.output
    , h4 [] [ text "debug" ]
    , text (JE.encode 2 ( W.jsonValue model.root ))
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

