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

module CoLiGui exposing (Model, Msg, init, update, view)

import Widget as W exposing (
    -- aRoot
    aSection
  , aVertical, aHorizontal, aSwitch, aBool, aString
  --, gKidsFmt
  , fmtList   --, fmtById
  )

import RSync

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



init : (Model, Cmd Msg)
init =
  let
  {-----------------------------------------------------}
    verbose whose = aBool ("1verbose" ++ whose) "Verbose" False "--verbose" "--quiet"
    fullname whose = aString ("2name" ++ whose) (whose ++ " Name") (whose ++ "Name='{{}}'")

    -- fmtList, fmtById
    fperson =
      aHorizontal "2fps" "Woman" [
        fullname "Her"
      , verbose "Her"
      ] (fmtList ("SHE:seq({{}})") ", ")
    
    mperson =
      aVertical "1mps" "Man" [
        fullname "His"
      , verbose "2his"
      ] (fmtList ("HE:byId({{}})") ", ")

    alt1 = aSwitch "alt1" "Alternative" [ fperson, mperson ]
  -----------------------------------------------------}


    
    --werbose = aBool "w" "Werbose" False "--werbose" "--quiet"
{-----------------------------------------------}

    jobSwitch : W.Node
    jobSwitch = aSwitch "jobTypes" "Job Types" [ RSync.init ]
    jobTypes : W.Node
    jobTypes = aSection 2 "Jobs" [
      jobSwitch   -- aSwitch "jobTypes" "Job Types" [ RSync.init ]
    ] (fmtList "jobs {{}}" " ")


    --- ( root, nodes ) = aRoot "Co Li GUI" [
    root = aSection 0 "Co Li GUI" [
      jobTypes
    ] (fmtList "coligui {{}} # ..." " ")
-----------------------------------------------}
    
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
      , ul [] [ W.nodeAsHtmlLI model.root ]
      ]
      
  in
    -- table [] [
    div [] [
      Html.App.map CallWidget (W.view model.root)
    , button [ onClick Save ] [ text "Save" ]
    , h3 [] [ text "Output" ]
    , text model.output
    , Html.App.map CallWidget dbg
    ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

