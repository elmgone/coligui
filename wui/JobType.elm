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

module JobType exposing (..)
{------------------------------------------------------- }
    Node, Msg, Id
  , aRoot, aVertical, aHorizontal, aSwitch, aBool, aBoolX, aBooT, aString
  --, Formatter
  , fmtList, fmtById   -- , fmtBool

  , update
  , view, viewRoot
  , treeToJson, nodeToJson  --, toJson
  , jobAsJson

  , nodeAsHtmlLI, cmdOf
  )
-------------------------------------------------------}

import Widget as W exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Json.Encode                               --as JE
import Json.Decode exposing ((:=))  --, (|:)) -- as JD
import Json.Decode.Extra exposing ((|:)) -- as JD
import Regex as RX   -- exposing (regex) as RX
import String exposing (..)
import Dict   -- as Di  --  exposing (..)

-- MODEL

{----------------------------------------------
----------------------------------------------}
type alias Model =
    { jobs : List Job
    , id   : String
    , name : String
    --, node : W.Node
    , cfgName      : String
    , tmpCfgName   : String
    }

init : Model
init =
  Model [
    Job [] "x-new" "Create New"
  , Job [] "x1" "default"
  , Job [] "x2" "hra"
  , Job [] "x3" "kati"
  ] "jt5" "JobTypes" "" "default"

{----------------------------------------------
type alias X1 =
    { JobType : Model
    }
----------------------------------------------}

decodeJobType : Json.Decode.Decoder Model
decodeJobType =
    Json.Decode.succeed Model
        |: ("jobs" := Json.Decode.list decodeJob)
        |: ("id" := Json.Decode.string)
        |: ("name" := Json.Decode.string)
{----------------------------------------------
decodeX1 : Json.Decode.Decoder X1
decodeX1 =
    Json.Decode.succeed X1
        |: ("job_type" := decodeJobType)
----------------------------------------------}

encodeJobType : Model -> Json.Encode.Value
encodeJobType record =
    Json.Encode.object
        [ ("jobs", Json.Encode.list <| List.map encodeJob record.jobs)
        , ("id", Json.Encode.string record.id)
        , ("name", Json.Encode.string record.name)
        ]
{----------------------------------------------
encodeX1 : X1 -> Json.Encode.Value
encodeX1 record =
    Json.Encode.object
        [ ("job_type", encodeJobType record.JobType)
        ]
----------------------------------------------}

type alias Job =
    { versions : List String
    , id : String
    , name : String
    }


{----------------------------------------------
type alias X1 =
    { Job : Job
    }
----------------------------------------------}

decodeJob : Json.Decode.Decoder Job
decodeJob =
    Json.Decode.succeed Job
        |: ("versions" := Json.Decode.list Json.Decode.string)
        |: ("id" := Json.Decode.string)
        |: ("name" := Json.Decode.string)
{----------------------------------------------
decodeX1 : Json.Decode.Decoder X1
decodeX1 =
    Json.Decode.succeed X1
        |: ("job" := decodeJob)
----------------------------------------------}

encodeJob : Job -> Json.Encode.Value
encodeJob record =
    Json.Encode.object
        [ -- ("versions", Json.Encode.list <| Json.Encode.list (Json.Encode.string record.versions))
          ("id", Json.Encode.string record.id)
        , ("name", Json.Encode.string record.name)
        ]

{----------------------------------------------
encodeX1 : X1 -> Json.Encode.Value
encodeX1 record =
    Json.Encode.object
        [ ("job", encodeJob record.Job)
        ]

----------------------------------------------}


-- UPDATE

type Msg =
  Load

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  table [] [ tr [] [
--    label [] [ text model.name ]
    td [] [ label [] [ text "Configuration" ] ]
  , td [] [ select []
      ( List.map (\ j -> option [] [ text j.name ] ) model.jobs ) ]
    {--------------------
    [
      option [] []
    , option [] []
    ]
    --------------------}
  ] ]
