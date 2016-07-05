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

--import Param exposing (BoolP, StringP, BoolM, StringM)
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

init : (Model, Cmd Msg)
init =
  let
    werbose = W.initBool "w" "Werbose" False
    srcF = W.initString "srcF" "Source Folder"
    
    ( root, nodes ) = W.initRoot "RSync" [werbose, srcF]
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
          --( newRoot, cmd ) = W.update wMsg model.root
        in
          ( { model | root = newRoot
            }, Cmd.map CallWidget cmd
          )

{--------------------------------------}
      Run ->
        let
{--------------------------------------
          flags = JE.list ( List.map ( Param.asJsonValue JE.bool ) model.flags )
          strings = JE.list ( List.map ( Param.asJsonValue JE.string ) model.strings )
          
          data = JE.encode 2 ( JE.object [
              ( "flags", flags )
            , ( "strings", strings )
          ] )
--------------------------------------}
          
          data = JE.encode 2 ( W.jsonValue model.root )
        in
            ( { model
                | output = data  -- JE.encode 2 ( JE.list ( List.map ( Param.asJsonValue JE.bool ) model.flags ) )
              }, Cmd.none
            )
--------------------------------------}


-- VIEW

view : Model -> Html Msg
view model =
  table [] [
    Html.App.map CallWidget (W.viewTR model.root)
  , button [ onClick Run ] [ text "Run" ]
  , h3 [] [ text "Output" ]
  , text model.output
  , h4 [] [ text "debug" ]
  , text (JE.encode 2 ( W.jsonValue model.root ))

  {----------------------------------------- }
  , ul [] [
      li [] [ text (JE.encode 2 ( W.jsonValue model.root )) ]
    ]
  -----------------------------------------}
  
  ]

{----------------------------------------- }
  Html.App.map CallWidget (table [] [
    W.viewTR model.root
  , button [ onClick Run ] [ text "Run" ]
  ])
-----------------------------------------}


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

