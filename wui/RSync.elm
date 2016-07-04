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

import Param exposing (BoolP, StringP, BoolM, StringM)
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
  --, srcFolder : StringP
  , flags     : List BoolP
  , strings   : List StringP
  --, visible   : Bool
  , output    : String
  
  -- widgets
  , werbose   : W.Node
  }

init : (Model, Cmd Msg)
init =
  let
    sfV = Param.init "srcF" "source folder" ""

    flags = [
      Param.init "v" "verbose" False
    , Param.init "r" "recursive" False
    ]

    strings = [
      Param.init "srcF" "source folder" ""
    ]
  in
    ( Model "" flags strings "" (W.initBool "w" "Werbose" False)
    , Cmd.none )


-- UPDATE

type Msg =
    ChangeFlag   Param.Id   (Param.Msg Bool)
--  | SrcFolderMsg StringP    (Param.Msg String)
  | ChangeString Param.Id    (Param.Msg String)
  | Run

  | CallWidget W.Id W.Msg

--    Save
--  | Edit
--  | EditPath String
--  | CreateSucceed Model
--  | CreateFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      CallWidget wId wMsg ->
        let
          -- FIXME: don't ignore wId - it must work for all flags, not just 'werbose'
          (newWerbose, wCmd) = W.update wMsg model.werbose
          cmd = Cmd.map (CallWidget wId) wCmd
        in
          ( { model | werbose = newWerbose
            }, cmd
          )

      ChangeFlag idx bMsg ->
        let
          newFlagsNCmds = List.map (Param.updateOne idx bMsg) model.flags
          ( newFlags, nCmds ) = List.unzip newFlagsNCmds
          nCmd = Cmd.map (ChangeFlag idx) (Cmd.batch nCmds)
        in
          ( { model | flags = newFlags
            }, nCmd
          )

      ChangeString id msg ->
        let
          newStringsNCmds = List.map (Param.updateOne id msg) model.strings
          ( newStrings, nCmds ) = List.unzip newStringsNCmds
          nCmd = Cmd.map (ChangeString id) (Cmd.batch nCmds)
        in
          ( { model | strings = newStrings
            }, nCmd
          )

      Run ->
        let
          flags = JE.list ( List.map ( Param.asJsonValue JE.bool ) model.flags )
          strings = JE.list ( List.map ( Param.asJsonValue JE.string ) model.strings )
          data = JE.encode 2 ( JE.object [
              ( "flags", flags )
            , ( "strings", strings )
          ] )
        in
            ( { model
                | output = data  -- JE.encode 2 ( JE.list ( List.map ( Param.asJsonValue JE.bool ) model.flags ) )
              }, Cmd.none
            )

{-- }
      SrcFolderMsg sfBV sMsg ->
        let
          (newSF, sfCmd) = Param.updateOne "srcF" sMsg sfBV
        in
          ( { model | srcFolder = newSF }
          , Cmd.map (SrcFolderMsg newSF) sfCmd
          )
--}

-- VIEW

view : Model -> Html Msg
view model =
  let
    flg id = viewOptFlagTr   (Param.get model.flags id)
    str id = viewOptStringTr (Param.get model.strings id)

    --sfP = Debug.log "src folder" (Param.get model.strings "srcF")
    
    werboseVL = List.map ( Html.App.map ( CallWidget "w" ) ) ( W.viewList model.werbose )
  in
    div [] [
      h1 [] [
        a [ href "http://localhost:33333" ] [ text "RSync" ]
      ]
    --, div [] werboseVL
    --, table [] [ tr [] ( List.map (\e -> td [] [ e ]) werboseVL ) ]
    
    , table [] [
        tr [] ( List.map (\e -> td [] [ e ]) werboseVL )
        
      , flg "v"
      , str "srcF"
      , flg "r"
      ]
    , button [ onClick Run ] [ text "Run" ]
    , h5 [] [ text "Debug" ]
    , ul [] [
        li [] [ text (toString model.strings) ]
      , li [] [ text (toString model.flags) ]
    ]
    , h4 [] [ text "Output" ]
    , text model.output
    ]


{--}
viewOptFlagTr : Maybe BoolP -> Html Msg
viewOptFlagTr optFlag =
  case optFlag of
    Nothing ->
      div [] []
    Just flag ->
      Html.App.map (ChangeFlag flag.id) (Param.viewTR Param.inputBool flag)
--}

{--}
viewOptStringTr : Maybe StringP -> Html Msg
viewOptStringTr optStr =
  case optStr of
    Nothing ->
      div [] []
    Just str ->
      Html.App.map (ChangeString str.id) (Param.viewTR Param.inputString str)
--}

{-- }
viewOptFlag : Maybe BoolP -> Html Msg
viewOptFlag optFlag =
  case optFlag of
    Nothing ->
      div [] []
    Just flag ->
      Html.App.map (ChangeFlag flag.id) (Param.viewOneOpt Param.inputBool optFlag)
--}

{-- }
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
{ --}

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

