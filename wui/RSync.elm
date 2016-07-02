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

import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Task
import Json.Decode as JD exposing ((:=))
import Json.Encode

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
  , srcFolder : StringP
  , flags     : List BoolP
  --, visible   : Bool
  }

init : (Model, Cmd Msg)
init =
  let
    sfV = Param.init "srcF" "source folder" ""

    flags = [
      Param.init "v" "verbose" False
    , Param.init "r" "recursive" False
    ]
  in
    ( Model "" sfV flags
    , Cmd.none )


-- UPDATE

type Msg =
    ChangeFlag   Param.Id   (Param.Msg Bool)
  | SrcFolderMsg StringP    (Param.Msg String)
--  | ChangeString Param.Id    (Param.Msg String)

--    Save
--  | Edit
--  | EditPath String
--  | CreateSucceed Model
--  | CreateFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      ChangeFlag idx bMsg ->
        let
          newFlagsNCmds = List.map (Param.updateOne idx bMsg) model.flags
          ( newFlags, nCmds ) = List.unzip newFlagsNCmds
          nCmd = Cmd.map (ChangeFlag idx) (Cmd.batch nCmds)
        in
          ( { model | flags = newFlags -- (Debug.log model.label nVal)
            }, nCmd
          )

      SrcFolderMsg sfBV sMsg ->
        let
          (newSF, sfCmd) = Param.updateOne "srcF" sMsg sfBV
        in
          ( { model | srcFolder = newSF }
          , Cmd.map (SrcFolderMsg newSF) sfCmd
          )


-- VIEW

view : Model -> Html Msg
view model =
  let
    sfP = Debug.log "src folder" model.srcFolder
    sfVTr = viewOptStringTr (Just sfP)

    verboseP = Debug.log "verbose" (Param.get model.flags "v")
    verboseVTr = viewOptFlagTr verboseP

    recursiveP = Debug.log "recursive" (Param.get model.flags "r")
    recursiveVTr = viewOptFlagTr recursiveP
  in
    div [] [
      h1 [] [ text "RSync" ]
    , table [] [
        verboseVTr
      , sfVTr
      , recursiveVTr
      ]
    ]


{-- }
view : Model -> Html Msg
view model =
  let
    sfv = Debug.log "src folder" model.srcFolder
    sfView = Html.App.map (SrcFolderMsg model.srcFolder) (Param.viewString sfv)

    --viewOptFlag = Param.viewOneOpt Param.inputBool

    verboseP = Debug.log "verbose" (Param.get model.flags "v")
    verboseV = viewOptFlag verboseP
    --verboseV = Html.App.map (ChangeFlag verboseP.id) (viewOptFlag verboseP)

    recursiveP = Debug.log "recursive" (Param.get model.flags "r")
    recursiveV = viewOptFlag recursiveP
    --recursiveV = Html.App.map (ChangeFlag recursiveP.id) (viewOptFlag recursiveP)
  in
    div [] [
      h1 [] [ text "RSync" ]
    , verboseV
    , sfView
    , recursiveV
    ]
--}


{-- }
viewOne : ( Param.Model valType -> Html (Param.Msg valType) ) -> Param.Model valType -> Html Msg
viewOne inputOf param =
  let
    viewParam = Param.viewOne inputOf
  in
    --Html.App.map (ChangeFlag flag.id) (viewBool (Just flag))
    Html.App.map (ChangeFlag param.id) (viewParam param)
--}

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
      Html.App.map (SrcFolderMsg str) (Param.viewTR Param.inputString str)
--}

{--}
viewOptFlag : Maybe BoolP -> Html Msg
viewOptFlag optFlag =
  case optFlag of
    Nothing ->
      div [] []
    Just flag ->
      Html.App.map (ChangeFlag flag.id) (Param.viewOneOpt Param.inputBool optFlag)
--}

{-- }
viewFlag : BoolP -> Html Msg
viewFlag flag =
  let
    viewBool = Param.viewOne Param.inputBool
  in
    --Html.App.map (ChangeFlag flag.id) (viewBool (Just flag))
    Html.App.map (ChangeFlag flag.id) (viewBool flag)
--}

{-- }
-- viewList : Model valType -> List (Html (Msg valType)) -> List (Html (Msg valType))
viewTR : Param.Model valType
  -> ( Param.Model valType -> Html (Param.Msg valType) )
  -> Html (Param.Msg valType)
viewTR paramP inputOf =
  let
    -- list = Param.viewList paramP [ Param.inputBool paramP ]
    list = Param.viewList paramP [ inputOf paramP ]
    cells = List.map (\e -> td [] [ e ]) list
  in
    tr [] cells
    --Html.App.map (ChangeFlag paramP.id) ( tr [] cells )
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

