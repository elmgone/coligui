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

import Param

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

type alias BoolP = Param.Model Bool
type alias StringP = Param.Model String

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

    --vCM = Cmd.map (VerboseMsg verbV) verbC
    --sfCM = Cmd.map (SrcFolderMsg sfV) sfC
    --cmd = Cmd.batch ([vCM, sfCM])
  in
    ( Model "" sfV flags
    , Cmd.none )
    --, sfCM)
    --, cmd)


-- UPDATE

type Msg =
    ChangeFlag   Param.Id   (Param.Msg Bool)
  | SrcFolderMsg StringP    (Param.Msg String)

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
          --(newSF, sfCmd) = StringV.update sMsg sfBV
          (newSF, sfCmd) = Param.updateOne "srcF" sMsg sfBV
        in
          ( { model | srcFolder = newSF
            }, Cmd.map (SrcFolderMsg newSF) sfCmd )

{-- }
      Save ->
        let
          newModel = { model
                     | path = model.editPath
                     , dirty = True
                     , editing = False
                     , errMsg = "(saving tag ..., e=F, d=T)"
                     }
        in
          ( Debug.log "Ok" newModel, saveTag newModel )
{ --}




-- VIEW

view : Model -> Html Msg
view model =
  let
    --vv = Debug.log "verbose" model.verbose
    flagsView = List.map viewFlag model.flags

    sfv = Debug.log "src folder" model.srcFolder
    sfView = Html.App.map (SrcFolderMsg model.srcFolder) (Param.viewString sfv)

  in
    div [] ( [
      h1 [] [ text "RSync" ]
    , sfView
    ] ++ flagsView )


viewFlag : BoolP -> Html Msg
viewFlag flag =
  Html.App.map (ChangeFlag flag.id) (Param.viewBool flag)


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

