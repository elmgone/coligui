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

--type alias BoolV = Param.BoolV

--type alias StringV = Param.StringV

-- import StringV

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
    init = init,  -- ( init "To Be or Not To Be" ),
    view = view,
    update = update,
    subscriptions = subscriptions
  }


-- MODEL

type alias Model =
  { id        : String
  , srcFolder : Param.Model String
  --, verbose   : Param.Model Bool
  , flags     : List (Param.Model Bool)
  --, visible   : Bool
  }

init : (Model, Cmd Msg)
init =
  let
    --( verbV, verbC ) = Param.init "verbose" False
    --vCM = Cmd.map (VerboseMsg verbV) verbC

    --( sfV, sfC ) = Param.init "source folder" ""
    --sfCM = Cmd.map (SrcFolderMsg sfV) sfC
    sfV = Param.init "source folder" ""

    --cmd = Cmd.batch ([vCM, sfCM])
  in
    (Model "" sfV []  -- True
    , Cmd.none)
    --, sfCM)
    --, cmd)


-- UPDATE

type Msg =
    --VerboseMsg   (Param.Model Bool)   (Param.Msg Bool)
    ChangeFlag   Param.Id   (Param.Msg Bool)
  | SrcFolderMsg (Param.Model String) (Param.Msg String)

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
          --newVerb = Param.updateModel bMsg verbBV
          newFlags = List.map (Param.updateModel idx bMsg) model.flags
        in
          --( { model | verbose = newVerb -- (Debug.log model.label nVal)
          ( { model | flags = newFlags -- (Debug.log model.label nVal)
            --}, Cmd.map (VerboseMsg newVerb) vCmd )
            }, Cmd.none )

{-- }
      VerboseMsg verbBV bMsg ->
        let
          -- (newVerb, vCmd) = BoolV.update bMsg verbBV
          newVerb = Param.updateModel bMsg verbBV
        in
          ( { model | verbose = newVerb -- (Debug.log model.label nVal)
            -- }, Cmd.map (VerboseMsg newVerb) vCmd )
            }, Cmd.none )
--}

      SrcFolderMsg sfBV sMsg ->
        let
          --(newSF, sfCmd) = StringV.update sMsg sfBV
          newSF = Param.updateModel "srcF" sMsg sfBV
        in
          ( { model | srcFolder = newSF
            --}, Cmd.map (SrcFolderMsg newSF) sfCmd )
            }, Cmd.none )

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

--updateFlag : Idx  model =
--updateFlag msg model =



-- VIEW

view : Model -> Html Msg
view model =
  let
    --vv = Debug.log "verbose" model.verbose
    --verbView = Html.App.map (VerboseMsg model.verbose) (Param.viewBool vv)

    sfv = Debug.log "src folder" model.srcFolder
    --sfView = Html.App.map (SrcFolderMsg model.srcFolder) (StringV.view sfv)
    sfView = Html.App.map (SrcFolderMsg model.srcFolder) (Param.viewString sfv)

  in
    div [] [
      h1 [] [ text "RSync" ]
    , sfView
    --, verbView
    ]

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

