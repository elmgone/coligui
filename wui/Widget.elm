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

module Widget exposing (..)
-- Node

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as JE
--import Json.Decode as JD

-- MODEL

type Value
  = BoolValue Bool
  | StringValue String

type alias Id = String

type alias Node =
  { id       : Id
  , label    : String
  , value    : Value
  , isActive : Bool
  , kids     : Kids
  }

type Kids
  = KidsList ( List Node )

initBool : Id -> String -> Bool -> Node
initBool id label flag =
  Node id label (BoolValue flag) True (KidsList [])

kids : Node -> List Node
kids node =
  case node.kids of
    KidsList kids_l ->
      kids_l

jsonValue : Node -> JE.Value
jsonValue node =
  JE.object [
    ( "id", JE.string node.id )
  , ( "active", JE.bool node.isActive )
  , ( "kids", JE.list ( List.map jsonValue ( kids node ) ) )
  ]



-- UPDATE

type Msg =
    Edit Value
  | Activate Bool

update : Msg -> Node -> ( Node, Cmd Msg )
update msg node =
    case msg of
      Edit val ->
        ( { node | value = (Debug.log node.label val)
          }, Cmd.none )

      Activate active ->
        ( { node | isActive = active  -- (Debug.log (model.label ++ " visible") vis)
          }, Cmd.none )


-- VIEW

{--}
viewList : Node -> List (Html Msg)   -- -> List (Html Msg)
viewList node =
  let
    inputElement =
      case node.value of
        BoolValue flag ->
          input [ type' "checkbox", checked flag, onCheck editBool ] []
        StringValue str ->
          input [ type' "text", value str, onInput editString ] []
  in
    [ label [] [ text node.label ]
    , inputElement
    ]
--}

editBool b =
  Edit (BoolValue b)

editString s =
  Edit (StringValue s)
