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
  | RootCmd

type alias Id = String

type alias Node =
  { id       : Id
  , label    : String
  , value    : Value
  --, isActive : Bool
  , kids     : Kids
  }

type Kids
  = KidsList ( List Node )

initRoot : String -> List Node -> Node
initRoot label kidsList =
  -- Node id label (BoolValue flag) (KidsList [])
  Node "root" label RootCmd (KidsList kidsList)

initBool : Id -> String -> Bool -> Node
initBool id label flag =
  -- Node id label (BoolValue flag) True (KidsList [])
  Node id label (BoolValue flag) (KidsList [])

kids : Node -> List Node
kids node =
  case node.kids of
    KidsList kids_l ->
      kids_l

jsonValue : Node -> JE.Value
jsonValue node =
  JE.object [
    ( "id", JE.string node.id )
  --, ( "active", JE.bool node.isActive )
  , ( "kids", JE.list ( List.map jsonValue ( kids node ) ) )
  ]



-- UPDATE

type Msg =
    Edit Value
  --| Activate Bool

update : Msg -> Node -> ( Node, Cmd Msg )
update msg node =
    case msg of
      Edit val ->
        ( { node | value = Debug.log node.label val
          }, Cmd.none )

--      RootCmd ->
--        ( node, Cmd.none )

--      Activate active ->
--        ( { node | isActive = active  -- (Debug.log (model.label ++ " visible") vis)
--          }, Cmd.none )


-- VIEW

{--}
view : Node -> Html Msg
view node =
  case node.value of
    BoolValue flag ->
      -- input [ type' "checkbox", checked flag, onCheck editBool ] []
      --div [] [ text ( "ERROR: view BoolValue NOT IMPLEMENTED: " ++ node.label ++ ": " ++ ( toString node.value ) ) ]
      notImplemented node "view BoolValue"

    StringValue str ->
      -- input [ type' "text", value str, onInput editString ] []
      --div [] [ text ( "ERROR: view StringValue NOT IMPLEMENTED: " ++ node.label ++ ": " ++ ( toString node.value ) ) ]
      notImplemented node "view StringValue"

    RootCmd ->
        -- h2 [] [ text node.label ]
        div []
          [ h2 []
            [ a [ href "http://localhost:33333" ] [ text node.label ]
            ]
          , table []
            ( List.map node2TR ( kids node ) )
          ]
--}

node2TR : Node -> Html Msg
node2TR node =
  let
    --kids_l = viewList node
    --tds_l = List.map (\x -> td [] [x]) (Debug.log "kids" kids_l)
    -- tds_l = List.map (\kid -> td [] [ viewList kid ]) (Debug.log "kids" kids_l)
    tds_l = List.map (\x -> td [] [x]) (viewList node)
  in
    tr [] tds_l
    --tr [] ( List.map (\x -> td [] [x]) (viewList node) )


{--}
viewList : Node -> List (Html Msg)
viewList node =
  let
    inputElement =
      case node.value of
        BoolValue flag ->
          input [ type' "checkbox", checked flag, onCheck editBool ] []
        StringValue str ->
          input [ type' "text", value str, onInput editString ] []
        RootCmd ->
          notImplemented node "viewList RootCmd"
          --div [] [ text ( "ERROR: viewList RootCmd NOT IMPLEMENTED: " ++ node.label ++ ": " ++ ( toString node.value ) ) ]
          -- h2 [] [ text node.label ]
          --h2 [] [
            --a [ href "http://localhost:33333" ] [ text node.label ]
          --]
  in
    [ label [] [ text node.label ]
    , inputElement
    ]
--}

editBool b =
  Edit (BoolValue b)

editString s =
  Edit (StringValue s)

notImplemented : Node -> String -> Html Msg
notImplemented node errDesr =
  div [ {-color "red"-} ] [ text ( "ERROR: " ++ errDesr ++ " NOT IMPLEMENTED: " ++ node.label ++ ": " ++ ( toString node.value ) ) ]
