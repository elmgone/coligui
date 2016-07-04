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
import Html.App
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

initRoot : String -> List Node -> ( Node, List Node )
initRoot label kidsList =
  let
    rootNode = Node "root" label RootCmd (KidsList kidsList)
    grandKids = flatKidsList rootNode
  in
    ( rootNode, rootNode :: grandKids )
    

-- List.foldl : (a -> b -> b) -> b -> List a -> b

flatKidsList : Node -> List Node
flatKidsList node =
  List.foldl List.append [] (List.map flatKidsList (kids node))

get : Id -> List Node -> Node
get id nodes =
  let
    optNode = List.head ( List.filter (\ n -> id == n.id) nodes )
  in
    case optNode of
      Nothing ->
        initBool id "NOT FOUND" False
      Just node ->
        node


initBool : Id -> String -> Bool -> Node
initBool id label flag =
  -- Node id label (BoolValue flag) True (KidsList [])
  Node id label (BoolValue flag) (KidsList [])

initString : Id -> String -> Node
initString id label =
  Node id label (StringValue "") (KidsList [])

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
    Modify Value
  --| Activate Bool

{--
update : Msg -> Node -> ( Node, Cmd Msg )
update msg node =
    case msg of
      Modify val ->
        ( { node | value = Debug.log node.label val
          }, Cmd.none )

--      Activate active ->
--        ( { node | isActive = active  -- (Debug.log (model.label ++ " visible") vis)
--          }, Cmd.none )
--}

update : Msg -> Node -> ( Node, Cmd Msg )
update msg node =
  let
    (nKids, cmds) = List.unzip ( List.map (update msg) ( kids node ) )
  in
    case msg of
      Modify val ->
        ( { node
            | value = Debug.log ( "update " ++ node.label ) val
            , kids = KidsList nKids
          }, Cmd.batch cmds )


{--}
updateNode : Id -> Msg -> Node -> ( Node, Cmd Msg )
updateNode id msg node =
  let
    (nKids, cmds) = List.unzip ( List.map (updateNode id msg) ( kids node ) )
    cmd = Cmd.batch cmds

    newNode =
      if id == node.id then
        case msg of
          Modify val ->
            { node | value = Debug.log ( "update " ++ node.label ) val }

      else
        node
  in
        ( { newNode | kids = KidsList nKids }
        , cmd
        )

--}

mapUpdate : (Node -> Node) -> Node -> Node
mapUpdate f node =
  let
    newNode = f node
    newKids = List.map (mapUpdate f) (kids node)
  in
    { newNode | kids = KidsList newKids }


--mapView : (Node -> List transKid -> result) -> Node -> Html Msg
--mapView f node =

--mapView : Node -> Html Msg
mapView wMsg2Msg node =
  let
    viewNode =
      --if List.length node.kids
      viewTR
    
    kidsResult = List.map viewNode ( kids node )
    htmlResult = table [] kidsResult
  in
    Html.App.map wMsg2Msg htmlResult
    --htmlResult
    --case htmlResult of
      --Html msg ->
        --wMsg2Msg msg

{--
map : (Node -> List transKid -> result) -> Node -> result
map f node =
  let
    kidsResult = List.map (map f) (kids node)
    newNode = f node kidsResult
  in
    { newNode | kids = KidsList newKids }
--}


-- VIEW

viewTR : Node -> Html Msg
viewTR node =
  case node.value of
    BoolValue flag ->
      -- notImplemented node "view BoolValue"
      node2TR node

    StringValue str ->
      -- notImplemented node "view StringValue"
      node2TR node

    RootCmd ->
        tr []
          [ h2 []
            [ a [ href "http://localhost:33333" ] [ text node.label ]
            ]
          , table []
            ( List.map viewTR ( kids node ) )
          ]
  

{--}
view : Node -> Html Msg
view node =
  case node.value of
    BoolValue flag ->
      -- notImplemented node "view BoolValue"
      node2TR node

    StringValue str ->
      -- notImplemented node "view StringValue"
      node2TR node

    RootCmd ->
        div []
          [ h2 []
            [ a [ href "http://localhost:33333" ] [ text node.label ]
            ]
          , table []
            --( List.map node2TR ( kids node ) )
            ( List.map view ( kids node ) )
          ]
--}

node2TR : Node -> Html Msg
node2TR node =
  let
    tds_l = List.map (\x -> td [] [x]) (viewList node)
  in
    tr [] tds_l


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
  in
    [ label [] [ text node.label ]
    , inputElement
    ]
--}

editBool b =
  Modify (BoolValue b)

editString s =
  Modify (StringValue s)

notImplemented : Node -> String -> Html Msg
notImplemented node errDesr =
  div [ {-color "red"-} ] [ text ( "ERROR: " ++ errDesr ++ " NOT IMPLEMENTED: " ++ node.label ++ ": " ++ ( toString node.value ) ) ]
