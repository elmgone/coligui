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

module Widget exposing (
    Node, Msg
  , initRoot, initVer, initHor, initSwitch
  , initBool, initString
  , update, mapUpdate
  , viewTR
  , jsonValue
  )


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
  | VerGroup
  | HorGroup
  | Switch Id
  --| Tabs Id

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

initVer : String -> List Node -> Node
initVer id kidsList =
  Node id "Vertical Group" VerGroup (KidsList kidsList)

initHor : String -> List Node -> Node
initHor id kidsList =
  Node id "Horizontal Group" HorGroup (KidsList kidsList)

initSwitch : String -> List Node -> Node
initSwitch id kidsList =
  Node id "Switch Group" (Switch "") (KidsList kidsList)

initBool : Id -> String -> Bool -> Node
initBool id label flag =
  Node id label (BoolValue flag) (KidsList [])

initString : Id -> String -> Node
initString id label =
  Node id label (StringValue "") (KidsList [])

kids : Node -> List Node
kids node =
  case node.kids of
    KidsList kids_l ->
      kids_l

flatKidsList : Node -> List Node
flatKidsList node =
  List.foldl List.append [] (List.map flatKidsList (kids node))

jsonValue : Node -> JE.Value
jsonValue node =
  let
    val =
      case node.value of
        BoolValue b ->
          JE.bool b
        StringValue s ->
          JE.string s
        RootCmd ->
          JE.string node.label
        VerGroup ->
          JE.string node.label
        HorGroup ->
          JE.string node.label
        Switch sid ->
          JE.string sid
    
    kids_l = kids node
    extra =
      if List.length kids_l > 0 then
        [ ( "kids", JE.list ( List.map jsonValue kids_l ) ) ]
      else
        []
  in
    JE.object ( [
      ( "id", JE.string node.id )
    , ( "value", val )
    --, ( "active", JE.bool node.isActive )
    ] ++ extra )

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



-- UPDATE

type Msg =
    Modify Id Value
  --| Activate Bool

update : Msg -> Node -> ( Node, Cmd Msg )
update msg node =
  --let
    --(nKids, cmds) = List.unzip ( List.map (update msg) ( kids node ) )
  --in
    case msg of
      Modify id val ->
        if id == node.id then
          ( { node
              | value = Debug.log ( "update " ++ node.label ) val
              --, kids = KidsList nKids
            -- }, Cmd.batch cmds )
            }, Cmd.none )
        else
          --( node, Cmd.batch cmds )
          ( node, Cmd.none )


{-----------------------------------------------------------}
mapUpdate : (Node -> (Node, Cmd a)) -> Node -> (Node, Cmd a)
mapUpdate f node =
  let
    ( newNode, cmd )  = f node
    ( newKids, cmds ) = List.unzip ( List.map (mapUpdate f) (kids node) )
  in
    ( { newNode | kids = KidsList newKids }
    , Cmd.batch ( cmd :: cmds ) )
-----------------------------------------------------------}




-- VIEW

viewTR : Id -> Node -> Html Msg
viewTR parentId node =
  case node.value of
    BoolValue _ ->
      node2TR parentId node

    StringValue _ ->
      node2TR parentId node

    RootCmd ->
        tr [] [ td []
          [ h2 []
            [ a [ href "http://localhost:33333" ] [ text node.label ]
            ]
          , table []
            ( List.map (viewTR node.id) ( kids node ) )
          ] ]

    VerGroup ->
      {---------------------------------------------
      let
        x = List.map viewList ( kids node )
      in
        tr [] [ td [] [
          table [ title (node.label ++ " " ++ node.id) ]
            ( List.map viewTR ( kids node ) )
        ] ]
      ---------------------------------------------}

        tr [] [ td [] [
          table [ title (node.label ++ " " ++ node.id) ]
            ( List.map (viewTR node.id) ( kids node ) )
        ] ]

    HorGroup ->
      let
        kidsAsTR_l = List.map (viewTR node.id) ( kids node )
        --kidsAsTables_l = List.map (\ kidTR_l -> table [] kidTR_l) kidsAsTrLists_l
        --kidsAsTDs_l = List.map (\ kidAsTable -> td [] [ kidAsTable ]) kidsAsTables_l
        kidsAsTDs_l = List.map (\ kidAsTR -> td [] [ table [] [ kidAsTR ] ]) kidsAsTR_l
      in
        tr [ title (node.label ++ " " ++ node.id) ] kidsAsTDs_l
  
    Switch sid ->
      {-------------------------------------------}
      let
        kids_l = kids node
        kidsIdsAndLabels_l = List.map (\ k -> (k.id, k.label) ) kids_l
        mkRadioTR (id, lbl) =
          tr [] [
            td [] [ label [] [ text lbl ] ]
          , td [] [ input [ type' "radio", value id, name node.id
                    , onClick (selectSwitch node.id id)
                    ] [] ]
          ]
        kidsAsRadios_l = List.map mkRadioTR kidsIdsAndLabels_l
        -- kidsAsTRs_l = List.map (\ r -> tr [] [ td [] [ r ] ] ) kidsAsRadios_l
        
        optSelectedKid = List.head ( List.filter (\ kid -> kid.id == sid ) kids_l )
        selectedKidTR =
          case optSelectedKid of
            Nothing ->
              tr [ title "select one switch option" ] []
            Just kid ->
              -- node2TR node.id kid
              viewTR  node.id kid
      in
        tr [ title (node.label ++ " " ++ node.id) ] [
          td [] [ table [] kidsAsRadios_l ]
        , td [] [ table [] [ selectedKidTR ] ]
        ]
      -------------------------------------------}
  

node2TR : Id -> Node -> Html Msg
node2TR parentId node =
  let
    tds_l = List.map (\x -> td [] [x]) (viewList parentId node)
  in
    tr [ title (node.label ++ " " ++ node.id) ] tds_l


{-----------------------------------------------}
viewList : Id -> Node -> List (Html Msg)
viewList parentId node =
  let
    inputElement =
      case node.value of
        BoolValue flag ->
          input [ type' "checkbox", checked flag, onCheck (editBool node.id) ] []
        StringValue str ->
          input [ type' "text", value str, onInput (editString node.id) ] []
        RootCmd ->
          notImplemented node "viewList RootCmd"
        VerGroup ->
          --notImplemented node "viewList VerGroup"

--        mkRadioTR (id, lbl) =
--          tr [] [
--            td [] [ label [] [ text lbl ] ]
--          , td [] [ 
          
          input [ type' "radio", value parentId, name node.id
                    , onClick (selectSwitch node.id parentId)
                    ] []
                    
--                     ]
--          ]

        HorGroup ->
          notImplemented node "viewList HorGroup"
        Switch sid ->
          input [ type' "radio", checked False
          --, onCheck (editBool node.id)
          ] []
          --notImplemented node ("viewList Switch " ++ sid)
  in
    [ label [] [ text node.label ]
    , inputElement
    ]
-----------------------------------------------}

editBool id b =
  Modify id (BoolValue b)

editString id s =
  Modify id (StringValue s)

selectSwitch sid kid =
  Modify sid (Switch kid)

notImplemented : Node -> String -> Html Msg
notImplemented node errDesr =
  div [ {-color "red"-} ] [ text ( "ERROR: " ++ errDesr ++ " NOT IMPLEMENTED: " ++ node.label ++ ": " ++ ( toString node.value ) ) ]



{------------------------------------------------ }
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
--------------------------------------------------}

