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
  , aRoot, aVertical, aHorizontal, aSwitch, aBool, aString
  --, Formatter
  , fmtKidsList, fmtKidsById

  , update
  , viewTR
  , treeToJson, nodeToJson  --, toJson

  , nodeAsHtmlLI
  )


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Json.Encode as JE
--import Json.Decode as JD
import Regex as RX   -- exposing (regex) as RX
import String exposing (..)
import Dict   -- as Di  --  exposing (..)

-- MODEL

type alias Node =
  { id       : Id
  , label    : String
  , value    : Value
  --, cmdFmt   : String
  , kids     : Kids
  , fmtr     : Formatter
  --, isActive : Bool
  }

type alias Id = String

type Kids
  = KidsList ( List Node )

type Value
  = BoolValue Bool
  | StringValue String
  | RootCmd
  | VerGroup
  | HorGroup
  | Switch Id
  --| Tabs Id

type Formatter
  = BoolFmtr String String
    -- BoolFmtr cTrue cFalse
    -- cmdlet = if isTrue then cTrue else cFalse
  | StringFmtr String
    -- StringFmtr sFmt
    -- cmdlet = sprintf sFmt ( cmdOf kid )
  | KidsListFmtr String String
    -- KidsListFmtr sFmt sSep
    -- cmdlet = sprintf sFmt ( join ( ListOf ( cmdOf kid ) ) sSep )
    -- order of kids is unchanged / unchangable
  | KidsByIdFmtr String String
    -- KidsByIdFmtr sFmt sSep
    -- cmdlet = sprintf sFmt ( join ( ListOf ( cmdOf kid ) SortedById ) sSep )
    -- order of kids is sorted by their Ids
  | SelectedKidFmtr  -- Id
  --| EmptyFmtr


fmtKidsList : String -> String -> Formatter
fmtKidsList cmdFmt listSep =
  KidsListFmtr cmdFmt listSep

fmtKidsById : String -> String -> Formatter
fmtKidsById cmdFmt listSep =
  KidsByIdFmtr cmdFmt listSep

aRoot : String -> List Node -> Formatter -> ( Node, List Node )
aRoot label kidsList fmtr =
  let
    rootNode = Node "root" label RootCmd (KidsList kidsList) fmtr
    grandKids = flatKidsList rootNode
  in
    ( rootNode, rootNode :: grandKids )

aVertical : String -> String -> List Node -> Formatter -> Node
aVertical id label kidsList fmtr =
  Node (id ++ "-VG") label VerGroup (KidsList kidsList) fmtr

aHorizontal : String -> String -> List Node -> Formatter -> Node
aHorizontal id label kidsList fmtr =
  Node (id ++ "-HG") label HorGroup (KidsList kidsList) fmtr

aSwitch : String -> String -> List Node -> Node
aSwitch id label kidsList =
  let
    optFirstKid = List.head kidsList
    fkid =
      case optFirstKid of
        Nothing  -> ""
        Just kid -> kid.id
  in
    Node (id ++ "-SW") label (Switch fkid) (KidsList kidsList) SelectedKidFmtr   -- EmptyFmtr

aBool : Id -> String -> Bool -> String -> String -> Node
aBool id label flag cmdTrue cmdFalse =
  Node (id ++ "_B") label (BoolValue flag) (KidsList []) (BoolFmtr cmdTrue cmdFalse)

aString : Id -> String -> String -> Node
aString id label cmdFmt =
  Node (id ++ "_S") label (StringValue "") (KidsList []) (StringFmtr cmdFmt)

kids : Node -> List Node
kids node =
  case node.kids of
    KidsList kids_l ->
      kids_l

flatKidsList : Node -> List Node
flatKidsList node =
  List.foldl List.append [] (List.map flatKidsList (kids node))

treeToJson : Int -> Node -> String
treeToJson indent node =
  JE.encode indent (jsonValueRec True node)

nodeToJson : Int -> Node -> String
nodeToJson indent node =
  JE.encode indent (jsonValueRec False node)

jsonValueRec : Bool -> Node -> JE.Value
jsonValueRec recurse node =
  let
    --(val, typ, boolState, strValue) =
    (val, typ) =
      case node.value of
        BoolValue b ->
          ( JE.bool b, "Bool" )
        StringValue s ->
          ( JE.string s, "String" )
        RootCmd ->
          ( JE.string node.label, "Root" )
        VerGroup ->
          ( JE.string node.label, "VerticalGroup" )
        HorGroup ->
          ( JE.string node.label, "HorizontalGroup" )
        Switch sid ->
          ( JE.string sid, "Switch" )

    cmdlet = cmdOf node

    kids_l = kids node
    extra =
      if recurse && List.length kids_l > 0 then
        [ ( "kids", JE.list ( List.map (jsonValueRec recurse) kids_l ) ) ]
      else
        []
  in
    JE.object ( [
      ( "id", JE.string node.id )
    , ( "label", JE.string node.label )
    , ( "type", JE.string typ )
    , ( "value", val )
    , ( "cmdlet", JE.string cmdlet )
    -- , ( "cmdFmt", JE.string node.cmdFmt )
    -- , ( "active", JE.bool node.isActive )
    ] ++ extra )


nodeAsHtmlLI : Node -> Html Msg
nodeAsHtmlLI node =
      li [] [ text ( nodeToJson 2 node )
      , kidsAsUL node
      ]

kidsAsUL : Node -> Html Msg
kidsAsUL node =
      ul [] ( List.map (\ k -> nodeAsHtmlLI k) ( kids node ) )


cmdOf : Node -> String
cmdOf node =
  let
    sprintf1 : String -> String -> String
    sprintf1 str param =
      RX.replace RX.All (RX.regex "({{}}|%s)") (\_ -> param) str

    sprintf : String -> String -> String -> String
    sprintf str sFmt param =
      RX.replace RX.All (RX.regex sFmt) (\_ -> param) str

    insertNodeValue : String -> Node -> String
    insertNodeValue str kid =
      -- RX.replace RX.All (RX.regex ("{{" ++ kid.id ++ "}}")) (\_ -> (cmdOf kid)) str
      sprintf str  ( "{{" ++ kid.id ++ "}}" )  ( cmdOf kid )

    cmdListOfKids : Node -> List String
    cmdListOfKids node =
      List.map (\ kid -> cmdOf kid) (kids node)

    cmdsOfKids : String -> Node -> String
    cmdsOfKids listSep node =
      join listSep ( cmdListOfKids node )

    kidsCmdletsByIdList : Node -> List (Id, String)
    kidsCmdletsByIdList node =
      List.map (\ k -> (k.id, cmdOf k)) (kids node)

    kidsCmdletsByIdDict : Node -> Dict.Dict Id String
    kidsCmdletsByIdDict node =
      Debug.log "kids Cmdlets By Id Dict" ( Dict.fromList ( kidsCmdletsByIdList node ) )

    kidsCmdletsListByIds : Node -> List String
    kidsCmdletsListByIds node =
      snd ( List.unzip ( Dict.toList ( kidsCmdletsByIdDict node ) ) )

    selectedId node =
      case node.value of
        Switch toKidId ->
          toKidId
        _ ->
          ""

    resultCmdlet =
      case node.fmtr of
        BoolFmtr cmdTrue cmdFalse ->
          case node.value of
            BoolValue b ->
              if b then cmdTrue
              else cmdFalse
            _ ->
              "!!! NEITHER TRUE NOR FALSE : " ++ (toString node.value)
              --Debug.crash ("!!! NEITHER TRUE NOR FALSE : " ++ (toString node.value))

        StringFmtr cmdFmt ->
          case node.value of
            StringValue strValue ->
              if strValue == "" then
                --strValue
                ""
              else
                sprintf1 cmdFmt strValue
            _ ->
              "!!! NOT A STRING : " ++ (toString node.value)
              --Debug.crash ("!!! NOT A STRING : " ++ (toString node.value))

        KidsListFmtr sFmt listSep ->
          sprintf1 sFmt ( cmdsOfKids listSep node )

        KidsByIdFmtr sFmt listSep ->
          -- cmdlet = sprintf sFmt ( join ( ListOf ( cmdOf kid ) SortedById ) sSep )
          -- order of kids is sorted by their Ids
          sprintf1 sFmt ( join listSep ( kidsCmdletsListByIds node ) )

        SelectedKidFmtr ->
          case (getSelectedKid (selectedId node) node) of
            Just kid ->
              cmdOf kid
            Nothing ->
              "!!! NOTHING SELECTED : " ++ (toString node.value)

{-------------------------------------------
        EmptyFmtr ->
          --Debug.crash ("!!! EMPTY : " ++ (toString node.fmtr))
          "!!! EMPTY : " ++ (toString node.fmtr)
-------------------------------------------}

  in
    Debug.log ("cmdOf " ++ node.id) resultCmdlet


getSelectedKid : Id -> Node -> Maybe Node
getSelectedKid sid node =
  -- optSelectedKid = List.head ( List.filter (\ kid -> kid.id == sid ) kids_l )
  List.head ( List.filter (\ kid -> kid.id == sid ) (kids node) )



get : Id -> List Node -> Node
get id nodes =
  let
    optNode = List.head ( List.filter (\ n -> id == n.id) nodes )
  in
    case optNode of
      Nothing ->
        aBool id "!!NOT FOUND!!" False "!!NOT FOUND - TRUE!!" "!!NOT FOUND - FALSE!!"
      Just node ->
        node



-- UPDATE

type Msg =
    Modify Id Value
  --| Activate Bool

update : Msg -> Node -> ( Node, Cmd Msg )
update msg node =
  mapUpdate (updateSingleNode msg) node

updateSingleNode : Msg -> Node -> ( Node, Cmd Msg )
updateSingleNode msg node =
  case msg of
    Modify id val ->
      let
        value = val
{------------------------------------
          case val of
            Switch sid ->
              val...
            _ ->
              val
{  ------------------------------------
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
------------------------------------}

      in
        if id == node.id then
          ( { node | value = Debug.log ( "update " ++ node.label ) value }
          , Cmd.none )
        else
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

{-----------------------------------------------------------
activateTree : Bool -> Node -> Node
activateTree act node =
  let
    nKids = List.map (activateTree act) (kids node)
  in
    { node
      | isActive = act
      , kids = KidsList nKids
    }
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
                    , checked (id == sid)
                    , onClick (selectSwitch node.id id)
                    ] [] ]
          ]
        kidsAsRadioTRs_l = List.map mkRadioTR kidsIdsAndLabels_l
        switchBoard = tr [] [ th [] [
          text node.label
        ]] :: kidsAsRadioTRs_l

        --optSelectedKid = List.head ( List.filter (\ kid -> kid.id == sid ) kids_l )
        --optSelectedKid = getSelectedKid node
        selectedKidTR =
          --case optSelectedKid of
          case (getSelectedKid sid node) of
            Nothing ->
              tr [ title "please select one switch option" ] [ td [] [
                text ("(please select one of the options for "
                  ++ node.label ++ ")")
              ] ]
            Just kid ->
              viewTR  node.id kid
      in
        tr [ title (node.label ++ " " ++ node.id) ] [
          td [] [ table [] switchBoard ]  -- kidsAsRadios_l ]
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

