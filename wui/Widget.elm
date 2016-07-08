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
  , fmtList, fmtById   -- , fmtBool

  , update
  , view
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
  | Group Bool
    -- Group isVertical -- default True
    -- a horizontal or vertical group, the flag says if it's vertical: default is vertical
  --| VerGroup
  --| HorGroup
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


fmtList : String -> String -> Formatter
fmtList cmdFmt listSep =
  KidsListFmtr cmdFmt listSep

fmtById : String -> String -> Formatter
fmtById cmdFmt listSep =
  KidsByIdFmtr cmdFmt listSep

--fmtBool : String -> String -> Formatter
--fmtBool cmdTrue cmdFalse =
--  BoolFmtr cmdTrue cmdFalse


aRoot : String -> List Node -> Formatter -> ( Node, List Node )
aRoot label kidsList fmtr =
  let
    rootNode = Node "root" label RootCmd (KidsList kidsList) fmtr
    grandKids = flatKidsList rootNode
  in
    ( rootNode, rootNode :: grandKids )

aVertical : String -> String -> List Node -> Formatter -> Node
aVertical id label kidsList fmtr =
  --Node (id ++ "-VG") label VerGroup (KidsList kidsList) fmtr
  Node (id ++ "-VG") label (Group True) (KidsList kidsList) fmtr

aHorizontal : String -> String -> List Node -> Formatter -> Node
aHorizontal id label kidsList fmtr =
  --Node (id ++ "-HG") label HorGroup (KidsList kidsList) fmtr
  Node (id ++ "-HG") label (Group False) (KidsList kidsList) fmtr

aSwitch : String -> String -> List Node -> Node
aSwitch id label kidsList =
  let
    optFirstKid = List.head kidsList
    fkid =
      case optFirstKid of
        Nothing  -> ""
        Just kid -> kid.id
  in
    Node (id ++ "-SW") label (Switch fkid) (KidsList kidsList) SelectedKidFmtr

aBool : Id -> String -> Bool -> String -> String -> Node
aBool id label flag cmdTrue cmdFalse =
  Node (id ++ "_B") label (BoolValue flag) (KidsList []) (BoolFmtr cmdTrue cmdFalse)
--aBool : Id -> String -> Bool -> Formatter -> Node
--aBool id label flag fmtr =
--  Node (id ++ "_B") label (BoolValue flag) (KidsList []) fmtr  -- (BoolFmtr cmdTrue cmdFalse)

aString : Id -> String -> String -> Node
aString id label cmdFmt =
  let
    strValue = StringValue (validateFormatForParam cmdFmt)
  in
    Node (id ++ "_S") label strValue (KidsList []) (StringFmtr cmdFmt)

validateFormatForParam : String -> String
validateFormatForParam cmdFmt =
      if contains "{{}}" cmdFmt then
        ""
      else
        "!! format MUST contain '{{}}' !!"

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
        Group isVertical ->
          ( JE.string node.label
          , if isVertical then
              "VerticalGroup"
            else
              "HorizontalGroup"
          )
--        VerGroup ->
--          ( JE.string node.label, "VerticalGroup" )
--        HorGroup ->
--          ( JE.string node.label, "HorizontalGroup" )
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


{----------------------------------------
get : Id -> List Node -> Node
get id nodes =
  let
    optNode = List.head ( List.filter (\ n -> id == n.id) nodes )
  in
    case optNode of
      Nothing ->
        aBool id "!!NOT FOUND!!" False "!!NOT FOUND - TRUE!!" "!!NOT FOUND - FALSE!!"
        --aBool id "!!NOT FOUND!!" False (BoolFmtr "!!NOT FOUND - TRUE!!" "!!NOT FOUND - FALSE!!")
      Just node ->
        node
----------------------------------------}



-- UPDATE

type Msg =
    Modify Id Value

update : Msg -> Node -> ( Node, Cmd Msg )
update msg node =
  mapUpdate (updateSingleNode msg) node

updateSingleNode : Msg -> Node -> ( Node, Cmd Msg )
updateSingleNode msg node =
  case msg of
    Modify id val ->
      let
        value = val
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




-- VIEW

view : Node -> Html Msg
view node =
  case node.value of
    BoolValue _ ->
      -- node2TR node
      node2Table node

    StringValue _ ->
      -- node2TR node
      node2Table node

    RootCmd ->
        tr [] [ td []
          [ h2 []
            [ a [ href "http://localhost:33333" ] [ text node.label ]
            ]
          , table []
            ( List.map view ( kids node ) )
          ] ]

    Group isVertical->
      if isVertical then
        tr [] [ td [] [
          table [ title (node.label ++ " " ++ node.id) ]
            ( List.map view ( kids node ) )
        ] ]
      else
        let
          kidsAsTR_l = List.map view ( kids node )
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

        selectedKidTR =
          case (getSelectedKid sid node) of
            Nothing ->
              tr [ title "please select one switch option" ] [ td [] [
                text ("(please select one of the options for "
                  ++ node.label ++ ")")
              ] ]
            Just kid ->
              view kid
      in
        tr [ title (node.label ++ " " ++ node.id) ] [
          td [] [ table [] switchBoard ]  -- kidsAsRadios_l ]
        , td [] [ table [] [ selectedKidTR ] ]
        ]
      -------------------------------------------}


{-----------------------------------------------}
node2Table : Node -> Html Msg
node2Table node =
  let
    nTable node =
      table [ title (node.label ++ " " ++ node.id) ] [
        tr [] ( List.map (\x -> td [] [x]) (viewList node) )
      ]
  in
    case node.value of
      BoolValue _ ->
        nTable node
      StringValue _ ->
        nTable node
      _ ->
        notImplemented node "node2Table"

-----------------------------------------------}


{-----------------------------------------------
node2TR : Node -> Html Msg
node2TR node =
  let
    tds_l = List.map (\x -> td [] [x]) (viewList node)
  in
    tr [ title (node.label ++ " " ++ node.id) ] tds_l
-----------------------------------------------}


{-----------------------------------------------}
viewList : Node -> List (Html Msg)
viewList node =
  let
    content : Html Msg
    content =
      case node.value of
        BoolValue flag ->
          input [ type' "checkbox", checked flag, onCheck (editBool node.id) ] []
        StringValue str ->
          input [ type' "text", value str, onInput (editString node.id) ] []
        RootCmd ->
          -- notImplemented node "viewList RootCmd"
          view node

        Group isVertical ->
          -- notImplemented node ("viewList " ++ (toString node.value))
          view node

        Switch sid ->
          -- input [ type' "radio", checked False
          -- ] []
          view node
  in
    [ label [] [ text node.label ]
    , content
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

