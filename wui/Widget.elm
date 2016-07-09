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
    Node, Msg, Id
  , aRoot, aVertical, aHorizontal, aSwitch, aBool, aBoolX, aBooT, aString
  --, Formatter
  , fmtList, fmtById   -- , fmtBool

  , update
  , view
  , treeToJson, nodeToJson  --, toJson
  , jobAsJson

  , nodeAsHtmlLI, cmdOf
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
  , descr    : String
  , value    : Value
  , kids     : Kids
  , fmtr     : Formatter
  }

type alias Id = String

type Kids
  = KidsList ( List Node )

type Value
  = BoolValue Bool
  | StringValue String
  | RootCmd
  --| Group Bool Bool
  | Group Bool
    -- Group isVertical showLabel -- default True False
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
    rootNode = Node "root" label "root node of the command" RootCmd (KidsList kidsList) fmtr
    grandKids = flatKidsList rootNode
  in
    ( rootNode, rootNode :: grandKids )

aVertical : String -> String -> List Node -> Formatter -> Node
aVertical id label kidsList fmtr =
  --Node (id ++ "-VG") label VerGroup (KidsList kidsList) fmtr
--  Node (id ++ "-VG") label (Group True False) (KidsList kidsList) fmtr
  Node (id ++ "-VG") label "a vertical grouping" (Group True) (KidsList kidsList) fmtr

aHorizontal : String -> String -> List Node -> Formatter -> Node
aHorizontal id label kidsList fmtr =
  --Node (id ++ "-HG") label HorGroup (KidsList kidsList) fmtr
--  Node (id ++ "-HG") label (Group False False) (KidsList kidsList) fmtr
  Node (id ++ "-HG") label "a horizontal grouping"(Group False) (KidsList kidsList) fmtr

aSwitch : String -> String -> List Node -> Node
aSwitch id label kidsList =
  let
    optFirstKid = List.head kidsList
    fkid =
      case optFirstKid of
        Nothing  -> ""
        Just kid -> kid.id
  in
    Node (id ++ "-SW") label "a switch" (Switch fkid) (KidsList kidsList) SelectedKidFmtr

--aBool : Id -> String -> Bool -> String -> String -> Node
--aBool id label flag cmdTrue cmdFalse =
--  Node (id ++ "_B") label (BoolValue flag) (KidsList []) (BoolFmtr cmdTrue cmdFalse)

--aBool : Id -> String -> String -> Bool -> String -> Node
--aBool id label descr flag cmdTrue =
--  Node (id ++ "_B") label descr (BoolValue flag) (KidsList []) (BoolFmtr cmdTrue "")
aBool : Id -> String -> String -> String -> Node
aBool id label descr cmdTrue =
  Node (id ++ "_B") label descr (BoolValue False) (KidsList []) (BoolFmtr cmdTrue "")

aBoolX : Id -> String -> String -> Bool -> String -> String -> Node
aBoolX id label descr flag cmdTrue cmdFalse =
  Node (id ++ "_BX") label descr (BoolValue flag) (KidsList []) (BoolFmtr cmdTrue cmdFalse)

aBooT : Id -> String -> String -> String -> Node
aBooT id label descr cmdTrue =
  aBoolX id label descr True cmdTrue ""

aString : Id -> String -> String -> String -> Node
aString id label descr cmdFmt =
  let
    strValue = StringValue (validateFormatForParam cmdFmt)
  in
    Node (id ++ "_S") label descr strValue (KidsList []) (StringFmtr cmdFmt)

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
        --Group isVertical showLabel ->
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

    rootNode = JE.object ( [
--    rootNode = [
        ( "id",          JE.string node.id )
      , ( "label",       JE.string node.label )
      , ( "description", JE.string node.descr )
      , ( "type",        JE.string typ )
      , ( "value",       val )
      , ( "cmdlet",      JE.string cmdlet )
      -- , ( "cmdFmt", JE.string node.cmdFmt )
      -- , ( "active", JE.bool node.isActive )
      ] ++ extra )
  in
    rootNode


--treeToJson : Int -> Node -> String
--treeToJson indent node =
--  JE.encode indent (jsonValueRec True node)


jobAsJson : Int -> Node -> String
jobAsJson indent node =
  let
  {-------------------------------------------- }
    --rootNode = JE.object ( [
    rootNode = [
        ( "id",          JE.string node.id )
      , ( "label",       JE.string node.label )
      , ( "description", JE.string node.descr )
      , ( "type",        JE.string typ )
      , ( "value",       val )
      , ( "cmdlet",      JE.string cmdlet )
      -- , ( "cmdFmt", JE.string node.cmdFmt )
      -- , ( "active", JE.bool node.isActive )
      ] ++ extra
  --------------------------------------------}
    rootNode = jsonValueRec True node

--  Job struct {
--    Id   string
--    Name string
--    Root Node
--  }

    job = JE.object ( [
        ( "name",          JE.string "hra" )
--    ,   ( "root",          JE.object rootNode )
    ,   ( "root",          rootNode )
    ] )
  in
    JE.encode indent job


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
      div [] ( [
        h2 [] [ a [ href "http://localhost:33333" ] [ text node.label ] ]
      ] ++ ( List.map view ( kids node ) ) )

    Group isVertical ->
      let
        showLabelXX = True
      in
        table [ title (toString node.value) ] (
          if isVertical then
            let
              -- one row
              ---row (lbl, cont) =
              row (cont, nd, showLabel) =
                ---tr [] [ mkLabel showLabel lbl, td [] [cont] ]
                tr [] [ mkLabel showLabel nd, td [] [cont] ]
              -- many rows
              rows node =
                List.map row (kidsListOfTuples node)
            in
                rows node

          else
            -- horizontal
            let
              (labels, conts) = kidsTupleOfLists node
            in
              --[ tr [] ( List.map (\ label -> mkLabel showLabel label ) labels )
              [ tr [] ( labels )
              , tr [] ( List.map (\ cont  -> td [] [cont] )  conts )
              ]
        )


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
          td [] [ table [] switchBoard ]
        , td [] [ table [] [ selectedKidTR ] ]
        ]
      -------------------------------------------}


--mkLabel : Bool -> String -> Html Msg
--mkLabel showLabel lblStr =
--  if showLabel && lblStr /= "" then
--    td [] [ text lblStr ]
--  else
--    td [ title lblStr ] []

mkLabel : Bool -> Node -> Html Msg
mkLabel showLabel node =
--  if showLabel && lblStr /= "" then
  if showLabel then
    td [ title node.descr ] [ text node.label ]
  else
    td [ title ( node.label ++ ": " ++ node.descr ) ] []


--kidsListOfTuples : Node -> List (String, Html Msg)
--kidsListOfTuples : Node -> List (Node, Html Msg)
kidsListOfTuples : Node -> List (Html Msg, Node, Bool)
kidsListOfTuples node =
  List.map viewTuple ( kids node )

--kidsTupleOfLists : Node -> (List String, List (Html Msg))
kidsTupleOfLists : Node -> (List (Html Msg), List (Html Msg))
kidsTupleOfLists node =
  --List.unzip (kidsListOfTuples node)
  let
    triple2htmlTuple (cont, kid, showLabel) =
      ( mkLabel showLabel kid, cont )
    tuples = List.map triple2htmlTuple (kidsListOfTuples node)
  in
    List.unzip (tuples)


{-----------------------------------------------}
node2Table : Node -> Html Msg
node2Table node =
  let
    --(lbl, cont) = viewTuple node
    (cont, nd, showLabel) = viewTuple node
    nTable node =
      table [ title (node.label ++ " " ++ node.id) ] [
        -- tr [] [ mkLabel True lbl, td [] [cont] ]
        tr [] [ mkLabel showLabel node, td [] [cont] ]
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




{-----------------------------------------------}
--viewTuple : Node -> (String, Html Msg)
viewTuple : Node -> (Html Msg, Node, Bool)
viewTuple node =
  let
    (content, showLabel) =
      case node.value of
        BoolValue flag ->
          ( input [ type' "checkbox", checked flag, onCheck (editBool node.id) ] []
          --, node.label
          , True
          )
        StringValue str ->
          ( input [ type' "text", value str, onInput (editString node.id) ] []
          --, node.label
          , True
          )
        RootCmd ->
          -- notImplemented node "viewList RootCmd"
          ( view node
          --, node.label
          , True
          )

        --Group isVertical showLabel ->
        Group isVertical ->
          ( view node
          --, if showLabel then node.label else ""
          --, node.label
          , True
          )

        Switch sid ->
          -- input [ type' "radio", checked False
          -- ] []
          ( view node
          --, ""  --  node.label
          --, node.label
          , False
          )
  in
    --( label [] [ text node.label ]
    --( label   -- node.label
    --, content
    --)
    ( content, node, showLabel )
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

