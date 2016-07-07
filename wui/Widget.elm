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
  --, Commander
  , gKidsSeq, gKidsById

  , update
  , viewTR
  , toJson
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
  , cmdr     : Commander
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

type Commander
  = BoolCmdr String String
    -- BoolCmdr cTrue cFalse
    -- cmdlet = if isTrue then cTrue else cFalse
  | StringCmdr String
    -- StringCmdr sFmt
    -- cmdlet = sprintf sFmt ( cmdOf kid )
  | KidsFmtCmdr String String
    -- KidsFmtCmdr sFmt sSep
    -- cmdlet = sprintf sFmt ( join ( ListOf ( cmdOf kid ) ) sSep )
    -- order of kids is unchanged / unchangable
  | KidsByIdCmdr String String
    -- KidsByIdCmdr sFmt sSep
    -- cmdlet = sprintf sFmt ( join ( ListOf ( cmdOf kid ) SortedById ) sSep )
    -- order of kids is sorted by their Ids
  | EmptyCmdr


gKidsSeq : String -> String -> Commander
gKidsSeq cmdFmt listSep =
  KidsFmtCmdr cmdFmt listSep

gKidsById : String -> String -> Commander
gKidsById cmdFmt listSep =
  KidsByIdCmdr cmdFmt listSep

aRoot : String -> String -> List Node -> ( Node, List Node )
aRoot label cmdFmt kidsList =
  let
    rootNode = Node "root" label RootCmd (KidsList kidsList) (StringCmdr cmdFmt)
    grandKids = flatKidsList rootNode
  in
    ( rootNode, rootNode :: grandKids )

--aVertical : String -> String -> String -> List Node -> Node
--aVertical id label cmdFmt kidsList =
aVertical : String -> String -> List Node -> Commander -> Node
aVertical id label kidsList cmdr =
--  Node (id ++ "-VG") label VerGroup (KidsList kidsList) (KidsFmtCmdr cmdFmt)  -- (StringCmdr cmdFmt)
  Node (id ++ "-VG") label VerGroup (KidsList kidsList) cmdr  -- (StringCmdr cmdFmt)

aHorizontal : String -> String -> String -> List Node -> Node
aHorizontal id label cmdFmt kidsList =
  Node (id ++ "-HG") label HorGroup (KidsList kidsList) (StringCmdr cmdFmt)

aSwitch : String -> String -> List Node -> Node
aSwitch id label kidsList =
  let
    optFirstKid = List.head kidsList
    fkid =
      case optFirstKid of
        Nothing  -> ""
        Just kid -> kid.id
  in
    Node (id ++ "-SW") label (Switch fkid) (KidsList kidsList) EmptyCmdr

aBool : Id -> String -> Bool -> String -> String -> Node
aBool id label flag cmdTrue cmdFalse =
  Node (id ++ "_B") label (BoolValue flag) (KidsList []) (BoolCmdr cmdTrue cmdFalse)

aString : Id -> String -> String -> Node
aString id label cmdFmt =
  Node (id ++ "_S") label (StringValue "") (KidsList []) (StringCmdr cmdFmt)

kids : Node -> List Node
kids node =
  case node.kids of
    KidsList kids_l ->
      kids_l

flatKidsList : Node -> List Node
flatKidsList node =
  List.foldl List.append [] (List.map flatKidsList (kids node))

toJson : Int -> Node -> String
toJson indent node =
  JE.encode indent (jsonValue node)

jsonValue : Node -> JE.Value
jsonValue node =
  let
    --(val, typ, boolState, strValue) =
    (val, typ) =
      case node.value of
        BoolValue b ->
          ( JE.bool b, "Bool"
          --, if b then 1 else 2, "!!!"
          )
        StringValue s ->
          ( JE.string s, "String"
          --, 0, s
          )
        RootCmd ->
          ( JE.string node.label, "Root"
          --, 0, "!!!"
          )
        VerGroup ->
          ( JE.string node.label, "VerticalGroup"
          --, 0, "!!!"
          )
        HorGroup ->
          ( JE.string node.label, "HorizontalGroup"
          --, 0, "!!!"
          )
        Switch sid ->
          ( JE.string sid, "Switch"
          --, 0, "!!!"
          )

    -- devowel = replace All (regex "[aeiou]") (\_ -> "")

{------------------------------------------------
    insertNodeValue sFmt nId nVal =
      RX.replace RX.All ( RX.regex ("{{" ++ kid.id ++ "}}") ) (\_ -> kid.) sFmt
    sprintf sFmt param =
      RX.replace RX.All (RX.regex "({{}}|%s)") (\_ -> param) sFmt
    kl = join "|" ( List.map (\ kid -> "{{" ++ kid.id ++ "}}") (kids node) )
    sprintfX sFmt param =
      RX.replace RX.All (RX.regex "({{}}|%s)") (\_ -> param) sFmt
    cmdlet =
      case node.cmdr of
        BoolCmdr cmdTrue cmdFalse ->
          if boolState == 1 then cmdTrue
          else if boolState == 2 then cmdFalse
          else ""
        StringCmdr cmdFmt ->
          sprintf cmdFmt strValue
        KidsFmtCmdr sFmt ->
          "!!! EMPTY !!!"
        EmptyCmdr ->
          "!!! EMPTY !!!"
------------------------------------------------}


    cmdlet = cmdOf node

    kids_l = kids node
    extra =
      if List.length kids_l > 0 then
        [ ( "kids", JE.list ( List.map jsonValue kids_l ) ) ]
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


cmdOf : Node -> String
cmdOf node =
  let
    sprintf1 str param =
      RX.replace RX.All (RX.regex "({{}}|%s)") (\_ -> param) str

    sprintf str sFmt param =
      RX.replace RX.All (RX.regex sFmt) (\_ -> param) str

    insertNodeValue str kid =
      -- RX.replace RX.All (RX.regex ("{{" ++ kid.id ++ "}}")) (\_ -> (cmdOf kid)) str
      sprintf str  ( "{{" ++ kid.id ++ "}}" )  ( cmdOf kid )

    cmdListOfKids node =
      List.map (\ kid -> cmdOf kid) (kids node)
    cmdsOfKids node listSep =
      join listSep ( cmdListOfKids node )

--    kidsByIdList node =
  --    List.map (\ k -> (k.id, k)) (kids node)
    kidsCmdletsByIdList node =
      List.map (\ k -> (k.id, cmdOf k)) (kids node)
    kidsCmdletsByIdDict node =
      Debug.log "kids Cmdlets By Id Dict" ( Dict.fromList ( kidsCmdletsByIdList node ) )
    kidsCmdletsListByIds node =
      snd ( List.unzip ( Dict.toList ( kidsCmdletsByIdDict node ) ) )

  in
      case node.cmdr of
        BoolCmdr cmdTrue cmdFalse ->
          case node.value of
            BoolValue b ->
              if b then cmdTrue
              else cmdFalse
            _ ->
              "!!! NEITHER TRUE NOR FALSE : " ++ (toString node.value)

        StringCmdr cmdFmt ->
          case node.value of
            StringValue strValue ->
              sprintf1 cmdFmt strValue
            _ ->
              "!!! NOT A STRING : " ++ (toString node.value)

        KidsFmtCmdr sFmt listSep ->
          -- sprintf1 sFmt (join lSep (List.map (\ kid -> cmdOf kid) (kids node)))
          sprintf1 sFmt ( cmdsOfKids node listSep )

        KidsByIdCmdr sFmt listSep ->
          -- cmdlet = sprintf sFmt ( join ( ListOf ( cmdOf kid ) SortedById ) sSep )
          -- order of kids is sorted by their Ids
          sprintf1 sFmt ( join listSep ( kidsCmdletsListByIds node ) )

        EmptyCmdr ->
          "!!! EMPTY : " ++ (toString node.cmdr)


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

        optSelectedKid = List.head ( List.filter (\ kid -> kid.id == sid ) kids_l )
        selectedKidTR =
          case optSelectedKid of
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

