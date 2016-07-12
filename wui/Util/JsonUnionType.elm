module JsonGames exposing (..)

import Html exposing (..)
import Json.Decode exposing (..)
import Json.Encode as JE

type Value
  = BoolValue Bool
  | StringValue String
  | RootCmd
  | Group Orientation
  | Switch Id

type alias Id = String

type Orientation = Vertical | Horizontal | CrossEyed

-- userID = oneOf [ map OldID int , map NewID string , null NoID ]



decValue =
  oneOf [
    object1 BoolValue ( "bool" := bool )
  , object1 StringValue ( "string" := string )
  , object1 identity ( "root" := null RootCmd )
  --, string ( null RootCmd )
  --, object1 Group ( "group" := bool )
  , object1 Group ( "group" := devOrient )
  , object1 Switch ( "switch" := string )
  ]

devOrient =
  --oneOf [
    map str2or string
  --]

str2or s =
  case s of
    "vertical" -> Vertical
    "horizontal" -> Horizontal
    _ -> CrossEyed

encValue v =
  case v of
    BoolValue b ->
      JE.object [ ("bool", JE.bool b ) ]
    StringValue s ->
      JE.object [ ("string", JE.string s ) ]
    RootCmd ->
      JE.object [ ("root", JE.null ) ]
      --JE.string "root"
--    Group vert ->
--      --JE.object [ ("group", JE.bool vert ) ]
--      if vert then
--      JE.object [ ("group", JE.string "vertical" ) ]
--      else
--        JE.object [ ("group", JE.string "horizontal" ) ]
    Group orient ->
      --JE.object [ ("group", JE.bool vert ) ]
      case orient of
        Vertical ->
          JE.object [ ("group", JE.string "vertical" ) ]
        Horizontal ->
          JE.object [ ("group", JE.string "horizontal" ) ]
        CrossEyed ->
          JE.object [ ("group", JE.string "xxx" ) ]
    Switch sw ->
      JE.object [ ("switch", JE.string sw ) ]


data = [
    RootCmd
  , Switch "left"
  --, Group True
  , Group Vertical
  , BoolValue False
  , StringValue "YESSSS"
  ]

dlis =
  List.map (\ v -> li [] [ text ( JE.encode 0 ( encValue v ) ) ] ) data
dul =
  ul [] dlis


res = [
     decodeString decValue "{\"bool\": true}"
  ,  decodeString decValue "{\"string\": \"hey\"}"
  ,  decodeString decValue "{\"root\": null}"
--  ,  decodeString decValue "{\"group\": false}"
  ,  decodeString decValue "{\"group\": \"horizontal\"}"
  ,  decodeString decValue "{\"switch\": \"opt1\"}"
  ]

res3 = "[ {\"bool\": true}, {\"string\": \"hey\"}, {\"root\": null}, {\"group\": \"ss\"}, {\"switch\": \"opt1\"} ]"
res3_l =
  decodeString ( list decValue ) res3

resli3 =
  case res3_l of
    Ok l ->
      List.map (\ v -> li [] [ text ( toString v ) ] ) l
    Err _ ->
      [ li [] [ text ( toString res3_l ) ]]

resul3 =
  ul [] resli3



res_sl = [
     "{\"bool\": true}"
  ,  "{\"string\": \"hey\"}"
  ,  "{\"root\": null}"
  ,  "{\"group\": \"h\"}"
  ,  "{\"switch\": \"opt1\"}"
  ]

resli =
  List.map (\ v -> li [] [ text ( toString v ) ] ) res

resul =
  ul [] resli

resli2 =
  List.map (\ s -> li [] [ text ( s ++ "  ==>>  " ++ toString ( decodeString decValue s ) ) ] ) res_sl

resul2 =
  ul [] resli2


main =
  -- toString res |> text
  div [] [
    dul, resul, resul2, resul3
  ]