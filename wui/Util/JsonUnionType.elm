module JsonGames exposing (..)

import Html exposing (..)
import Json.Decode exposing (..)
import Json.Encode as JE

type Value
  = BoolValue Bool
  | StringValue String
  | RootCmd
  | Group Bool
  | Switch Id
type alias Id = String
-- userID = oneOf [ map OldID int , map NewID string , null NoID ]

decValue =
  oneOf [
    object1 BoolValue ( "bool" := bool )
  , object1 StringValue ( "string" := string )
  , object1 identity ( "root" := null RootCmd )
  , object1 Group ( "group" := bool )
  , object1 Switch ( "switch" := string )
  ]


encValue v =
  case v of
    BoolValue b ->
      JE.object [ ("bool", JE.bool b ) ]
    StringValue s ->
      JE.object [ ("string", JE.string s ) ]
    RootCmd ->
      JE.object [ ("root", JE.null ) ]
    Group vert ->
      JE.object [ ("group", JE.bool vert ) ]
    Switch sw ->
      JE.object [ ("switch", JE.string sw ) ]


data = [
    RootCmd
  , Switch "left"
  , Group True
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
  ,  decodeString decValue "{\"group\": false}"
  ,  decodeString decValue "{\"switch\": \"opt1\"}"
  ]

res3 = "[ {\"bool\": true}, {\"string\": \"hey\"}, {\"root\": null}, {\"group\": false}, {\"switch\": \"opt1\"} ]"
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
  ,  "{\"group\": false}"
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