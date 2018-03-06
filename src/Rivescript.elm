module Rivescript exposing (..)


import Rivescript.Types
import Rivescript.Extensions.Directions as Directions


import Task exposing (Task)


-- Re-export for import elsewhere
type alias Processor = Rivescript.Types.Processor


apply : List Processor -> String -> ( String, Maybe (Task Never String) )
apply pipeline string =
  case pipeline of
    [] ->
      (string, Nothing)
    proc :: rest ->
      Maybe.withDefault (apply rest string) (proc string)


directions : List Processor
directions =
  [ Directions.delay, Directions.send ]
