module Rivescript exposing (..)


import Rivescript.Types
import Rivescript.Extensions.Directions as Directions
import Rivescript.Extensions.Errors as Errors


import Task exposing (Task)


-- Re-export for import elsewhere
type alias Processor = Rivescript.Types.Processor


apply
  : List Processor
  -> String
  -> ( Maybe String, Maybe (Task Never String) )
apply pipeline string =
  case pipeline of
    [] ->
      (Just string, Nothing)
    proc :: rest ->
      Result.withDefault (apply rest string) (proc string)


directions : List Processor
directions =
  [ Directions.delay, Directions.noreply, Directions.send ]


errors : List Processor
errors =
  [ Errors.deeprecursion, Errors.noreply ]


extensions : List Processor
extensions =
  List.foldl List.append [] [ directions, errors ]
