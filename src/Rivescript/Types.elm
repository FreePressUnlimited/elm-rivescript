module Rivescript.Types exposing (..)


import Task exposing (Task)


type alias Processor =
  String -> Maybe ( String, Maybe (Task Never String) )
