module Rivescript.Types exposing (..)


import Task exposing (Task)


type alias Processor =
  String -> Maybe (String, Task Never String)
