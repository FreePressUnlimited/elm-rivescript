module Rivescript.Types exposing (..)


import Task exposing (Task)

import Parser exposing (Error)


type alias Processor =
  String -> Result Error ( Maybe String, Maybe (Task Never String) )
