module Rivescript exposing (..)

-- All parsers should be rewritten as:
-- parse : String -> (String, Task Error String)


import Rivescript.Types exposing (Processor)
import Rivescript.Extensions.Directions as Directions


directions : List Processor
directions =
  [ Directions.delay ]
