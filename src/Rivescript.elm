module Rivescript exposing (parse)


import Parser exposing (run)

import Rivescript.Types exposing ( Direction(..), Message )
import Rivescript.Extensions.Directions as Directions


-- All parsers should be rewritter as:
-- parse : String -> (String, Task Error String)

parse : String -> Message
parse str =
  case run Directions.pipeline str of
    Ok { message, direction } ->
      { message = message
      , direction = Just direction
      }
    Err _ ->
      { message = str
      , direction = Nothing
      }
