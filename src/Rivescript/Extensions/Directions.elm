module Rivescript.Extensions.Directions exposing (delay)

import Process
import Task exposing (Task)
import Time


import Parser exposing
  ( Parser, run
  , (|.), (|=), succeed
  , ignore, oneOrMore
  , keyword, symbol, float, end
  , source, ignoreUntil
  )


import Parser.LanguageKit exposing
  ( LineComment(..), MultiComment(..)
  , whitespace
  )


import Rivescript.Types exposing (Processor)


type alias Delay =
  { now : String
  , wait : Float
  , later : String
  }


delayParser : Parser Delay
delayParser =
  succeed Delay
    |= (source <| ignoreUntil "<")
    |. keyword "delay"
    |. whitespace
      { allowTabs = True
      , lineComment = NoLineComment
      , multiComment = NoMultiComment
      }
    |. keyword "seconds"
    |. symbol "="
    |= float
    |. symbol ">"
    |= (source <| ignore oneOrMore (\_ -> True))
    |. end


delayTask : Float -> String -> Task Never String
delayTask wait payload =
  Process.sleep (wait * Time.second)
    |> Task.andThen (\_ -> Task.succeed payload)


delay : Processor
delay string =
    case run delayParser string of
      Ok {now, wait, later} ->
        Just (now, delayTask wait later)
      Err _ ->
        Nothing
