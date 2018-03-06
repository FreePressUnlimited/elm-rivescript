module Rivescript.Extensions.Directions exposing (delay)

import Process
import Task exposing (Task)
import Time


import Parser exposing
  ( Parser, run
  , (|.), (|=), succeed
  , Count(..), ignore, oneOrMore
  , keyword, symbol, float, end
  , source
  )


import Parser.LanguageKit exposing
  ( LineComment(..), MultiComment(..)
  , whitespace
  )


import Rivescript.Types exposing (Processor)


type alias Delay =
  { string : String
  , delay_ : Float
  , deferred : String
  }


delayParser : Parser Delay
delayParser =
  succeed Delay
    |= head
    |. symbol "<"
    |. keyword "delay"
    |. ignore (Exactly 1) (\c -> ' ' == c)
    |. keyword "seconds"
    |. symbol "="
    |= float
    |. symbol ">"
    |= tail


delayTask : Float -> String -> Task Never String
delayTask wait payload =
  Process.sleep (wait * Time.second)
    |> Task.andThen (\_ -> Task.succeed payload)


delay : Processor
delay str =
    case run delayParser str of
      Ok {string, delay_, deferred} ->
        Just (string, Just <| delayTask delay_ deferred)
      Err _ ->
        Nothing


head : Parser String
head =
  source <| ignore oneOrMore (\c -> '<' /= c)


tail : Parser String
tail =
  source <| ignore oneOrMore (\_ -> True)
    |. end
