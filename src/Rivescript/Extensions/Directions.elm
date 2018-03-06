module Rivescript.Extensions.Directions exposing
  ( delay, send )

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


type alias Send =
  { string : String
  , deferred : String
  }


head : Parser String
head =
  source <| ignore oneOrMore (\c -> '<' /= c)


tail : Parser String
tail =
  source <| ignore oneOrMore (\_ -> True)
    |. end


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
delay data =
    case run delayParser data of
      Ok {string, delay_, deferred} ->
        Just (string, Just <| delayTask delay_ deferred)
      Err _ ->
        Nothing


sendParser : Parser Send
sendParser =
  succeed Send
    |= head
    |. symbol "<"
    |. keyword "send"
    |. symbol ">"
    |= tail


sendTask : String -> Task Never String
sendTask payload =
  Task.succeed payload


send : Processor
send data =
  case run sendParser data of
    Ok {string, deferred} ->
      Just (string, Just (sendTask deferred))
    Err _ ->
      Nothing
