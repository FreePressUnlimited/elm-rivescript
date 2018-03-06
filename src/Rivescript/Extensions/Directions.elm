module Rivescript.Extensions.Directions exposing
  ( delay, noreply, send )

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
        Ok (Just string, Just (delayTask delay_ deferred))
      Err error ->
        Err error


noreplyParser : Parser ()
noreplyParser =
  succeed ()
    |. symbol "<"
    |. keyword "noreply"
    |. symbol ">"


noreply : Processor
noreply data =
  case run noreplyParser data of
    Ok () ->
      Ok (Nothing, Nothing)
    Err error ->
      Err error


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
      Ok (Just string, Just (sendTask deferred))
    Err error ->
      Err error
