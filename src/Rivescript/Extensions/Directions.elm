module Rivescript.Extensions.Directions exposing
  ( delay, noreply, send
  )

{-| Processors for all directions described in the [Dexter documentation](http://docs.rundexter.com/writing/bot/directions/). These directions extend the behaviour and implement additional funcionality on top of the [RiveScript standard library](https://www.rivescript.com/docs/tutorial). Note that `<get>`, `<set>` and `<star>` are supported by RiveScript by default.

@docs delay

@docs noreply

@docs send
-}


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


{-| Processor that parses `delay` directions as follows:

    - Hello, world!<delay seconds=2.0>My name is Marvin.

  This response results in a messages sent to your subscription that wraps both the "Hello, World!" response and a command that sends a second message after two seconds, which contains the "My name is Marvin." response.
-}
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


{-| Processor that parses `noreply` directions as follows:

    - <noreply>

  This response results in a message sent to your subscription that contains no response and no command – i.e. `Ok (Nothing, Nothing)`. This ensures that your bot can refuse to reply to messages without throwing `ERR: No Reply Found` errors.
-}
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


{-| Processor that parses `noreply` directions as follows:

    - Hello, world!<send>My name is Marvin.

  This response results in a messages sent to your subscription that wraps both the "Hello, World!" response and a command that sends a second message immediately, which contains the "My name is Marvin." response. In behaviour, this is identical to:

    - Hello, world!<delay seconds=0.0>My name is Marvin.
-}
send : Processor
send data =
  case run sendParser data of
    Ok {string, deferred} ->
      Ok (Just string, Just (sendTask deferred))
    Err error ->
      Err error
