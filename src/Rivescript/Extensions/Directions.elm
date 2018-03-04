module Rivescript.Extensions.Directions exposing (pipeline)


import Parser exposing
  ( Parser, (|.), (|=)
  , ignore, ignoreUntil, andThen, succeed, end
  , keyword, symbol, int, source
  , oneOf, oneOrMore )


import Rivescript.Types exposing ( Direction(..) )


type alias Result =
  { message : String
  , direction : Direction
  }


pipeline : Parser Result
pipeline =
  succeed Result
    |= oneOf
      [ (source <| ignoreUntil "<") andThen send
      , (source <| ignoreUntil "<") andThen delay
      -- , (ignore 1) andThen noreply
      ]
    -- |= (source <| ignoreUntil "<")
    -- |= oneOf [ send, delay, noreply ]


send : Parser Direction
send =
  succeed Send
    |. keyword "send"
    |. symbol ">"
    |= (source <| ignore oneOrMore (\_ -> True))
    |. end


delay : Parser Direction
delay =
  succeed Delay
    |. keyword "delay"
    |. whitespace
    |. keyword "seconds"
    |. symbol "="
    |= int
    |. symbol ">"
    |= (source <| ignore oneOrMore (\_ -> True))
    |. end


noreply : Parser Direction
noreply =
  succeed Noreply
    |. keyword "noreply"
    |. symbol ">"
    |. end


whitespace : Parser ()
whitespace =
  ignore oneOrMore (\c -> c == ' ')
