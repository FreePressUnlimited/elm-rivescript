module Rivescript.Extensions.Directions exposing
  ( Direction(..)
  , parse
  )


import Parser exposing
  ( Parser, run, (|.), (|=)
  , ignore, succeed, end
  , keyword, symbol, int
  , oneOf, oneOrMore )


type Direction
  = Send
  | Delay Int
  | Noreply


parse : String -> Maybe Direction
parse str =
  case run (oneOf [send, delay, noreply]) str of
    Ok dir ->
      Just dir
    Err _ ->
      Nothing



send : Parser Direction
send =
  succeed Send
    |. keyword "send"
    |. end


delay : Parser Direction
delay =
  succeed Delay
    |. keyword "delay"
    |. whitespace
    |. keyword "seconds"
    |. symbol "="
    |= int
    |. end


noreply : Parser Direction
noreply =
  succeed Noreply
    |. keyword "noreply"
    |. end


whitespace : Parser ()
whitespace =
  ignore oneOrMore (\c -> c == ' ')
