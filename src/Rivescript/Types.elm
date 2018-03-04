module Rivescript.Types exposing
  ( Direction(..), Message
  )


type Direction
  = Send String
  | Delay Int String
  | Noreply


type alias Message =
  { message : String
  , direction : Maybe Direction
  }
