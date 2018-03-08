module Rivescript.Types exposing
  ( Processor, Pipeline
  )

{-| Supporting types for the Rivescript libraries.

@docs Processor

@docs Pipeline
-}


import Task exposing (Task)

import Parser exposing (Error)


{-| `Processor` is a type alias for a function that parses bot responses.
-}
type alias Processor
  = String
  -> Result Error ( Maybe String, Maybe (Task Never String) )


{-| A `Pipeline` is a list of [`Processor`](#Processor). Use [`apply`](Rivescript#apply) to run a string through a list of processors.
-}
type alias Pipeline
  = List Processor
