module Compat.Display exposing
    ( DisplayConfig
    , Shape(..)
    , boxMonad
    , circleMonad
    , default
    )

{-| Small, app-wide display knobs.
Keep this as a compat shim so upstream refactors don’t leak.
-}


type Shape
    = Box
    | Circle
    | Rounded Int


type alias DisplayConfig =
    { monad : Shape
    , container : Shape
    , stroke : Float
    , padding : Float
    }


default : DisplayConfig
default =
    { monad = Circle
    , container = Rounded 10
    , stroke = 1.5
    , padding = 8
    }


{-| Prebaked variants you might enable from flags later.
-}
circleMonad : DisplayConfig -> DisplayConfig
circleMonad cfg =
    { cfg | monad = Circle }


boxMonad : DisplayConfig -> DisplayConfig
boxMonad cfg =
    { cfg | monad = Box }
