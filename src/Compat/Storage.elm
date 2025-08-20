module Compat.Storage exposing
    ( field2
    , maybeField2
    )

{-| Decode from either a "new" or an "old" key.
-}

import Json.Decode as D exposing (Decoder)


field2 : String -> String -> Decoder a -> Decoder a
field2 newKey oldKey decoder =
    D.oneOf
        [ D.field newKey decoder
        , D.field oldKey decoder
        ]


maybeField2 : String -> String -> Decoder a -> Decoder (Maybe a)
maybeField2 newKey oldKey decoder =
    D.oneOf
        [ D.map Just (D.field newKey decoder)
        , D.map Just (D.field oldKey decoder)
        , D.succeed Nothing
        ]
