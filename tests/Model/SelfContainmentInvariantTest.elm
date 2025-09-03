module Model.SelfContainmentInvariantTest exposing (tests)

import Dict
import Expect
import Model
    exposing
        ( DisplayMode(..)
        , Map
        , MapItem
        , MapProps(..)
        , Point
        , Rectangle
        , Size
        , TopicProps
        )
import Model.Invariant as Invariant
import Test exposing (..)


tests : Test
tests =
    describe "Global invariant: no map contains itself"
        [ test "deliberate self-containment is detected" <|
            \_ ->
                let
                    emptyRect : Rectangle
                    emptyRect =
                        Rectangle 0 0 100 80

                    bogusAssocId : Int
                    bogusAssocId =
                        0

                    map1 : Map
                    map1 =
                        { id = 1
                        , rect = emptyRect
                        , items =
                            Dict.fromList
                                [ ( 1
                                  , MapItem
                                        1
                                        bogusAssocId
                                        False
                                        False
                                        (MapTopic (TopicProps (Point 0 0) (Size 60 40) (Container Model.WhiteBox)))
                                  )
                                ]
                        }

                    maps =
                        Dict.fromList [ ( 1, map1 ) ]
                in
                Expect.equal [ 1 ] (Invariant.offendingSelfContainers maps)
        ]
