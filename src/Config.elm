module Config exposing (..)

import Model exposing (Point, Size)



-- CONFIG


homeMapName : String
homeMapName =
    "DM6 Elm"


version : String
version =
    "0.2.0-snapshot"


date : String
date =
    "Sep 1, 2025"


newTopicPos : Point
newTopicPos =
    Point 186 180


mainFont : String
mainFont =
    "sans-serif"


toolbarFontSize : number
toolbarFontSize =
    14


contentFontSize : number
contentFontSize =
    13


footerFontSize : number
footerFontSize =
    13


topicWidth : number
topicWidth =
    156


topicHeight : number
topicHeight =
    28



-- also width/height of square icon box


topicW2 : Float
topicW2 =
    topicWidth / 2


topicH2 : Float
topicH2 =
    topicHeight / 2


topicSize : Size
topicSize =
    Size topicWidth topicHeight


topicLabelWeight : String
topicLabelWeight =
    "bold"



-- "normal"


topicDetailSize : Size
topicDetailSize =
    Size
        (topicWidth - topicHeight)
        -- detail width does not include icon box
        (topicLineHeight * contentFontSize + 2 * (topicDetailPadding + topicBorderWidth))


topicDetailMaxWidth : number
topicDetailMaxWidth =
    300


topicDetailPadding : number
topicDetailPadding =
    8


topicLineHeight : Float
topicLineHeight =
    1.5


topicDefaultText : String
topicDefaultText =
    "New Topic"


topicIconSize : number
topicIconSize =
    16


topicBorderWidth : number
topicBorderWidth =
    1


topicRadius : number
topicRadius =
    7


assocWidth : Float
assocWidth =
    1.5


assocRadius : number
assocRadius =
    14



-- should not be bigger than half topicSize height


assocColor : String
assocColor =
    "black"


assocDelayMillis : number
assocDelayMillis =
    200


whiteBoxRange : Size
whiteBoxRange =
    Size 250 150


whiteBoxRadius : number
whiteBoxRadius =
    14


whiteBoxPadding : number
whiteBoxPadding =
    12


blackBoxOffset : number
blackBoxOffset =
    5
