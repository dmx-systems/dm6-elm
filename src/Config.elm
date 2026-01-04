module Config exposing (..)

import ModelParts exposing (Point, Size)



-- CONFIG


rootBoxName = "DM6 Elm"
version = "0.4-snapshot"
date = "Jan 4, 2026"

appHeaderHeight = 36

mainFont = "sans-serif"
editorFont = "monospace"
toolFontSize = 15
contentFontSize = 13
footerFontSize = 13

hoverColor = "#d0d0d7" -- Firefox default hover button color (index.css)
disabledColor = "#b0b0b0" -- see button.tool:disabled (index.css)
topicLimboFilter = "contrast(0.3) brightness(1.5)"
assocLimboColor = "#858585"
topicBoxShadow = "gray 5px 5px 5px"
assocDropShadow =
  { dx = 2 -- SVG default
  , dy = 2 -- SVG default
  , stdDeviation = 2 -- SVG default
  , floodColor = "black" -- SVG default
  }

mapToolbarIconSize = 22
itemToolbarIconSize = 18
toolbarColor = "#e9e9ed" -- Firefox button color

initTopicPos = Point 104 72
initTopicText = "New Topic"
initTopicIcon = Just "disc"
initBoxText = "New Box"
initBoxIcon = Just "archive"

topicWidth = 156
topicHeight = 28 -- also width/height of square icon box
topicW2 = topicWidth // 2
topicH2 = topicHeight // 2
topicSize = Size topicWidth topicHeight
topicLabelWeight = "bold" -- "normal"
topicDetailSize = Size
  (topicWidth - topicHeight) -- detail width does not include icon box
  <| round (topicLineHeight * contentFontSize + 2 * (topicDetailPadding + topicBorderWidth))
topicDetailMaxWidth = 300
topicDetailPadding = 8
topicLineHeight = 1.5 -- float
topicIconSize = 16
topicBorderWidth = 1
topicRadius = 7

assocWidth = 2 -- float
assocHitArea = 20
assocRadius = 14 -- should not be bigger than half topicSize height
assocColor = "black"
assocDelayMillis = 200

whiteBoxRange = Size 250 150
whiteBoxRadius = 14
whiteBoxPadding = 12

blackBoxOffset = 5
