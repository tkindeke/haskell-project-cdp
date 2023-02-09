{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
module Widgets where
import Brick
    ( bg,
      fg,
      attrMap,
      attrName,
      Widget,
      (<+>),
      (<=>),
      hBox,
      joinBorders,
      padBottom,
      padLeftRight,
      setAvailableSize,
      str,
      vBox,
      vLimit,
      withAttr,
      withBorderStyle,
      Padding(Pad), AttrMap, AttrName, App (appDraw, appStartEvent, appHandleEvent, appAttrMap, appChooseCursor, App), resizeOrQuit, neverShowCursor, on, padTop )
import Brick.Widgets.Border.Style ( unicode )
import Brick.Widgets.Border ( border, hBorder, vBorder )
import Graphics.Vty ( defAttr, blue, white )
import Brick.Widgets.Center (hCenterLayer, centerLayer, center, hCenter)

myMap :: AttrMap
blueBgAttr :: AttrName
specificAttr :: AttrName
btnWhiteOnBlue :: AttrName

blueBgAttr = attrName "blueBg"
specificAttr = blueBgAttr <> attrName "specific"
btnWhiteOnBlue = attrName "btnWhiteOnBlue"
--myMap = attrMap defAttr [ (blueBgAttr, bg blue) , (specificAttr, fg white)]

myMap = attrMap defAttr
    [ (attrName "btnWhiteOnBlue",   white `on` blue) ]

homeScreenUI :: Widget ()
homeScreenUI =
    centerLayer $
    withBorderStyle unicode $
    border $
    padLeftRight 1 $
    padBottom (Pad 1) $ 
    setAvailableSize (80, 500) $
    (hBox [str "Haskell Assessment [Haskell demo project]"] <=> hBorder <=>
     padTop (Pad 1) (str "This assessment has timer enabled.") <=> 
     str "You must complete all sections within the said time." <=>
     str "Kindly note that timer starts as soon as you click on the start button." <=>
     padBottom (Pad 1) (str "To start the assessment, please click on 'Start'.") <=> 
     table <=>
     hCenter startBtn)

table :: Widget ()
table =
    joinBorders $
    withBorderStyle unicode $ 
    border $
    vLimit 5 $
    setAvailableSize (50, 10) $
    (vBox [str "Section Title", hBorder, str "Section 1"] <+> vBorder <+>
    vBox [str "No of Questions", hBorder, str "30"] )

startBtn :: Widget ()
startBtn =
    padTop (Pad 1) $
    joinBorders $
    withBorderStyle unicode $ 
    border $
    vLimit 5 $
    setAvailableSize (50, 10) $
    padLeftRight 2 $
    str " START " 

homeScreenApp :: App () e ()
homeScreenApp =
    App { appDraw = const [homeScreenUI]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return ()
        , appAttrMap = const $ myMap
        , appChooseCursor = neverShowCursor
        }