{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# HLINT ignore "Redundant $" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Widgets where

import Brick
  ( App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent),
    AttrMap,
    AttrName,
    BrickEvent (VtyEvent),
    EventM,
    Extent,
    Padding (Pad),
    Widget,
    attrMap,
    attrName,
    bg,
    clickable,
    continueWithoutRedraw,
    defaultMain,
    fg,
    hBox,
    halt,
    joinBorders,
    neverShowCursor,
    on,
    padBottom,
    padLeftRight,
    padTop,
    resizeOrQuit,
    setAvailableSize,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
  )
import qualified Brick as T
import Brick.Forms (checkboxField)
import Brick.Widgets.Border (border, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, centerLayer, hCenter, hCenterLayer, vCenterLayer)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import DataTypes
import Graphics.Vty (Event (EvKey), Key (KChar), blue, defAttr, white)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import Utility

data St = St {_currentState :: AppState, _lastReportedClick :: Maybe Name}

blueBgAttr :: AttrName
blueBgAttr = attrName "blueBg"

specificAttr :: AttrName
specificAttr = blueBgAttr <> attrName "specific"

btnWhiteOnBlue :: AttrName
btnWhiteOnBlue = attrName "btnWhiteOnBlue"

myMap :: AttrMap
myMap = attrMap defAttr [(attrName "btnWhiteOnBlue", white `on` blue)]

initialState :: St
initialState = St {_currentState = Overview, _lastReportedClick = Nothing}

makeLenses ''St

submitBtn :: Widget Name
submitBtn = button " SUBMIT " BtnSubmit

cancelBtn :: Widget Name
cancelBtn = button " CANCEL " BtnCancel

header :: Widget Name
header = hBox [str "Haskell Assessment [Haskell demo project]"] <=> hBorder

-- Home Screen [START]--
owerviewUI :: [Widget Name]
owerviewUI = [container (vBox [header, overviewBody])]

overviewBody :: Widget Name
overviewBody =
  vBox
    [ str "You must complete all sections within the said time.",
      str "Kindly note that timer starts as soon as you click on the start button.",
      padBottom (Pad 1) (str "To start the assessment, please click on 'Start'."),
      overviewTab,
      start
    ]

overviewTab :: Widget Name
overviewTab =
  joinBorders $
    withBorderStyle unicode $
      border $
        vLimit 5 $
          setAvailableSize (50, 10) $
            ( vBox [str "Section Title", hBorder, str "Monads"] <+> vBorder
                <+> vBox [str "No of Questions", hBorder, str "5"]
            )

start :: Widget Name
start = button " START " BtnStart

-- Home Screen [END]--

assessmentUI :: [Widget Name]
assessmentUI = [container (vBox [header, assessmentBody])]

assessmentBody :: Widget Name
assessmentBody = hBox [questionWidget] <=> hBox [cancelBtn <+> submitBtn]

questionWidget :: Widget Name
questionWidget =
  vBox
    [ str "1. What is the type of return?",
      str "[] Monad m => m a -> (a -> m b) -> m b",
      str "[] Monad m => a -> (a -> m b) -> m b",
      str "[] Monad m => m a -> a",
      str "[] Monad m => a -> m a"
    ]

drawUI :: St -> [Widget Name]
drawUI st = ui
  where
    ui = case st ^. currentState of
      Overview -> owerviewUI
      Running -> assessmentUI

-- Cancel -> initUI
-- Complete -> initUI

assessmentApp :: App St e Name
assessmentApp =
  App
    { appDraw = drawUI,
      appHandleEvent = resizeOrQuit,
      appStartEvent = return (),
      appAttrMap = const $ myMap,
      appChooseCursor = neverShowCursor
    }

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent ev@(T.MouseDown n _ _ loc) = do
  currentState .= case n of
    BtnStart -> Running
    BtnCancel -> Running
    BtnSubmit -> Running

renderUI :: IO ()
renderUI = void $ defaultMain assessmentApp initialState
