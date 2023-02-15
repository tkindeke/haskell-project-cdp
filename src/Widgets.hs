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
import DataTypes ( Name(..), AppState(..) )
import Graphics.Vty (Event (EvKey), Key (KChar), blue, defAttr, white)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl ( (.=) )
import Lens.Micro.TH (makeLenses)
import Utility ( createContainer, createButton )


data St = St {_currentState :: AppState, _result :: Int }

blueBgAttr :: AttrName
blueBgAttr = attrName "blueBg"

specificAttr :: AttrName
specificAttr = blueBgAttr <> attrName "specific"

btnWhiteOnBlue :: AttrName
btnWhiteOnBlue = attrName "btnWhiteOnBlue"

myMap :: AttrMap
myMap = attrMap defAttr [(attrName "btnWhiteOnBlue", white `on` blue)]

initialState :: St
initialState = St {_currentState = Overview, _result = 65}

makeLenses ''St

header :: Widget Name
header = hBox [str "Haskell Assessment [Haskell demo project]"] <=> hBorder

--- Home Screen [START]---
owerviewUI :: [Widget Name]
owerviewUI = [createContainer (vBox [header, overviewBody])]

overviewBody :: Widget Name
overviewBody =
  vBox
    [ str "You must complete all sections within the said time.",
      str "Kindly note that timer starts as soon as you click on the start button.",
      padBottom (Pad 1) (str "To start the assessment, please click on 'Start'."),
      overviewTab,
      startBtn
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

startBtn :: Widget Name
startBtn = createButton " START " BtnStart

--- Assessment Screen [START]---
assessmentUI :: [Widget Name]
assessmentUI = [createContainer (vBox [header, assessmentBody])]

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

submitBtn :: Widget Name
submitBtn = createButton " SUBMIT " BtnSubmit

cancelBtn :: Widget Name
cancelBtn = createButton " CANCEL " BtnCancel

--- Result Screen [START]---
resultUI :: St -> [Widget Name]
resultUI st = [createContainer (vBox [header, resultBody st])]

resultBody :: St -> Widget Name
resultBody st = hBox [resultWidget st] <=> hBox [closeBtn]

resultWidget :: St -> Widget Name
resultWidget st = content
  where
    content = if st^.result >= 60 then
      vBox [str "Congratulations! you have successfuly completed the quiz",
            str "Total marks earned 40",
            str "Marks earned 40/50",
            str "Percentage obtained 80%"]
      else
        vBox [str "sorry, you failed the quiz",
            str "Total marks earned 40",
            str "Marks earned 40/50",
            str "Percentage obtained 30%"]

closeBtn :: Widget Name
closeBtn = createButton " CLOSE " BtnClose

--- Screen rendering ---
drawUI :: St -> [Widget Name]
drawUI st = ui
  where
    ui = case st ^. currentState of
      Overview -> owerviewUI
      Quiz -> assessmentUI
      Result -> resultUI st

assessmentApp :: App St e Name
assessmentApp =
  App
    { appDraw = drawUI,
      appHandleEvent = appEvent,
      appStartEvent = do
              vty <- T.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True,
      appAttrMap = const $ myMap,
      appChooseCursor = neverShowCursor
    }

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent ev@(T.MouseDown n _ _ loc) = do
  currentState .= case n of
    BtnStart -> Quiz
    BtnCancel -> Overview
    BtnSubmit -> Result
    BtnClose -> Overview
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = T.halt
appEvent _ = return ()

renderUI :: IO ()
renderUI = void $ defaultMain assessmentApp initialState
