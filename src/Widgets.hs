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
import qualified DataTypes as DT
import Graphics.Vty (Event (EvKey), Key (KChar), blue, defAttr, white)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl ( (.=) )
import Lens.Micro.TH (makeLenses)
import Utility ( createContainer, createButton )
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Maybe (fromJust)
import qualified System.IO as S



data St = St {_currentState :: AppState, _result :: Int, _config :: Cfg} deriving (Show)

data Cfg = Cfg {_title :: String, _notes :: String, _sectionTitle :: String, _questionNbr :: String} deriving (Show)

-- assessmentData :: Maybe Assessment
-- assessmentData = undefined

blueBgAttr :: AttrName
blueBgAttr = attrName "blueBg"

specificAttr :: AttrName
specificAttr = blueBgAttr <> attrName "specific"

btnWhiteOnBlue :: AttrName
btnWhiteOnBlue = attrName "btnWhiteOnBlue"

myMap :: AttrMap
myMap = attrMap defAttr [(attrName "btnWhiteOnBlue", white `on` blue)]

makeLenses ''St
makeLenses ''Cfg

header :: St -> Widget Name
header st = hBox [str $ st^.config.title] <=> hBorder

--- Home Screen [START]---
owerviewUI :: St -> [Widget Name]
owerviewUI st = [createContainer (vBox [header st, overviewBody st])]

overviewBody :: St -> Widget Name
overviewBody st =
  vBox
    [ 
      padBottom (Pad 1) (str $ st^.config.notes),
      overviewTab st,
      startBtn
    ]

overviewTab :: St -> Widget Name
overviewTab st =
  joinBorders $
    withBorderStyle unicode $
      border $
        vLimit 5 $
          setAvailableSize (50, 10) $
            ( vBox [str "Section Title", hBorder, str $ st^.config.sectionTitle] <+> vBorder
                <+> vBox [str "No of Questions", hBorder, str $  st^.config.questionNbr]
            )

startBtn :: Widget Name
startBtn = createButton " START " BtnStart

--- Assessment Screen [START]---
assessmentUI :: St -> [Widget Name]
assessmentUI st = [createContainer (vBox [header st, assessmentBody])]

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
resultUI st = [createContainer (vBox [header st, resultBody st])]

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
      Overview -> owerviewUI st
      Quiz -> assessmentUI st
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
renderUI = do
  handle <- S.openFile "/home/ninadu/Desktop/workspace/haskell/haskell-project-cdp/json/data.json" S.ReadMode
  fileContent <- S.hGetContents handle
  let byteContent = BLU.fromString fileContent
  let assessmentData = A.decode byteContent :: Maybe Assessment
  void (defaultMain assessmentApp . initState $ fromJust (assessmentData))
  S.hClose handle

initState :: Assessment -> St
initState a@(Assessment {assessmentTitle = at, section = s, instructions = i, questions = q}) =
  St
    { _config =
        Cfg
          { _title = at,
            _sectionTitle = s,
            _notes = i,
            _questionNbr = show (length q) 
          },
      _currentState = Overview,
      _result = 0
    }