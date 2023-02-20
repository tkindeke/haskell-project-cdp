{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BlockArguments #-}
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
import Brick.Forms (Form, checkboxField)
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
import Data.Text (Text)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import Brick.Widgets.List (List, listSelectedElement)


makeLenses ''St
makeLenses ''Cfg
makeLenses ''UserResult

header :: St -> Widget Name
header st = hBox [str $ st^.config.title] <=> hBorder

--- Home Screen [START]---
owerviewUI :: St -> [Widget Name]
owerviewUI st = [createContainer
                  (vBox [
                    header st,
                    vBox[
                      padBottom (Pad 1) (str $ st^.config.notes),
                        joinBorders $
                          withBorderStyle unicode $
                            border $
                              vLimit 5 $
                                setAvailableSize (50, 10) $
                                  ( vBox [str "Section Title", hBorder, str $ st^.config.sectionTitle] <+> vBorder
                                      <+> vBox [str "No of Questions", hBorder, str $  st^.config.questionsCount]
                                  ),
                      createButton " START " BtnStart
                    ]
                  ])
                ]

--- Assessment Screen [START]---
assessmentUI :: St -> [Widget Name]
assessmentUI st = [createContainer (vBox [header st] <=> (assessmentBody st))]

assessmentBody :: St -> Widget Name
assessmentBody st =  vBox [ T.padTopBottom 1 $ vLimit 25 $ L.renderList (drawListQuestion st) False (st^.listQuestions) ] <=>
                     hBox [createButton " CANCEL " BtnCancel <+> createButton " SUBMIT " BtnSubmit]

drawListQuestion :: St -> Bool -> Question -> Widget Name
drawListQuestion st _ q@(Question {questionTitle=qt, answers=a}) = padBottom (Pad 1) (str qt <=> vBox (map (drawAnswer st) a))

drawAnswer :: St -> Answer -> Widget Name
drawAnswer st a@(Answer {answerTitle=at}) = str ("   [] " ++ at)

--- Result Screen [START]---
resultUI :: St -> [Widget Name]
resultUI st = [createContainer (vBox [header st, resultBody st])]

resultBody :: St -> Widget Name
resultBody st = hBox [resultWidget st] <=> hBox [createButton " CLOSE " BtnClose]

resultWidget :: St -> Widget Name
resultWidget st = vBox [quizFeedBack,
                        str ("Total marks earned " ++ show(st^.result.resultScore)),
                        str ("Marks earned " ++ show(st^.result.resultScore) ++ "/" ++ st^.config.questionsCount),
                        str ("Percentage obtained " ++ show(st^.result.resultPercentage) ++ "%")]
  where
    quizFeedBack = if st^.result.resultPercentage >= 60 then
                      str "Congratulations! you have successfuly completed the quiz"
                   else
                      str "sorry, you failed the quiz"

--- Screen rendering ---
drawUI :: St -> [Widget Name]
drawUI st = case st ^. currentState of
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
      appAttrMap = const $ attrMap V.defAttr [],
      appChooseCursor = neverShowCursor
    }

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent ev@(T.MouseDown n e x loc) = do
  currentState .= case n of
    BtnStart -> Quiz
    BtnCancel -> Overview
    BtnSubmit -> Result
    BtnClose -> Overview
    LstQuestions -> Quiz
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = T.halt
appEvent _ = return ()

renderUI :: IO ()
renderUI = do
  handle <- S.openFile "data.json" S.ReadMode
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
            _questionsCount = show (length q) ,
            _quizQuestions = q
          },
      _currentState = Overview,
      _result = UserResult{
        _answer1 = "",
        _answer2 = "",
        _answer3 = "",
        _answer4 = "",
        _answer5 = "",
        _resultScore = 0,
        _resultPercentage = 0
      },
      _listQuestions = L.list LstQuestions (Vec.fromList(q)) 0,
      _selectedItem = ""
    }