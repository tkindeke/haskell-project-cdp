{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import Data.Text (Text)
import Data.Aeson ( FromJSON )
import GHC.Generics (Generic)
import Brick (attrName, AttrName)
import qualified Brick.AttrMap
import Brick.Widgets.List (List)

type Label = String

data Name = BtnStart | BtnCancel | BtnSubmit | BtnClose | LstQuestions
          deriving (Show, Ord, Eq)

data AppState = Overview | Quiz | Result
    deriving ( Eq, Show )

data Answer = Answer{
    answerTitle::String,
    correct::Bool,
    selected::Bool
}deriving (Show,Generic)

data Question = Question{
    questionTitle::String,
    answers::[Answer],
    score::Int
}deriving (Show,Generic)

data Assessment = Assessment{
    assessmentTitle::String,
    section::String,
    instructions::String,
    questions::[Question],
    minPercentage::Int
} deriving (Show,Generic)

instance FromJSON Answer
instance FromJSON Question
instance FromJSON Assessment
data St = St {_selectedItem :: String, _currentState :: AppState, _listQuestions :: List Name Question, _config :: Cfg, _result :: UserResult} deriving (Show)

data Cfg = Cfg {_title :: String, _notes :: String, _sectionTitle :: String, _questionsCount :: String, _quizQuestions :: [Question]} deriving (Show)

data UserResult = UserResult
  { _answer1 :: String,
    _answer2 :: String,
    _answer3 :: String,
    _answer4 :: String,
    _answer5 :: String,
    _resultScore :: Int,
    _resultPercentage :: Int
  }
  deriving (Show)