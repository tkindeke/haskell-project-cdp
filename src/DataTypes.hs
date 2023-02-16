{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import Data.Text (Text)
import Data.Aeson ( FromJSON )
import GHC.Generics (Generic)
import Brick (attrName, AttrName)
import qualified Brick.AttrMap

data Name = BtnStart | BtnCancel | BtnSubmit | BtnClose
          deriving (Show, Ord, Eq)

data AppState = Overview | Quiz | Result
    deriving ( Eq, Show )

data Answer = Answer{
    answerTitle::Text,
    correct::Bool,
    selected::Bool
}deriving (Show,Generic)

data Question = Question{
    questionTitle::Text,
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