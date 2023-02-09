{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
module Main where
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Graphics.Vty
  ( Vty
  , Picture(..)
  , Cursor(..)
  , Event(..)
  , update
  , outputIface
  , displayBounds
  , shutdown
  , nextEvent
  , mkVty
  , defaultConfig
  , restoreInputState
  , inputIface
  )

import Brick.Util (fg, on)
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core 
  ( (<=>)
  , (<+>)
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
  )

import Brick
    ( Widget,
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
      withBorderStyle,
      Padding(Pad),
      customMain,
      simpleApp,
      App )
import Brick.Widgets.Center ( center )
import Brick.Widgets.Border ( border, hBorder, vBorder )
import Brick.Widgets.Border.Style ( unicode )
import Control.Monad ()
import Data.Semigroup (Min(Min))

displayScreen :: (Ord n) => Widget n -> IO ()
displayScreen w = Main.defaultMain (simpleApp w) ()

defaultMain :: (Ord n) => App s e n -> s -> IO s
defaultMain app st = do
    let builder = mkVty defaultConfig
    initialVty <- builder
    customMain initialVty builder Nothing app st

homeScreen :: Widget ()
homeScreen =
    withBorderStyle unicode $
    padLeftRight 1 $
    padBottom (Pad 2) $ 
    (hBox [str "Haskell Assessment [Haskell demo project]"] <=> hBorder <=>
     str "This assessment has timer enabled." <=> 
     str "You must complete all sections within the said time." <=>
     str "Kindly note that timer starts as soon as you click on the start button." <=>
     str "To start the assessment, please click on 'Start'." <=> 
     table)
            
table :: Widget ()
table =
    joinBorders $
    withBorderStyle unicode $ 
    border $
    vLimit 5 $
    setAvailableSize (100, 200) $
    -- center $
    (vBox [str "Section Title", hBorder, str "Section 1"] <+> vBorder <+>
    vBox [str "No of Questions", hBorder, str "30"] )

main :: IO ()
main = Main.displayScreen homeScreen


