module Utility where

import Brick
  ( Padding (Pad),
    Widget,
    clickable,
    joinBorders,
    padBottom,
    padLeftRight,
    padTop,
    setAvailableSize,
    str,
    vLimit,
    withBorderStyle,
  )
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicode)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.UTF8 as BLU (ByteString, fromString)
import DataTypes (Label, Name)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, hIsEOF, openFile)
createContainer :: Widget Name -> Widget Name
createContainer w =
  withBorderStyle unicode $
    border $
      padLeftRight 1 $
        padBottom (Pad 1) $
          setAvailableSize (80, 1000) w
createButton :: Label -> Name -> Widget Name
createButton l n =
  clickable
    n
    (padTop (Pad 1) $
        joinBorders $
          withBorderStyle unicode $
            border $
              vLimit 5 $
                setAvailableSize (50, 10) $
                  padLeftRight 2 $ str l)


