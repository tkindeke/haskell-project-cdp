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
import DataTypes (Assessment (Assessment), Name)
import GHC.IO.Handle (hDuplicate)
import Graphics.Vty (MaybeDefault)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, hIsEOF, openFile)

-- returnIOHandle :: FilePath -> IO Handle
-- returnIOHandle a = openFile a ReadMode

-- returnIOString :: Handle -> IO String
-- returnIOString = System.IO.hGetContents

-- returnByteString :: String -> BLU.ByteString
-- returnByteString =  BLU.fromString

-- y :: Maybe Assessment

--applies styles to the screen main container
container :: Widget Name -> Widget Name
container w =
  withBorderStyle unicode $
    border $
      padLeftRight 1 $
        padBottom (Pad 1) $
          setAvailableSize (80, 500) w

type Label = String

button :: Label -> Name -> Widget Name
button l n =
  clickable
    n
    ( padTop (Pad 1) $
        joinBorders $
          withBorderStyle unicode $
            border $
              vLimit 5 $
                setAvailableSize (50, 10) $
                  padLeftRight 2 $ str l
    )

getAssessment :: FilePath -> IO ()
getAssessment fp = do
  hdl <- openFile fp ReadMode
  fc <- System.IO.hGetContents hdl
  let btfc = BLU.fromString fc
  let assessments = decode btfc :: Maybe Assessment
  hClose hdl

-- returnString:: FilePath -> BLU.ByteString
-- returnString fp = returnIOHandle fp >>= returnIOString >>= returnByteString

-- getFileContentIOS :: FilePath -> IO String
-- getFileContentIOS a = openFile a ReadMode >>= getFileContent

-- getFileContentBS :: String -> BLU.ByteString
-- getFileContentBS = fromString

-- returnMaybeAssessment :: BLU.ByteString -> Maybe Assessment
-- returnMaybeAssessment = Data.Aeson.decode

-- combineFunction :: FilePath -> BLU.ByteString
-- combineFunction fp = getFileContentIOS fp >>= getFileContentBS
