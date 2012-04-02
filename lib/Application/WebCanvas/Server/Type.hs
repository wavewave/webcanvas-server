{-# LANGUAGE OverloadedStrings #-}

module Application.WebCanvas.Server.Type where

import Control.Applicative
import Data.Text.Encoding as E
import Data.UUID
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Yesod.Dispatch
import Text.Blaze
import Application.WebCanvas.Type
-- import Debug.Trace 
import Data.Acid
import System.Locale
import Data.Text as T
import Data.Time.Clock
import Data.Time.Format


-- | 

instance SinglePiece UUID where
  fromSinglePiece = fromString . C.unpack . E.encodeUtf8
  toSinglePiece = E.decodeUtf8 . C.pack . toString 

-- | 

instance SinglePiece UTCTime where
  fromSinglePiece = parseTime defaultTimeLocale "%Y%m%d-%H%M%S-%Z" . T.unpack 
  toSinglePiece = T.pack . formatTime defaultTimeLocale "%Y%m%d_%H%M%S_%Z"

-- | 

instance ToHtml UUID where
  toHtml = toHtml . toString 

-- | 

instance ToHtml UTCTime where
  toHtml = toHtml . show



data WebCanvasServer = WebCanvasServer {
  server_acid :: AcidState WebCanvasItemRepository
}
