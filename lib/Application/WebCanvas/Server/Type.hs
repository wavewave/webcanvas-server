{-# LANGUAGE OverloadedStrings #-}

module Application.WebCanvas.Server.Type where

import           Data.Acid
import qualified Data.ByteString.Char8 as C
import           Data.Text as T
import           Data.Text.Encoding as E
import           Data.Time.Clock
import           Data.Time.Format
import           Data.UUID
import           System.Locale
import           Text.Blaze
import           Yesod.Dispatch
--
import           Application.WebCanvas.Type

-- | 

instance PathPiece UUID where
  fromPathPiece = fromString . C.unpack . E.encodeUtf8
  toPathPiece = E.decodeUtf8 . C.pack . toString 

-- | 

instance PathPiece UTCTime where
  fromPathPiece = parseTime defaultTimeLocale "%Y%m%d-%H%M%S-%Z" . T.unpack 
  toPathPiece = T.pack . formatTime defaultTimeLocale "%Y%m%d_%H%M%S_%Z"

-- | 

instance ToMarkup UUID where
  toMarkup = toMarkup . toString 

-- | 

instance ToMarkup UTCTime where
  toMarkup = toMarkup . show



data WebCanvasServer = WebCanvasServer {
  server_acid :: AcidState WebCanvasItemRepository
}
