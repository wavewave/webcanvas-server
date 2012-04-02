{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application.WebCanvas.Server.Yesod where 

import Yesod hiding (update)
import Network.Wai
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC 
import Application.WebCanvas.Type
import Data.Acid
import Data.Attoparsec as P
import Data.Aeson as A
import Data.UUID
import Data.UUID.V5
import Application.WebCanvas.Server.Type
import System.IO
import Network.HTTP.Types (urlDecode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import System.FilePath 
import Control.Concurrent
import Data.Function (on)
import Data.List (sortBy)

mkYesod "WebCanvasServer" [parseRoutes|
/ HomeR GET
/listwebcanvas  ListWebCanvasR GET
/uploadwebcanvas  UploadWebCanvasR POST
/recent RecentR GET
/webcanvas/#UUID WebCanvasR 
|]

instance Yesod WebCanvasServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000


getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [whamlet|
!!!
<html>
  <head> 
    <title> test 
  <body> 
    <h1> hello world 
|]


defhlet :: GGWidget m Handler ()
defhlet = [whamlet| <h1> HTML output not supported |]


getListWebCanvasR :: Handler RepHtmlJson
getListWebCanvasR = do 
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Methods" "POST, GET"
  setHeader "X-Requested-With" "XmlHttpRequest"
  setHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type"

  liftIO $ putStrLn "getListWebCanvasR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  let nr = reverse (sortBy (compare `on` webcanvas_creationtime) r )
  liftIO $ putStrLn $ show nr 
  defaultLayoutJson defhlet (A.toJSON (Just nr))


recentPNG :: T.Text -> GGWidget m Handler () 
recentPNG content = [whamlet| 
  <img src="#{content}">
|]

getRecentR :: Handler RepHtml 
getRecentR = do
  content <- liftIO $ S.readFile "recent.png.base64"
  defaultLayout (recentPNG (T.decodeUtf8 content))

-- | 

nextUUID :: UTCTime -> IO UUID
nextUUID ctime = return . generateNamed namespaceURL . S.unpack . SC.pack $ show ctime 

-- | 

cvsItemFileName :: WebCanvasItem -> FilePath 
cvsItemFileName (WebCanvasItem uuid _) = "data" </> show uuid ++ ".png" ++ ".base64"

-- | 

postUploadWebCanvasR :: Handler RepHtmlJson
postUploadWebCanvasR = do 
  setHeader "Access-Control-Allow-Origin" "ianwookim.org"
  setHeader "Access-Control-Allow-Methods" "POST, GET"
  setHeader "X-Requested-With" "XmlHttpRequest"
  setHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type"

  -- let mutctime = T.parseTime defaultTimeLocale "%F" (unpack timetxt)
  ctime <- liftIO $ getCurrentTime
  uuid <- liftIO (nextUUID ctime)
  let ncvsitem = WebCanvasItem uuid ctime

  -- let utctime = maybe (error "error") id mutctime
 
  liftIO $ putStrLn "" 
  liftIO $ putStrLn $ show ctime
  liftIO $ putStrLn $ show uuid 

  -- liftIO $ putStrLn $ show utctime 

  liftIO $ putStrLn "postQueueR called" 
  acid <- return.server_acid =<< getYesod
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs'
      decoded' = urlDecode True bs
      decoded = SC.drop 4 decoded'   
  liftIO $ withFile (cvsItemFileName ncvsitem) WriteMode $  -- "recent.png.base64" WriteMode $ 
    \h -> S.hPutStr h decoded

  minfo <- liftIO $ update acid (AddWebCanvasItem ncvsitem)

  defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebCanvasItem))

-- | 

handleWebCanvasR :: UUID -> Handler RepHtmlJson
handleWebCanvasR name = do
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Methods" "POST, GET"
  setHeader "X-Requested-With" "XmlHttpRequest"
  setHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type"

  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getWebCanvasR name
    "PUT" -> putWebCanvasR name
    "DELETE" -> deleteWebCanvasR name
    x -> error ("No such action " ++ show x ++ " in handlerWebCanvasR")

getWebCanvasR :: UUID -> Handler RepHtmlJson
getWebCanvasR idee = do 
  liftIO $ putStrLn "getWebCanvasR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryWebCanvasItem idee)
  liftIO $ putStrLn $ show r
  case r of 
    Nothing -> defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe T.Text))
    Just item -> do 
      content <- liftIO $ S.readFile (cvsItemFileName item)
      defaultLayoutJson defhlet (A.toJSON (Just (T.decodeUtf8 content) :: Maybe T.Text))

-- let hlet = [whamlet| <h1> File #{idee}|]
-- defaultLayoutJson hlet (A.toJSON (Just r))


putWebCanvasR :: UUID -> Handler RepHtmlJson
putWebCanvasR idee = do 
  liftIO $ putStrLn "putWebCanvasR called"
  acid <- return.server_acid =<< getYesod
  _wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result WebCanvasItem) of 
        Success minfo -> do 
          if idee == webcanvas_uuid minfo
            then do r <- liftIO $ update acid (UpdateWebCanvasItem minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "webcanvasname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebCanvasItem))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebCanvasItem))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebCanvasItem))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebCanvasItem))

deleteWebCanvasR :: UUID -> Handler RepHtmlJson
deleteWebCanvasR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteWebCanvasItem idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))
