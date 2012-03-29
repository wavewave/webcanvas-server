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
import Application.WebCanvas.Type
import Data.Acid
import Data.Attoparsec as P
import Data.Aeson as A
import Data.UUID
import Application.WebCanvas.Server.Type

mkYesod "WebcanvasServer" [parseRoutes|
/ HomeR GET
/listwebcanvas  ListWebcanvasR GET
/uploadwebcanvas  UploadWebcanvasR POST
/webcanvas/#UUID WebcanvasR 
|]

instance Yesod WebcanvasServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000

{-instance RenderMessage WebcanvasServer FormMessage where
  renderMessage _ _ = defaultFormMessage -}


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


getListWebcanvasR :: Handler RepHtmlJson
getListWebcanvasR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUploadWebcanvasR :: Handler RepHtmlJson
postUploadWebcanvasR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- return.server_acid =<< getYesod
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result WebcanvasInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddWebcanvas minfo)
          liftIO $ print (Just r)
          liftIO $ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebcanvasInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebcanvasInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebcanvasInfo))



handleWebcanvasR :: UUID -> Handler RepHtmlJson
handleWebcanvasR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getWebcanvasR name
    "PUT" -> putWebcanvasR name
    "DELETE" -> deleteWebcanvasR name
    x -> error ("No such action " ++ show x ++ " in handlerWebcanvasR")

getWebcanvasR :: UUID -> Handler RepHtmlJson
getWebcanvasR idee = do 
  liftIO $ putStrLn "getWebcanvasR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryWebcanvas idee)
  liftIO $ putStrLn $ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


putWebcanvasR :: UUID -> Handler RepHtmlJson
putWebcanvasR idee = do 
  liftIO $ putStrLn "putWebcanvasR called"
  acid <- return.server_acid =<< getYesod
  _wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result WebcanvasInfo) of 
        Success minfo -> do 
          if idee == webcanvas_uuid minfo
            then do r <- liftIO $ update acid (UpdateWebcanvas minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "webcanvasname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebcanvasInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebcanvasInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebcanvasInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebcanvasInfo))

deleteWebcanvasR :: UUID -> Handler RepHtmlJson
deleteWebcanvasR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteWebcanvas idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))
