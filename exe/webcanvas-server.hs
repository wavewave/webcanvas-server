{-# LANGUAGE OverloadedStrings #-}

module Main where

import Application.WebCanvas.Server.Type
import Application.WebCanvas.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

main :: IO ()
main = do 
  putStrLn "webcanvas-server"
  acid <- openLocalState M.empty 
  warpDebug 7500 (WebCanvasServer acid)

