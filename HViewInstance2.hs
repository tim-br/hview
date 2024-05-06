{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HViewInstance2 (
    Instance2(..),
    handleMessage,
) where

import qualified Network.WebSockets as WS
import HViewWebSockets (Renderer(..), SendMessage(..), sendJsonMessage)
import Text.Mustache
import Text.Mustache.Compile
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, toJSON, (.=), object, encode)
import Data.Text.Lazy as TL

data Instance2 = Instance2 {
    hId :: String,
    value :: Int
} deriving (Show, Generic)

instance ToJSON Instance2

instance Renderer Instance2 where
    render (Instance2 hId value ) = do
      let templateStr = "<div h-id={{id}}><div h-value>{{value}}</div> <button h-click=\"inc\">Increment To Value 1</button><button h-click=\"dec\">Decrement To Value 1</button></div>"
      let compiledTemplate = compileMustacheText "page" templateStr
      case compiledTemplate of
        Left bundle ->  "Error compiling template"
        Right template -> renderMustache template $ object
          [ "value" .= (value :: Int)
          , "id"    .= (hId :: String) ]

updateCount :: Instance2 -> Int -> Instance2
updateCount (Instance2 hId oldCount) newCount = Instance2 hId newCount

handleMessage :: WS.Connection -> String -> String -> Int -> IO ()
handleMessage conn hId message value = do
    putStrLn $ "Handling message with ID: " ++ hId ++ " and message: " ++ message
    let html = case message of
                    "inc" -> render (Instance2 hId (value + 2))
                    "dec" -> render (Instance2 hId (value - 2))
                    _     -> render (Instance2 hId value)
    sendJsonMessage conn (SendMessage { HViewWebSockets.hId = hId, html = TL.unpack html })