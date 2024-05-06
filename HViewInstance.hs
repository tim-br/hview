{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HViewInstance (
    Counter(..),
    updateCount,
    handleMessage,
) where

import qualified Network.WebSockets as WS
import HViewWebSockets (Renderer(..), SendMessage(..), sendJsonMessage)
import Text.Mustache
import Text.Mustache.Compile
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, toJSON, (.=), object, encode)
import Data.Text.Lazy as TL

data Counter = Counter {
    hId :: String,
    count :: Int
} deriving (Show, Generic)

instance ToJSON Counter

instance Renderer Counter where
    render (Counter hId count) = do
      let templateStr = "<div h-id={{id}}><h1>Counter: <div h-value>{{count}}</div></h1><button h-click=\"increment\">Increment</button><button h-click=\"decrement\">Decrement</button></div>"
      let compiledTemplate = compileMustacheText "page" templateStr
      case compiledTemplate of
        Left bundle ->  "Error compiling template"
        Right template -> renderMustache template $ object
          [ "count" .= (count :: Int)
          , "id"    .= (hId :: String) ]

updateCount :: Counter -> Int -> Counter
updateCount (Counter hId oldCount) newCount = Counter hId newCount

handleMessage :: WS.Connection -> String -> String -> Int -> IO ()
handleMessage conn hId message value = do
    putStrLn $ "Handling message with ID: " ++ hId ++ " and message: " ++ message
    let html = case message of
                    "increment" -> render (Counter hId (value + 1))
                    "decrement" -> render (Counter hId (value - 1))
                    _           -> render (Counter hId value)
    sendJsonMessage conn (SendMessage { HViewWebSockets.hId = hId, html = TL.unpack html })