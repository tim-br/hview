{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HViewInstance (
    Counter(..),
    updateCount,
    handleMessage,
) where

import qualified Network.WebSockets as WS
import HViewWebSockets (Renderer(..), SendMessage(..), sendJsonMessage, Body(..), Message(..))
import Text.Mustache
import Text.Mustache.Compile
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, toJSON, (.=), object, encode)
import Data.Text.Lazy as TL

data Counter = Counter {
    count :: Int
} deriving (Show, Generic)

instance ToJSON Counter

instance Renderer Counter where
    render hId (Counter count) = do
      let templateStr = "<div h-id={{id}}><h1>Counter: <div h-value>{{count}}</div></h1><button h-click=\"increment\">Increment</button><button h-click=\"decrement\">Decrement</button></div>"
      let compiledTemplate = compileMustacheText "page" templateStr
      case compiledTemplate of
        Left bundle ->  "Error compiling template"
        Right template -> renderMustache template $ object
          [ "count" .= (count :: Int)
          , "id"    .= (hId :: String) ]

updateCount :: Counter -> Int -> Counter
updateCount (Counter oldCount) newCount = Counter newCount


handleMessage :: WS.Connection -> Message -> IO ()
handleMessage conn message = do
    putStrLn $ "Handling message with ID: " ++ hID message
    case body message of
        Body dispatch payload -> do
            let html = case dispatch of
                            "increment" -> render (hID message) (Counter (payload + 1))
                            "decrement" -> render (hID message) (Counter (payload - 2))
                            _     -> render (hID message) (Counter payload)
            sendJsonMessage conn (SendMessage { hId = hID message, html = TL.unpack html })