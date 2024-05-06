{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ExistentialQuantification #-}


module HViewWebSockets (
    runWebSocketServer,
    Renderer(..),
    sendJsonMessage,
    SendMessage(..),
    Message(..),
) where

import qualified Network.WebSockets as WS
import Data.Text ()
import Control.Monad (forever)
import Data.Aeson (FromJSON, decode)
import qualified Data.Text.Lazy.Encoding as TextEncoding
import GHC.Generics (Generic)
import Data.Text.Lazy as TL
import Data.Aeson (ToJSON, parseJSON, Value(..), toJSON, (.=), object, encode)
import Text.Mustache
import Text.Mustache.Compile
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson.Types (withObject, Parser)
import Data.Aeson ((.:))

-- Define a typeclass for rendering
class Renderer a where
    render :: String -> a -> TL.Text

-- Define a type for template data
-- data TemplateData = TemplateData {
--     id :: String,
--     count :: Int
-- } deriving (Show, Generic)

--instance ToJSON TemplateData

-- Implement Renderer for TemplateData
-- instance Renderer TemplateData where
--     render (TemplateData hId count) = buttonT hId count

-- Modify buttonT to accept a value of typeclass Renderer
-- buttonT :: Renderer a => a -> TL.Text
-- buttonT = render

-- Overload buttonT to accept custom HTML
-- buttonT :: TL.Text -> TL.Text
-- buttonT customHTML = customHTML

-- data Body = Body {
--     dispatch :: String,
--     payload :: Int
-- } deriving (Show, Generic)

data Message = Message {
    hID :: String,
    targetID :: Maybe String,
    dispatch :: String
} deriving (Show, Generic)

instance FromJSON Message

data SendMessage = SendMessage
  { hId :: String
  , html :: String
  } deriving (Show, Generic)

-- instance (FromJSON a) => FromJSON (Message a)

--instance FromJSON (Message a)
--instance (FromJSON a) => FromJSON (Message a)

instance FromJSON SendMessage


instance ToJSON SendMessage where
    toJSON (SendMessage hId html) = object
        [ "h_id" Data.Aeson..= hId
        , "html" Data.Aeson..= html
        ]

-- Helper function to send JSON via WebSocket
sendJsonMessage :: WS.Connection -> SendMessage -> IO ()
sendJsonMessage conn msg =
    WS.sendTextData conn (BSL.toStrict $ encode msg)

-- -- handleMessage :: String -> String -> Int -> Text
-- handleMessage conn hId message value = do
--   putStrLn $ "Handling message with ID: " ++ hId ++ " and message: " ++ message
--   let html = case message of
--                 "increment" -> buttonT hId (value + 1)
--                 "decrement" -> buttonT hId (value - 1)
--                 _           -> buttonT hId 99
--   sendJsonMessage conn (SendMessage { hId = hId, html = TL.unpack html })

-- type MessageHandler = WS.Connection -> String -> String -> Int -> IO ()

type MessageHandler =  WS.Connection -> Message -> IO ()

wsApp :: MessageHandler -> WS.ServerApp
wsApp handleMessage pending_conn = do
  conn <- WS.acceptRequest pending_conn
  WS.forkPingThread conn 30  -- Keep the connection alive
  forever $ do
    msg <- WS.receiveData conn
    --WS.sendTextData conn (msg :: Text)  -- Echo back the received message
    case decode msg :: Maybe Message of
      Just parsedMsg -> handleMessage conn parsedMsg
      Nothing -> putStrLn "Error parsing JSON"

-- Run the WebSocket server
runWebSocketServer :: MessageHandler -> IO ()
runWebSocketServer handleMsg = WS.runServer "127.0.0.1" 3001 (wsApp handleMsg)

-- {-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- module HViewWebSockets (runWebSocketServer, buttonT) where

-- import qualified Network.WebSockets as WS
-- import Data.Text
-- import Control.Monad (forever)
-- import Data.Aeson (FromJSON, decode)
-- import qualified Data.Text.Lazy.Encoding as TextEncoding
-- import GHC.Generics (Generic)
-- import Data.Text.Lazy as TL
-- import Data.Aeson (ToJSON, toJSON, (.=), object, encode)
-- import Text.Mustache
-- import Text.Mustache.Compile
-- import qualified Data.ByteString.Lazy as BSL

-- -- WebSocket application logic
-- -- wsApp :: WS.ServerApp
-- -- wsApp pending_conn = do
-- --   conn <- WS.acceptRequest pending_conn
-- --   WS.forkPingThread conn 30  -- Keep the connection alive
-- --   let loop = do
-- --         msg <- WS.receiveData conn
-- --         WS.sendTextData conn (msg :: Text)  -- Echo back the received message
-- --   loop

-- data Message = Message {
--     h_id :: String,
--     msg :: String,
--     value :: Int
-- } deriving (Generic, Show)

-- data SendMessage = SendMessage
--   { hId :: String
--   , html :: String
--   } deriving (Show, Generic)

-- instance FromJSON Message
-- instance FromJSON SendMessage

-- instance ToJSON SendMessage where
--     toJSON (SendMessage hId html) = object
--         [ "h_id" Data.Aeson..= hId
--         , "html" Data.Aeson..= html
--         ]

-- -- Helper function to send JSON via WebSocket
-- sendJsonMessage :: WS.Connection -> SendMessage -> IO ()
-- sendJsonMessage conn msg =
--     WS.sendTextData conn (BSL.toStrict $ encode msg)

-- buttonT :: String -> Int -> TL.Text
-- buttonT hId count = do
--   let templateStr = "<div h-id={{id}}><h1>Counter: <div h-value>{{count}}</div></h1><button h-click=\"increment\">Increment</button><button h-click=\"decrement\">Decrement</button></div>"
--   let compiledTemplate = compileMustacheText "page" templateStr
--   case compiledTemplate of
--     Left bundle ->  "Error compiling template"
--     Right template -> renderMustache template $ object
--       [ "count" .= (count :: Int)
--       , "id"    .= (hId :: String) ]

-- -- handleMessage :: String -> String -> Int -> Text
-- handleMessage conn hId message value = do
--   putStrLn $ "Handling message with ID: " ++ hId ++ " and message: " ++ message
--   let html = case message of
--                 "increment" -> buttonT hId (value + 1)
--                 "decrement" -> buttonT hId (value - 1)
--                 _           -> buttonT hId 99
--   sendJsonMessage conn (SendMessage { hId = hId, html = TL.unpack html })

-- wsApp :: WS.ServerApp
-- wsApp pending_conn = do
--   conn <- WS.acceptRequest pending_conn
--   WS.forkPingThread conn 30  -- Keep the connection alive
--   forever $ do
--     msg <- WS.receiveData conn
--     --WS.sendTextData conn (msg :: Text)  -- Echo back the received message
--     case decode msg :: Maybe Message of
--       Just (Message hId message value) -> handleMessage conn hId message value
--       Nothing -> putStrLn "Error parsing JSON"

-- -- Run the WebSocket server
-- runWebSocketServer :: IO ()
-- runWebSocketServer = WS.runServer "127.0.0.1" 3001 wsApp