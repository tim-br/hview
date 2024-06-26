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

import Control.Concurrent.MVar
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
    render :: a -> IO TL.Text
    
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
    targetID :: String,
    dispatch :: String,
    payload :: String
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

type MessageHandler a =  WS.Connection -> Message -> (Message -> MVar a -> IO String) -> MVar a -> IO ()

wsApp :: MessageHandler a -> (Message -> MVar a -> IO String) -> a -> WS.ServerApp
wsApp messageHandler generateHTML initialValue pending_conn = do
    conn <- WS.acceptRequest pending_conn
    WS.forkPingThread conn 30  -- Keep the connection alive
    globalState <- newMVar initialValue
    forever $ do
        msg <- WS.receiveData conn
        case decode msg of
            Just parsedMsg -> messageHandler conn parsedMsg generateHTML globalState
            Nothing -> putStrLn "Error parsing JSON"

-- Run the WebSocket server
runWebSocketServer :: (Message -> MVar a -> IO String) -> a -> IO ()
runWebSocketServer generateHTML initialValue = do
  WS.runServer "127.0.0.1" 3001 (wsApp handleMessage generateHTML initialValue)

handleMessage :: WS.Connection -> Message -> (Message -> MVar a -> IO String) -> MVar a -> IO ()
handleMessage conn message generateHTML globalState = do
    putStrLn $ "Handling message with ID: " ++ hID message
            -- Extract the IO String output from the render function
    html <- generateHTML message globalState
    -- Use the extracted String `html` in sendJsonMessage
    sendJsonMessage conn (SendMessage { hId = targetID message, html = html })
          --sendJsonMessage conn (SendMessage { hId = hID message, html = "" })