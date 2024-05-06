{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module FloatExample (
    Instance2(..),
    handleMessage,
) where

import qualified Network.WebSockets as WS
import HViewWebSockets (Renderer(..), SendMessage(..), sendJsonMessage, Message(..), Body(..))
import Text.Mustache
import Text.Mustache.Compile
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, toJSON, (.=), object, encode, decode)
import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE


data Instance2 = Instance2 {
    --hId :: String,
    value :: String
} deriving (Show, Generic)

instance ToJSON Instance2

instance Renderer Instance2 where
    render hid (Instance2 value ) = do
      let templateStr = "<div h-id={{id}}><div h-value>{{value}}</div> <button h-click=\"set-float\">Show Float</button><div class=\"float\"></div></div>"
      let compiledTemplate = compileMustacheText "page" templateStr
      case compiledTemplate of
        Left bundle ->  "Error compiling template"
        Right template -> renderMustache template $ object
          [ "value" .= (value :: String)
          , "id"    .= (hid :: String) ]

handleMessage :: WS.Connection -> Message -> IO ()
handleMessage conn message = do
    putStrLn $ "Handling message with ID: " ++ hID message
    case body message of
        Body dispatch payload -> do
            let html = case dispatch of
                            "set-float" -> render (hID message) (Instance2 "")
            sendJsonMessage conn (SendMessage { hId = hID message, html = TL.unpack html })


-- handleMessage :: WS.Connection -> Message -> IO ()
-- handleMessage conn (Message hId body) = do
--     putStrLn $ "Handling message with ID: " ++ hId
--     case decode body :: Maybe Instance2 of
--         Just (Instance2 val) -> do
--             let html = case val of
--                             "inc" -> render hId (Instance2 (val + 2))
--                             "dec" -> render hId (Instance2 (val - 2))
--                             _     -> render hId (Instance2 val)
--             sendJsonMessage conn (SendMessage { hId = hId, html = TL.unpack html })
--         Nothing -> putStrLn "Error parsing JSON"


