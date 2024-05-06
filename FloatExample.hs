{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module FloatExample (
    Instance2(..),
    foo,
    render
) where

import qualified Network.WebSockets as WS
import HViewWebSockets (SendMessage(..), sendJsonMessage, Message(..))
import Text.Mustache ( compileMustacheText, renderMustache )
import Text.Mustache.Compile
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, toJSON, (.=), object, encode, decode)
import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE


data Instance2 = Instance2 {
    --hId :: String,
    value :: String
} deriving (Show, Generic)

instance ToJSON Instance2

createMainDiv :: String -> String -> IO String
createMainDiv value id = do
  let templateStr = "<div h-id={{id}} target-id=\"bottomDiv\">{{{content}}}</div>"
      compiledTemplate = compileMustacheText "mainDiv" templateStr
  case compiledTemplate of
    Left bundle -> return "Error compiling main div template"
    Right template -> do
      let rendered = renderMustache template $ object
            [ "id"      .= id
            , "content" .= TL.pack value  -- Convert String to Text
            ]
      return $ TL.unpack rendered  -- Convert Text back to String


-- Function to create the bottom div using Mustache
createBottomDiv :: String -> IO String
createBottomDiv valueClass = do
  let templateStr = "<div h-id=\"bottomDiv\" class=\"{{valueClass}}\"></div>"
      compiledTemplate = compileMustacheText "bottomDiv" templateStr
  case compiledTemplate of
    Left bundle -> return "Error compiling bottom div template"
    Right template -> do
      let rendered = renderMustache template $ object
            [ "valueClass"      .= valueClass
            ]
      return $ TL.unpack rendered  -- Convert Text back to String


-- instance Renderer Instance2 where
--     render hid (Instance2 value ) = do
--       let templateStr = "<div h-id={{id}} target-id=\"bottomDiv\"><div h-value>{{value}}</div> <button h-click=\"set-float\">Show Float</button><div h-id\"bottomDiv\" class=\"{{value}}\"></div></div>"
--       let compiledTemplate = compileMustacheText "page" templateStr
--       case compiledTemplate of
--         Left bundle ->  "Error compiling template"
--         Right template -> renderMustache template $ object
--           [ "value" .= (value :: String)
--           , "id"    .= (hid :: String) ]


render :: String -> String -> IO String
render hid value = do
  let buttonStr = "<button h-click=\"set-float\">Show Float</button>"
  let unsetButtonStr = "<button h-click=\"unset-float\">Hide Float</button>"
  let newValue = buttonStr ++ unsetButtonStr
  mainDivContent <- createMainDiv newValue hid
  bottomDivContent <- createBottomDiv value
  let templateStr = "{{{mainDiv}}}{{{bottomDiv}}}"
      buttonStr = "<button h-click=\"set-float\">Show Float</button>"
      unsetButtonStr = "<button h-click=\"unset-float\">Hide Float</button>"
      compiledTemplate = compileMustacheText "page" templateStr
  case compiledTemplate of
    Left bundle -> return "Error compiling page template"
    Right template -> do
      let rendered = renderMustache template $ object
            [ "mainDiv"   .= T.pack mainDivContent  -- Convert String to Text
            , "button"    .= T.pack buttonStr  -- Convert String to Text
            , "unsetButton"    .= T.pack unsetButtonStr  -- Convert String to Text
            , "bottomDiv" .= T.pack bottomDivContent  -- Convert String to Text
            ]
      return $ TL.unpack rendered  -- Convert Text back to String

foo :: Message -> IO String
foo message =
    case dispatch message of
        "set-float" -> FloatExample.createBottomDiv "float"
        "unset-float" -> FloatExample.createBottomDiv "none"
        _ -> FloatExample.render "myid" "none"

-- handleMessage :: WS.Connection -> Message -> (Message -> IO String) -> IO ()
-- handleMessage conn message generateHTML = do
--     putStrLn $ "Handling message with ID: " ++ hID message
--     case targetID message of
--         Just targetID -> do
--             -- Extract the IO String output from the render function
--             html <- generateHTML message
--             -- Use the extracted String `html` in sendJsonMessage
--             sendJsonMessage conn (SendMessage { hId = targetID, html = html })
--         Nothing -> do 
--           -- In case of Nothing, send an empty string
--           putStrLn "do nothing"
--           --sendJsonMessage conn (SendMessage { hId = hID message, html = "" })



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


