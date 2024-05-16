{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module ToastExample (
    Instance2(..),
    dispatcher,
    render,
    mount
) where

import Control.Concurrent.MVar
import qualified Network.WebSockets as WS
import HViewWebSockets (SendMessage(..), sendJsonMessage, Message(..), Renderer(..))
import Text.Mustache ( compileMustacheText, renderMustache, compileMustacheFile )
import Text.Mustache.Compile
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, toJSON, (.=), object, encode, decode)
import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE


data Instance2 = Instance2 {
    --hId :: String,
    value :: Int,
    floatClass :: String
} deriving (Show, Generic)

instance ToJSON Instance2

-- Define a data structure for your items
data Item = Item { name :: String, price :: Double } deriving (Show)

-- Sample data
items :: [Item]
items = [
    Item { name = "Apple", price = 1.99 },
    Item { name = "Banana", price = 0.99 },
    Item { name = "Orange", price = 2.49 }
  ]

-- Function to generate list item
generateListItem :: Item -> IO String
generateListItem Item {name = name, price = price} = do
  let templateStr = "{{name}} - {{price}}"
  let compiledTemplate = compileMustacheText "mainDiv" templateStr
  case compiledTemplate of
    Left _ -> return "Error compiling main div template"
    Right template -> do
      let rendered = renderMustache template $ object ["name" .= name, "price" .= price]
      return $ TL.unpack rendered

createList :: String -> [Item] -> IO String
createList hid items = do
  template <- compileMustacheFile "templates/list.mustache"
  itemStrings <- mapM generateListItem items
  let rendered = renderMustache template $ object ["items" .= itemStrings, "hid" .= hid]
  return $ TL.unpack rendered

createMainDiv :: String -> IO String
createMainDiv id = do
      template <- compileMustacheFile "templates/mainDiv.mustache"
      let rendered = renderMustache template $ object
            [ "id"      .= id
            ]
      return $ TL.unpack rendered  -- Convert Text back to String

counterDiv :: String -> String -> IO String
counterDiv hId value = do
  template <- compileMustacheFile "templates/counterDiv.mustache"
  let rendered = renderMustache template $ object
        [ "id"    .= hId
        , "count" .= TL.pack value  -- Convert String to Text
        ]
  return $ TL.unpack rendered

-- Function to create the bottom div using Mustache
createBottomDiv :: String -> IO String
createBottomDiv floatClass = do
  template <- compileMustacheFile "templates/bottomDiv.mustache"
  let rendered = renderMustache template $ object
        [ "floatClass" .= floatClass
        ]
  return $ TL.unpack rendered  -- Convert Text back to String

-- render :: Instance2 -> IO String 
-- render instance2 = do
--   template <- compileMustacheFile "templates/toastExample.mustache"
--   itemStrings <- mapM generateListItem items
--   let rendered = renderMustache template $ object
--         [ "floatClass" .= TL.pack (floatClass instance2),
--           "id" .= TL.pack "hid",
--           "count" .= (TL.pack $ show (value instance2)),
--           "items" .= itemStrings,
--           "counter-id" .= TL.pack "counter-id"
--         ]
--   return $ TL.unpack rendered


instance Renderer Instance2 where
    render instance2 = do
        template <- compileMustacheFile "templates/toastExample.mustache"
        itemStrings <- mapM generateListItem items -- You need to define generateListItem
        let rendered = renderMustache template $ object
              [ "floatClass" .= TL.pack (floatClass instance2),
                "id" .= TL.pack "hid",
                "count" .= TL.pack (show (value instance2)),
                "items" .= itemStrings,
                "counter-id" .= TL.pack "fixx"
              ]
        return rendered

mount :: Instance2
mount = Instance2 {
    value = 0,
    floatClass = "none"
}


adjustCounter :: String -> Int -> String
adjustCounter "increment" value = show (value + 1)
adjustCounter "decrement" value = show (value - 1)
adjustCounter _ value = show (value)

handleDispatch :: String -> String -> MVar Instance2 -> IO String
handleDispatch "set-float" _ _ = ToastExample.createBottomDiv "float"
handleDispatch "unset-float" _ _ = ToastExample.createBottomDiv "none"
handleDispatch "increment" _ globalState = do
  modifyMVar_ globalState $ \currentState -> 
    return currentState { value = value currentState + 1 }
  newValue <- readMVar globalState
  counterDiv "fixx" $ show (value newValue)
handleDispatch "decrement" _ globalState = do
  modifyMVar_ globalState $ \currentState ->
    return currentState { value = value currentState - 1 }
  newValue <- readMVar globalState
  counterDiv "fixx" $ show (value newValue)
handleDispatch _ _ globalState = do 
  newValue <- readMVar globalState
  renderedText <- render newValue  -- Correctly binds the IO TL.Text to renderedText
  return $ TL.unpack renderedText  -- Correctly unpacks TL.Text to String within the IO monad

dispatcher :: Message -> MVar Instance2 -> IO String
dispatcher message = handleDispatch (dispatch message) (payload message)