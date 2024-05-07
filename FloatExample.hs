{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module FloatExample (
    Instance2(..),
    dispatcher,
    render
) where

import Control.Concurrent.MVar
import qualified Network.WebSockets as WS
import HViewWebSockets (SendMessage(..), sendJsonMessage, Message(..))
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
    value :: String
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
createBottomDiv valueClass = do
  template <- compileMustacheFile "templates/bottomDiv.mustache"
  let rendered = renderMustache template $ object
        [ "valueClass"      .= valueClass
        ]
  return $ TL.unpack rendered  -- Convert Text back to String


render :: String -> String -> IO String
render hid value = do
  mainDivContent <- createMainDiv hid
  list <- createList "list" items
  bottomDivContent <- createBottomDiv value
  counterDiv <- counterDiv "fixx" "0"
  let templateStr = "{{{mainDiv}}}{{{counterDiv}}} {{{list}}} {{{bottomDiv}}}"
      compiledTemplate = compileMustacheText "page" templateStr
  case compiledTemplate of
    Left _ -> return "Error compiling page template"
    Right template -> do
      let rendered = renderMustache template $ object
            [ "mainDiv"   .= T.pack mainDivContent  -- Convert String to Text
            , "counterDiv" .= T.pack counterDiv
            , "bottomDiv" .= T.pack bottomDivContent  -- Convert String to Text
            , "list" .= T.pack list
            ]
      return $ TL.unpack rendered  -- Convert Text back to String

adjustCounter :: String -> Int -> String
adjustCounter "increment" value = show (value + 1)
adjustCounter "decrement" value = show (value - 1)
adjustCounter _ value = show (value)

handleDispatch :: (Num a, Show a) => String -> String -> MVar a -> IO String
handleDispatch "set-float" _ _ = FloatExample.createBottomDiv "float"
handleDispatch "unset-float" _ _ = FloatExample.createBottomDiv "none"
handleDispatch "increment" _ globalState = do
  modifyMVar_ globalState $ \value -> return (value + 1)
  newValue <- readMVar globalState
  counterDiv "fixx" $ show newValue
handleDispatch "decrement" _ globalState = do
  modifyMVar_ globalState $ \value -> return (value - 1)
  newValue <- readMVar globalState
  counterDiv "fixx" $ show newValue
handleDispatch _ _ _ = FloatExample.render "myid" "none"

dispatcher :: (Num a, Show a) => Message -> MVar a -> IO String
dispatcher message = handleDispatch (dispatch message) (payload message)