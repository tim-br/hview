{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module FloatExample (
    Instance2(..),
    dispatcher,
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
  let templateStr = "<h1>Items</h1>\n" ++
                    "<ul h-id={{hid}}>\n" ++
                    "{{#items}}\n" ++
                    "<li>{{{.}}}</li>\n" ++
                    "{{/items}}\n" ++
                    "</ul>\n"      
      compiledTemplate = compileMustacheText "mainDiv" (T.pack templateStr)
  case compiledTemplate of
    Left _ -> return "Error compiling main div template"
    Right template -> do
      itemStrings <- mapM generateListItem items
      let rendered = renderMustache template $ object ["items" .= itemStrings, "hid" .= hid]
      return $ TL.unpack rendered

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

counterDiv :: String -> String -> IO String
counterDiv hId value = do
  let templateStr = "<div h-id={{id}} target-id={{id}}><h1>Counter: <div h-value>{{count}}</div></h1><button h-click=\"increment\">Increment</button><button h-click=\"decrement\">Decrement</button></div>"
      compiledTemplate = compileMustacheText "mainDiv" templateStr
  case compiledTemplate of
    Left _ -> return "Error compiling main div template"
    Right template -> do
      let rendered = renderMustache template $ object
            [ "id"    .= hId
            , "count" .= TL.pack value  -- Convert String to Text
            ]
      return $ TL.unpack rendered

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
  list <- createList "list" items
  bottomDivContent <- createBottomDiv value
  counterDiv <- counterDiv "fixx" "0"
  let templateStr = "{{{mainDiv}}}{{{counterDiv}}} {{{list}}} {{{bottomDiv}}}"
      -- buttonStr = "<button h-click=\"set-float\">Show Float</button>"
      -- unsetButtonStr = "<button h-click=\"unset-float\">Hide Float</button>"
      compiledTemplate = compileMustacheText "page" templateStr
  case compiledTemplate of
    Left _ -> return "Error compiling page template"
    Right template -> do
      let rendered = renderMustache template $ object
            [ "mainDiv"   .= T.pack mainDivContent  -- Convert String to Text
            , "button"    .= T.pack buttonStr  -- Convert String to Text
            , "unsetButton"    .= T.pack unsetButtonStr  -- Convert String to Text
            , "counterDiv" .= T.pack counterDiv
            , "bottomDiv" .= T.pack bottomDivContent  -- Convert String to Text
            , "list" .= T.pack list
            ]
      return $ TL.unpack rendered  -- Convert Text back to String

adjustCounter :: String -> Int -> String
adjustCounter "increment" value = show (value + 1)
adjustCounter "decrement" value = show (value - 1)
adjustCounter _ value = show (value)

dispatcher :: Message -> IO String
dispatcher message =
    case dispatch message of
        "set-float" -> FloatExample.createBottomDiv "float"
        "unset-float" -> FloatExample.createBottomDiv "none"
        "increment" -> counterDiv "fixx" $ show ((read (payload message) :: Int) + 1)
        "decrement" -> counterDiv "fixx" $ show ((read (payload message) :: Int) - 1)
        _ -> FloatExample.render "myid" "none"