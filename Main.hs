{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text as T
import Data.Text.Lazy as TL

import Text.Mustache
import Text.Mustache.Compile
import HViewWebSockets (runWebSocketServer, buttonT)
import Data.Aeson ((.=), object)
import Control.Concurrent (forkIO)

mainPage :: String -> TL.Text
mainPage name = do
  let button = buttonT "myid" 0
  let button32 = buttonT "myid-32" 32
  let compiledTemplate = compileMustacheText "page" "<html><head><script src='/js/hview.js'></script></head><body><div>Hello, {{name}}!</div> {{{button}}} {{{button32}}} </body></html>"
  case compiledTemplate of
    Left bundle ->  "error"
    Right template -> renderMustache template $ object
      [ "name"   .= (name :: String)
      , "button" .= button  -- pass the result of buttonTemplate as a variable
      , "button32" .= button32  ]

scottyServer = scotty 3000 $ do

  get "/page" $ do
    html (mainPage "")

  get "/:word" $ do
    beam <- captureParam "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

  get "/js/:file" $ do
    jsFile <- captureParam "file"
    setHeader "Content-Type" "application/javascript"
    file $ "static/js/" ++ jsFile

main :: IO ()
main = do
  _ <- forkIO runWebSocketServer
  scottyServer