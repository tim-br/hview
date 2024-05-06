{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text as T
import Data.Text.Lazy as TL

import Text.Mustache
import Text.Mustache.Compile
import HViewWebSockets (runWebSocketServer, render)
-- import HViewInstance (Counter(..), handleMessage)
import FloatExample (Instance2(..), handleMessage)
--import HViewInstance2 (Instance2(..), handleMessage)
import Data.Aeson ((.=), object)
import Control.Concurrent (forkIO)


-- example for Counter from HviewInstance
-- mainPage :: String -> TL.Text
-- mainPage name = do
--   let counter1 = render $ Counter "myid" 0
--   let counter2 = render $ Counter "myid-32" 32
--   let compiledTemplate = compileMustacheText "page" "<html><head><script src='/js/hview.js'></script></head><body><div>Hello, {{name}}!</div> {{{counter1}}} {{{counter2}}} </body></html>"
--   case compiledTemplate of
--     Left bundle ->  "error"
--     Right template -> renderMustache template $ object
--       [ "name"   .= (name :: String)
--       , "counter1" .= counter1  -- pass the result of buttonTemplate as a variable
--       , "counter2" .= counter2  ]

-- Using Instance2 from HViewInstanc2
mainPage :: String -> TL.Text
mainPage name = do
  let counter1 = render "myid" (Instance2 "")
  -- let counter2 = render "myid-32" $ Instance2 32
  -- let counter1 = render "myid" $ Counter 0
  -- let counter2 = render "myid-32" $ Counter 32

  let compiledTemplate = compileMustacheText "page" "<html><head><link rel=\"stylesheet\" href=\"/css/styles.css\"><script src='/js/hview.js'></script></head><body><div>Test, {{name}}!</div> {{{counter1}}}  </body></html>"
  case compiledTemplate of
    Left bundle ->  "error"
    Right template -> renderMustache template $ object
      [ "name"   .= (name :: String)
      , "counter1" .= counter1
       ]

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

  get "/css/:file" $ do
    cssFile <- captureParam "file"
    setHeader "Content-Type" "application/css"
    file $ "static/css/" ++ cssFile

main :: IO ()
main = do
  _ <- forkIO $ runWebSocketServer handleMessage
  scottyServer