{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Network.Socket
import System.IO
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Control.Exception (catch, IOException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

serverHost, serverPort :: String
serverHost = "127.0.0.1"
serverPort = "3000"

main :: IO ()
main = startGUI defaultConfig { jsPort = Just 8023 } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "🎮 GuessGame Client 🎮"

    -- Body style
    getBody window # set style
      [ ("background","linear-gradient(120deg,#FFDEE9,#B5FFFC)")
      , ("font-family","'Comic Sans MS', cursive, sans-serif")
      , ("display","flex")
      , ("justify-content","center")
      , ("align-items","center")
      , ("height","100vh")
      ]

    -- Container
    container <- UI.div # set style
      [ ("background","rgba(0,0,0,0.7)")
      , ("padding","30px")
      , ("border-radius","20px")
      , ("width","500px")
      , ("box-shadow","0 0 30px rgba(0,0,0,0.5)")
      , ("display","flex")
      , ("flex-direction","column")
      , ("align-items","center")
      ]

    -- --- LOGIN SCREEN ---
    loginTitle <- UI.h1 # set text "🎮 Welcome to Guess Game 🎮"
                        # set style [("color","#fff"),("margin-bottom","20px")]
    nameInput <- UI.input # set (attr "placeholder") "Enter your name"
                          # set style
                            [ ("padding","10px")
                            , ("width","80%")
                            , ("border-radius","8px")
                            , ("border","none")
                            , ("margin-bottom","15px")
                            , ("font-size","16px")
                            ]
    loginBtn <- UI.button # set text "LOGIN"
                          # set style
                            [ ("padding","10px 20px")
                            , ("border-radius","12px")
                            , ("border","none")
                            , ("background","linear-gradient(45deg,#FF6FD8,#3813C2)")
                            , ("color","#fff")
                            , ("cursor","pointer")
                            , ("font-family","'Comic Sans MS', cursive")
                            , ("transition","0.3s all")
                            ]
    element container #+ [element loginTitle, element nameInput, element loginBtn]
    getBody window #+ [element container]

    -- Khi bấm LOGIN
    on UI.click loginBtn $ \_ -> do
        playerName <- get value nameInput  -- :: String
        if null playerName
          then return ()
          else do
            -- Clear container
            element container # set children []
            gameUI window container playerName

-- --- GAME UI ---
gameUI :: Window -> Element -> String -> UI ()
gameUI window container playerName = do
    -- Title
    hTitle <- UI.h1 # set text ("🎲 Guess Game - " ++ playerName)
                   # set style [("color","#fff"),("margin-bottom","20px")]

    -- Input
    input <- UI.input # set (attr "placeholder") "Enter your guess (1-100)"
                      # set style
                        [ ("padding","10px")
                        , ("width","80%")
                        , ("border-radius","8px")
                        , ("border","none")
                        , ("margin-bottom","15px")
                        , ("font-size","16px")
                        ]

    -- Send button
    sendBtn <- UI.button # set text "SEND"
                        # set style
                          [ ("padding","10px 20px")
                          , ("border-radius","12px")
                          , ("border","none")
                          , ("background","linear-gradient(45deg,#FF6FD8,#3813C2)")
                          , ("color","#fff")
                          , ("cursor","pointer")
                          , ("font-family","'Comic Sans MS', cursive")
                          , ("transition","0.3s all")
                          ]

    -- Output
    output <- UI.div # set style
      [ ("width","100%")
      , ("height","300px")
      , ("background","rgba(255,255,255,0.1)")
      , ("color","#fff")
      , ("padding","10px")
      , ("overflow-y","auto")
      , ("border-radius","8px")
      , ("display","flex")
      , ("flex-direction","column")
      ]

    element container #+ [element hTitle, element input, element sendBtn, element output]

    -- Connect to server
    liftIO $ do
      addrinfos <- getAddrInfo Nothing (Just serverHost) (Just serverPort)
      let serveraddr = head addrinfos
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      connect sock (addrAddress serveraddr)
      h <- socketToHandle sock ReadWriteMode
      hSetBuffering h LineBuffering

      -- Reader thread
      void $ forkIO $ forever $ do
        msg <- (TIO.hGetLine h) `catch` (\(_ :: IOException) -> return "Server disconnected.")
        let msgClean = T.strip msg  -- loại bỏ ký tự \r, whitespace đầu/cuối
        runUI window $ do
          let decorated = if "Too" `T.isPrefixOf` msgClean then "😢 " <> msgClean
                          else if "Correct" `T.isPrefixOf` msgClean then "😎 " <> msgClean
                          else msgClean
          msgDiv <- UI.div # set text (T.unpack decorated)
          element output #+ [element msgDiv]

      -- Send button
      runUI window $ on UI.click sendBtn $ \_ -> do
        txt <- get value input  -- String
        liftIO $ TIO.hPutStrLn h (T.pack txt)  -- convert sang Text trước khi gửi
        element input # set value ""
