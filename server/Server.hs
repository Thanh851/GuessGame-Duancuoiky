{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import System.IO
import Control.Concurrent
import System.Random (randomRIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forever, when)
import Control.Exception (catch, IOException)

main :: IO ()
main = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just defaultHints {addrFlags = [AI_PASSIVE]}) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)
    listen sock 5
    putStrLn "Server started, waiting for 2 players..."

    -- Nhận 2 client
    (conn1, _) <- accept sock
    (conn2, _) <- accept sock

    h1 <- socketToHandle conn1 ReadWriteMode
    h2 <- socketToHandle conn2 ReadWriteMode
    hSetBuffering h1 LineBuffering
    hSetBuffering h2 LineBuffering

    TIO.hPutStrLn h1 "Welcome Player 1!"
    TIO.hPutStrLn h2 "Welcome Player 2!"
    TIO.hPutStrLn h1 "Your turn! Guess a number (1-100):"

    -- Sinh số bí mật
    secret <- randomRIO (1,100 :: Int)
    putStrLn $ "Secret number is: " ++ show secret

    -- Biến trạng thái
    mVar <- newMVar (1 :: Int)  -- lượt hiện tại
    finished <- newMVar False   -- trạng thái game kết thúc

    -- Threads cho từng người chơi
    forkIO $ playerHandler 1 h1 h2 secret mVar finished
    forkIO $ playerHandler 2 h2 h1 secret mVar finished

    forever $ threadDelay 1000000


-- | Xử lý từng người chơi
playerHandler :: Int -> Handle -> Handle -> Int -> MVar Int -> MVar Bool -> IO ()
playerHandler playerId hSelf hOther secret mVar finished = forever $ do
    guessTxt <- catch (TIO.hGetLine hSelf) handler
    let cleanTxt = T.unpack (T.strip guessTxt)

    end <- readMVar finished
    when (not end && not (null cleanTxt)) $ do
        turn <- readMVar mVar
        if turn /= playerId
          then TIO.hPutStrLn hSelf "Not your turn!"
          else do
              let maybeGuess = reads cleanTxt :: [(Int, String)]
              case maybeGuess of
                [(guess, _)] -> do
                    if guess == secret then do
                        TIO.hPutStrLn hSelf "🎉 Correct! You win!"
                        TIO.hPutStrLn hOther "😢 Opponent guessed correctly. You lose!"
                        putStrLn $ "Player " ++ show playerId ++ " won!"
                        modifyMVar_ finished (const $ return True)
                    else do
                        let hint | guess < secret = "Too low!"
                                 | otherwise      = "Too high!"
                        TIO.hPutStrLn hSelf (T.pack hint)
                        TIO.hPutStrLn hOther (T.pack $ "Player " ++ show playerId ++
                            " guessed: " ++ show guess ++ " (" ++ hint ++ ")")

                        -- Đổi lượt
                        modifyMVar_ mVar (\x -> return (if x == 1 then 2 else 1))
                        next <- readMVar mVar
                        when (not end) $
                            if next == playerId
                              then return ()
                              else TIO.hPutStrLn hOther "Your turn! Guess a number (1-100):"
                _ -> TIO.hPutStrLn hSelf "⚠️ Please enter a valid number."
  where
    handler :: IOException -> IO T.Text
    handler _ = return ""
