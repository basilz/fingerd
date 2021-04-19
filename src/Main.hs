{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad (forever, when)
import Data.ByteString (ByteString)
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Modify
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.IO ( hSetBuffering, BufferMode(NoBuffering), stdin )
import System.Exit ( exitSuccess )

handleQuery :: Connection -> ByteString -> IO ByteString
handleQuery conn msg = do
  case msg of
    "\r\n" -> do
      users <- getUsers conn
      case users of
        Nothing -> return "No user found"
        Just rows ->
          let usernames = map username rows
              newlineSeparated = T.concat $ intersperse "\n" usernames
           in return $ encodeUtf8 newlineSeparated
    name -> do
      user <- getUser conn (decodeUtf8 name)
      case user of
        Nothing -> return "User not found"
        Just usr -> return $ formatUser usr

handleModify :: Connection -> ByteString -> IO ByteString
handleModify = undefined

handle :: (Connection -> ByteString -> IO ByteString) -> Connection -> Socket -> IO ()
handle action conn sock = forever $ do
  (soc, _) <- accept sock
  msg <- recv soc 1024
  putStrLn "Got connection, handling action"
  resp <- action conn msg
  sendAll soc resp
  close soc

exitOnQ :: IO ()
exitOnQ = do
    hSetBuffering stdin NoBuffering
    c <- getChar
    when (c /= 'q') exitOnQ
    exitSuccess  -- or "exitWith" and some ExitCode value, use hoogle.

main :: IO ()
main = do
  _ <- forkIO $ service "finger.db" "79" $ handle handleQuery
  _ <- forkIO $ service "finger.db" "7979" $ handle handleModify
  exitOnQ
 where
  service dbPath port f = withSocketsDo $ do
    addrinfos <-
      getAddrInfo
        (Just (defaultHints{addrFlags = [AI_PASSIVE]}))
        Nothing
        (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)

    listen sock 1
    -- only one connection open at a time
    conn <- open dbPath
    _ <- f conn sock

    SQLite.close conn