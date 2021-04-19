{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Modify where

import Control.Exception ( throwIO, Exception )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text ( strip, Text )
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable ( Typeable )
import Database.SQLite.Simple
    ( execute,
      execute_,
      query,
      query_,
      field,
      Only(Only),
      FromRow(..),
      Connection,
      ToRow(..),
      Query )
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types ( Null(..) )
import Text.RawString.QQ ( r )

data User = User
    { userId :: Integer
    , username :: Text
    , shell :: Text
    , homeDirectory :: Text
    , realName :: Text
    , phone :: Text
    }
    deriving (Eq, Show)

instance FromRow User where
    fromRow =
        User <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance ToRow User where
    toRow (User id_ username shell homeDir realName phone) =
        toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers =
    [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT
  ,username TEXT UNIQUE
  ,shell TEXT
  ,homeDirectory TEXT
  ,realName TEXT
  ,phone TEXT)
|]

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
    results <- query conn getUserQuery (Only $ strip username)
    case results of
        [] -> return Nothing
        [user] -> return $ Just user
        _ -> throwIO DuplicateData

getUsers :: Connection -> IO (Maybe [User])
getUsers conn = do
    rows <- query_ conn allUsers
    case rows of
        [] -> return Nothing
        users -> return $ Just users

insertUser :: Connection -> UserRow -> IO ()
insertUser conn = execute conn "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

updateUser :: Connection -> User -> IO ()
updateUser conn = execute conn "UPDATE users SET username = ?, shell = ?, homeDirectory = ?, realName = ?, phone = ? where id = ?"

deleteUser :: Connection -> Text -> IO ()
deleteUser conn = execute conn "DELETE FROM users WHERE username = ?" . Only


--    let usernames = map username rows
--        newlineSeparated = T.concat $ intersperse "\n" usernames
--    return $ encodeUtf8 newlineSeparated

--  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
    BS.concat
        [ "Login: "
        , e username
        , "\t\t\t\t"
        , "Name: "
        , e realName
        , "\n"
        , "Directory: "
        , e homeDir
        , "\t\t\t"
        , "Shell: "
        , e shell
        , "\n"
        ]
  where
    e = encodeUtf8

--returnUser :: Connection -> Text -> IO User
--returnUser dbConn username = do
--    maybeUser <- getUser dbConn (T.strip username)
--    case maybeUser of
--        Nothing -> do
--            putStrLn ("Couldn't find matching user for username: " ++ show username)
--            return ()

--        Just user -> sendAll soc (formatUser user)

createDatabase :: Connection -> IO ()
createDatabase conn = do
    execute_ conn createUsers
    insertUser conn meUser
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    SQLite.close conn
  where
    meUser :: UserRow
    meUser = (Null, "basil", "/bin/bash", "/home/basil", "Andrea Basile", "3349272731")
