module Main where

import Database.SQLite.Simple ( open )
import Database.SQLite.Simple.Types ( Null(Null) )
import Modify ( insertUser, deleteUser )
import Options ( Command(Delete, Update, Insert), programOpts )
import Options.Applicative (execParser)

main :: IO ()
main = do
    command <- execParser programOpts
    conn <- open "finger.db"
    case command of
        Insert u s h r p -> insertUser conn (Null, u, s, h, r, p)
        Update u s h r p -> undefined
        Delete u -> deleteUser conn u