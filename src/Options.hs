module Options where

import Data.Text
import Options.Applicative

data Command
    = Insert
        { username :: Text
        , shell :: Text
        , homeDirectory :: Text
        , realName :: Text
        , phone :: Text
        }
    | Update
        { username :: Text
        , shell :: Text
        , homeDirectory :: Text
        , realName :: Text
        , phone :: Text
        }
    | Delete
        {username :: Text}

newtype Options = Options {cmd :: Command}

insertOpts :: Parser Command
insertOpts =
    Insert <$> strArgument (metavar "USERNAME")
        <*> strArgument (metavar "SHELL")
        <*> strArgument (metavar "HOME")
        <*> strArgument (metavar "REALNAME")
        <*> strArgument (metavar "PHONE")

updateOpts :: Parser Command
updateOpts =
    Update <$> strArgument (metavar "USERNAME")
        <*> strArgument (metavar "SHELL")
        <*> strArgument (metavar "HOME")
        <*> strArgument (metavar "REALNAME")
        <*> strArgument (metavar "PHONE")

deleteOpts :: Parser Command
deleteOpts =
    Delete <$> strArgument (metavar "USERNAME")

insert :: Mod CommandFields Command
insert = command "insert" (info insertOpts (progDesc "Insert a new user"))

update :: Mod CommandFields Command
update = command "update" (info updateOpts (progDesc "Update an existing user"))

delete :: Mod CommandFields Command
delete = command "delete" (info deleteOpts (progDesc "Delete an existing user"))

programOpts :: ParserInfo Command
programOpts =
    info
        ( helper
            <*> hsubparser (insert <> update <> delete)
        )
        (fullDesc <> progDesc "An utility to interact with fingerd database")