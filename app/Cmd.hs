{-# LANGUAGE OverloadedStrings #-}

module Cmd (
  CmdOpts (CmdOpts),
  MongoOpts (MongoOpts)
  ) where

import           Data.Bifunctor
import           Data.Maybe
import           Data.Text        hiding (group)
import           Database.MongoDB hiding (Group, group)
import           Options

-------------------------------------------------------------------------------------------------------------------------
-- Global options
-------------------------------------------------------------------------------------------------------------------------
data CmdOpts = CmdOpts {
  mongoOpts :: MongoOpts,
  port      :: Int
  } deriving (Eq, Show)

instance Options CmdOpts where
  defineOptions = pure CmdOpts
                  <*> defineOptions
                  <*> simpleOption "port" 3000 "Port on which to listen for incoming HTTP calls."

-------------------------------------------------------------------------------------------------------------------------
-- MongoDB options
-------------------------------------------------------------------------------------------------------------------------

data MongoOpts = MongoOpts {
  mongoHost :: Host,
  mongoDb   :: Text,
  mongoCol  :: Text
  } deriving (Eq, Show)

mongoGroup :: Group
mongoGroup = group "mongodb" "MongoDB options" "Show MongoDB specific options"

optionType_host :: OptionType Host
optionType_host = optionType "mongodb host" (host "localhost") parseHost showHostPort
    where
      parseHost = first (const "Invalid mongodb host") . readHostPortM

optionType_text :: OptionType Text
optionType_text = optionType "text" "" (Right . pack) show

-- | Helper for defining mongo-specific options.
mongoOptions :: OptionType a -> String -> String -> Char -> Maybe a -> DefineOptions a
mongoOptions opts desc long short def = defineOption opts $ \o -> o {
  optionLongFlags   = [long],
  optionShortFlags  = [short],
  optionDescription = desc,
  optionGroup       = Just mongoGroup,
  optionDefault     = fromMaybe (optionTypeDefault opts) def
  }

hostOption :: DefineOptions Host
hostOption = mongoOptions optionType_host "MongoDB host to connect to." "mongo-host" 'm' Nothing

dbOption :: DefineOptions Text
dbOption = mongoOptions optionType_text "Database in which to store documents." "database" 'd' (Just "webhook")

colOption :: DefineOptions Text
colOption = mongoOptions optionType_text "Collection in which to store documents." "collection" 'c' (Just "documents")

instance Options MongoOpts where
  defineOptions = pure MongoOpts
            <*> hostOption
            <*> dbOption
            <*> colOption
