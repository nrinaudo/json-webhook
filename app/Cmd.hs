{-# LANGUAGE OverloadedStrings #-}

module Cmd
  ( CmdOpts (..)
  , MongoOpts (..)
  , MongoDb (..)
  , MongoCol (..)
  ) where

import           Data.Bifunctor
import           Data.Text        hiding (group)
import           Database.MongoDB hiding (Group, group)
import           Options

-------------------------------------------------------------------------------------------------------------------------
-- Global options
-------------------------------------------------------------------------------------------------------------------------
data CmdOpts = CmdOpts
  { mongoOpts :: MongoOpts
  , port      :: Int
  } deriving (Eq, Show)

instance Options CmdOpts where
  defineOptions = CmdOpts
                  <$> defineOptions
                  <*> simpleOption "port" 3000 "Port on which to listen for incoming HTTP calls."

-------------------------------------------------------------------------------------------------------------------------
-- MongoDB options
-------------------------------------------------------------------------------------------------------------------------

newtype MongoDb = MongoDb { unwrapDb :: Text } deriving (Eq, Show)
newtype MongoCol = MongoCol { unwrapCol :: Text } deriving (Eq, Show)

data MongoOpts = MongoOpts
  { mongoOptsHost :: Host
  , mongoOptsDb   :: MongoDb
  , mongoOptsCol  :: MongoCol
  } deriving (Eq, Show)

mongoGroup :: Group
mongoGroup = group "mongodb" "MongoDB options" "Show MongoDB specific options"

optionType_host :: OptionType Host
optionType_host = optionType "mongodb host" (host "localhost") parseHost showHostPort
    where
      parseHost = first (\e -> "Invalid mongodb host: " <> e) . readHostPortM

optionType_db :: OptionType MongoDb
optionType_db = optionType "mongo database" (MongoDb "webhook") (Right . MongoDb . pack) show

optionType_col :: OptionType MongoCol
optionType_col = optionType "mongo collection" (MongoCol "documents") (Right . MongoCol . pack) show

-- | Helper for defining mongo-specific options.
mongoOptions :: OptionType a -> String -> String -> Char -> DefineOptions a
mongoOptions opts desc long short = defineOption opts $ \o -> o
  { optionLongFlags   = [long]
  , optionShortFlags  = [short]
  , optionDescription = desc
  , optionGroup       = Just mongoGroup
  }

hostOption :: DefineOptions Host
hostOption = mongoOptions optionType_host "MongoDB host to connect to." "mongo-host" 'm'

dbOption :: DefineOptions MongoDb
dbOption = mongoOptions optionType_db "Database in which to store documents." "database" 'd'

colOption :: DefineOptions MongoCol
colOption = mongoOptions optionType_col "Collection in which to store documents." "collection" 'c'

instance Options MongoOpts where
  defineOptions = pure MongoOpts
            <*> hostOption
            <*> dbOption
            <*> colOption
