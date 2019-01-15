{-# LANGUAGE OverloadedStrings #-}

module Data.Document (
  Document (Document),
  documentContent,
  documentCreatedAt,
  documentDomain,
  insertDocument,
  findDocuments,
  countDocuments
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson             as AESON
import           Data.Bson              as BSON hiding (Document)
import           Data.HashMap.Strict    as Map (fromList, toList)
import           Data.Maybe             (catMaybes)
import           Data.Scientific
import           Data.Time
import           Data.Vector            as Vector (fromList, toList)
import           Database.MongoDB       as MongoDB hiding (Document)

data Document = Document {
  documentCreatedAt :: UTCTime,
  documentDomain    :: String,
  documentContent   :: AESON.Value
  } deriving (Eq, Show)

------------------------------------------------------------------------------------------------------------------------
-- MongoDB queries
------------------------------------------------------------------------------------------------------------------------

insertDocument :: MonadIO m => Collection -> Document -> Action m MongoDB.Value
insertDocument col doc = insert col (documentToObj doc)

findDocuments :: MonadIO m => Collection -> String ->  Action m [Document]
findDocuments col domain = do
  cursor  <- find (select  ["domain" =: domain] col) { sort = ["date" =: (1 :: Int)] }
  objects <- rest cursor
  -- Filter out invalid documents. It'd probably be better to log something here.
  return . catMaybes $ objToDocument <$> objects

countDocuments :: MonadIO m => Collection -> String ->  Action m Int
countDocuments col domain = count (select  ["domain" =: domain] col)

------------------------------------------------------------------------------------------------------------------------
-- BSON.Val instance
------------------------------------------------------------------------------------------------------------------------

documentToObj :: Document -> [Field]
documentToObj (Document date domain content) = [
    "date"    := val date,
    "domain"  := val domain,
    "content" := jsonToBSON content
    ]

objToDocument :: [Field] -> Maybe Document
objToDocument doc = do
  date    <- doc !? "date"
  domain  <- doc !? "domain"
  content <- bsonToJSON $ valueAt "content" doc
  return $ Document date domain content

instance BSON.Val Document where
  val = val . documentToObj

  cast' (BSON.Doc doc) = objToDocument doc
  cast' _              = Nothing

jsonToBSON :: AESON.Value -> BSON.Value
jsonToBSON (AESON.Object obj) = BSON.Doc $ tupleToField <$> Map.toList obj
    where tupleToField (k, v) = k := jsonToBSON v
jsonToBSON (AESON.Number num) = either BSON.Float BSON.Int64 (floatingOrInteger num)
jsonToBSON (AESON.Array arr)  = BSON.Array . Vector.toList $ jsonToBSON <$> arr
jsonToBSON (AESON.String str) = BSON.String str
jsonToBSON (AESON.Bool bool)  = BSON.Bool bool
jsonToBSON AESON.Null         = BSON.Null

bsonToJSON :: BSON.Value -> Maybe AESON.Value
bsonToJSON (BSON.Float float) = return $ toJSON float
bsonToJSON (BSON.String str)  = return $ toJSON str
bsonToJSON (BSON.Doc doc)     = AESON.Object . Map.fromList <$> traverse fieldToTuple doc
  where fieldToTuple (k := v) = (\v2 -> (k, v2)) <$> bsonToJSON v
bsonToJSON (BSON.Array arr)   = AESON.Array . Vector.fromList <$> traverse bsonToJSON arr
bsonToJSON (BSON.Bool bool)   = return $ toJSON bool
bsonToJSON BSON.Null          = return AESON.Null
bsonToJSON (BSON.Int32 num)   = return $ toJSON num
bsonToJSON (BSON.Int64 num)   = return $ toJSON num
bsonToJSON _                  = Nothing
