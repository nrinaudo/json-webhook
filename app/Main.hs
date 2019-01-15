{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cmd
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson                (object, (.=))
import           Data.Document
import           Data.Text
import           Data.Time
import           Database.MongoDB          hiding (Document, runCommand)
import           Network.HTTP.Types.Status
import           Options
import           Web.Scotty                hiding (Options)

-------------------------------------------------------------------------------------------------------------------------
-- Mongo helpers
-------------------------------------------------------------------------------------------------------------------------
data Mongo = Mongo
  { mongoCon :: Pipe
  , mongoDb  :: MongoDb
  , mongoCol :: MongoCol
  }

runQuery :: MonadIO m => Mongo -> Action m a -> m a
runQuery mongo = access (mongoCon mongo) master (unwrapDb . mongoDb $ mongo)

mongoConnect :: MongoOpts -> IO Mongo
mongoConnect conf =  do
  pipe      <- connect (mongoOptsHost conf)
  let mongo = Mongo pipe (mongoOptsDb conf) (mongoOptsCol conf)
  _         <- runQuery mongo $ ensureIndex (dateIndex mongo)
  return mongo
    where
      dateIndex mongo = Index (colName mongo) ["date" =: (1 :: Int)] "creation_date" False False (Just 86400)

colName :: Mongo -> Text
colName = unwrapCol . mongoCol

-------------------------------------------------------------------------------------------------------------------------
-- Web service
-------------------------------------------------------------------------------------------------------------------------
receiveAd :: Mongo -> ActionM ()
receiveAd mongo = do
  domain  <- param "domain"
  time    <- lift getCurrentTime
  content <- jsonData
  _       <- lift $ runQuery mongo $ insertDocument (colName mongo) (Document time domain content)
  status status201

listAds :: Mongo -> ActionM ()
listAds mongo = do
  domain <- param "domain"
  ads    <- lift $ runQuery mongo $ findDocuments (colName mongo) domain
  json $ documentContent <$> ads

countAds :: Mongo -> ActionM ()
countAds mongo = do
  domain <- param "domain"
  total  <- lift $ runQuery mongo $ countDocuments (colName mongo) domain
  json $ object ["count" .= total]

runWith :: Int -> Mongo -> IO ()
runWith portNum mongo =
  scotty portNum $ do
    put "/:domain" (receiveAd mongo)
    get "/:domain" (listAds mongo)
    get "/:domain/count" (countAds mongo)

main :: IO ()
main = runCommand $ \(CmdOpts opts portNum) _ -> do
  mongo <- mongoConnect opts
  _     <- runWith portNum mongo
  close (mongoCon mongo)
