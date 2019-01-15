{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cmd
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
-- Mongo initialisation & teardown
-------------------------------------------------------------------------------------------------------------------------
data Mongo = Mongo {
  mongoCon        :: Pipe,
  mongoDatabase   :: Text,
  mongoCollection :: Text
  }

mongoConnect :: MongoOpts -> IO Mongo
mongoConnect (MongoOpts hst db col) =  do
  pipe <- connect hst
  _    <- access pipe master db $ ensureIndex dateIndex
  return $ Mongo pipe db col
    where dateIndex = Index col ["date" =: (1 :: Int)] "creation_date" False False (Just 86400)

-------------------------------------------------------------------------------------------------------------------------
-- Web service
-------------------------------------------------------------------------------------------------------------------------
receiveAd :: Mongo -> ActionM ()
receiveAd (Mongo pipe db col) = do
  domain  <- param "domain"
  time    <- lift getCurrentTime
  content <- jsonData
  _       <- lift $ access pipe master db $ insertDocument col (Document time domain content)
  status status201

listAds :: Mongo -> ActionM ()
listAds (Mongo pipe db col) = do
  domain <- param "domain"
  ads    <- lift $ access pipe master db $ findDocuments col domain
  json $ documentContent <$> ads

countAds :: Mongo -> ActionM ()
countAds (Mongo pipe db col) = do
  domain <- param "domain"
  total  <- lift $ access pipe master db $ countDocuments col domain
  json $ object ["count" .= total]

runWith :: Int -> Mongo -> IO ()
runWith port mongo =
  scotty port $ do
    put "/:domain" (receiveAd mongo)
    get "/:domain" (listAds mongo)
    get "/:domain/count" (countAds mongo)

main :: IO ()
main = runCommand $ \(CmdOpts mongoOpts port) _ -> do
  mongo <- mongoConnect mongoOpts
  _     <- runWith port mongo
  close (mongoCon mongo)
