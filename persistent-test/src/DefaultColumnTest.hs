{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module DefaultColumnTest (specsWith) where

import Database.Persist.TH
import Init
import Data.List (singleton)

share [mkPersist sqlSettings, mkMigrate "migrate1"] [persistLowerCase|
DefaultTest sql=def_test
  fieldOne Int default=0
  fieldTwo Int Maybe default=1
  fieldThree Bool default=false
  deriving Show Eq
|]

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDB = do
  describe "DefaultColumnTest" $ do
    describe "Migration" $ do
      it "sets defaults" $
        runDB $ do
          rawExecute "DROP TABLE IF EXISTS def_test;" []
          runMigration migrate1
          rawExecute "INSERT INTO def_test DEFAULT VALUES;" []

          Just (Entity _ DefaultTest{..}) <- selectFirst [] []
          liftIO $ do
            defaultTestFieldOne @?= 0
            defaultTestFieldTwo @?= Just 1
            defaultTestFieldThree @?= False

    describe "insert" $ testInsert runDB insert
    describe "insert_" $ testInsert runDB insert_
    describe "insertMany" $ testInsert runDB (insertMany . singleton)
    describe "insertMany" $ testInsert runDB (insertMany_ . singleton)

testInsert
  :: Runner SqlBackend m
  => RunDb SqlBackend m
  -> (DefaultTest -> SqlPersistT m a)
  -> Spec
testInsert runDB inserter = do
  it "respects default" $ do
    runDB $ do
      rawExecute "DROP TABLE IF EXISTS def_test;" []
      runMigration migrate1

      inserter DefaultTest
        { defaultTestFieldOne = 1
        , defaultTestFieldTwo = Nothing
        , defaultTestFieldThree = True
        }

      Just (Entity _ DefaultTest{..}) <- selectFirst [] []
      liftIO $ do
        defaultTestFieldOne @?= 1
        defaultTestFieldTwo @?= Just 1
        defaultTestFieldThree @?= True

  it "overrides defaults" $ do
    runDB $ do
      rawExecute "DROP TABLE IF EXISTS def_test;" []
      runMigration migrate1

      inserter DefaultTest
        { defaultTestFieldOne = 1
        , defaultTestFieldTwo = Just 2
        , defaultTestFieldThree = True
        }

      Just (Entity _ DefaultTest{..}) <- selectFirst [] []
      liftIO $ do
        defaultTestFieldOne @?= 1
        defaultTestFieldTwo @?= Just 2
        defaultTestFieldThree @?= True
