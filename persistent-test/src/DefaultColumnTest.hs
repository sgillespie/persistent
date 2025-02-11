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

    describe "insert" $ testInsertOne runDB insert
    describe "insert_" $ testInsertOne runDB insert_
    describe "insertMany" $ do
      testInsertOne runDB (insertMany . singleton)
      testInsertMany runDB insertMany
    describe "insertMany_" $ do
      testInsertOne runDB (insertMany_ . singleton)
      testInsertMany runDB insertMany_

testInsertOne
  :: Runner SqlBackend m
  => RunDb SqlBackend m
  -> (DefaultTest -> SqlPersistT m a)
  -> Spec
testInsertOne runDB inserter = do
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

testInsertMany
  :: Runner SqlBackend m
  => RunDb SqlBackend m
  -> ([DefaultTest] -> SqlPersistT m a)
  -> Spec
testInsertMany runDB inserter = do
  let defaultTest =
        DefaultTest
          { defaultTestFieldOne = 1
          , defaultTestFieldTwo = Nothing
          , defaultTestFieldThree = True
          }

  it "respects defaults with many entities" $ do
    runDB $ do
      rawExecute "DROP TABLE IF EXISTS def_test;" []
      runMigration migrate1

      inserter
        [ defaultTest
        , defaultTest
        ]

      [Entity _ ent1, Entity _ ent2] <- selectList [] []
      liftIO $ do
        defaultTestFieldOne ent1 @?= 1
        defaultTestFieldTwo ent1 @?= Just 1
        defaultTestFieldThree ent1 @?= True

        defaultTestFieldOne ent2 @?= 1
        defaultTestFieldTwo ent2 @?= Just 1
        defaultTestFieldThree ent2 @?= True

  it "overrides defaults with many entities" $ do
    runDB $ do
      rawExecute "DROP TABLE IF EXISTS def_test;" []
      runMigration migrate1

      inserter
        [ defaultTest
        , defaultTest { defaultTestFieldTwo = Just 2 }
        ]

      [Entity _ ent1, Entity _ ent2] <- selectList [] []
      liftIO $ do
        defaultTestFieldOne ent1 @?= 1
        defaultTestFieldTwo ent1 @?= Nothing
        defaultTestFieldThree ent1 @?= True

        defaultTestFieldOne ent2 @?= 1
        defaultTestFieldTwo ent2 @?= Just 2
        defaultTestFieldThree ent2 @?= True
