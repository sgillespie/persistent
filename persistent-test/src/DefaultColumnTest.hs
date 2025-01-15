{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module DefaultColumnTest (specsWith) where

import Database.Persist.TH
import Init

share [mkPersist sqlSettings, mkMigrate "migrate1"] [persistLowerCase|
DefaultTest sql=def_test
  fieldOne Int default=0
  fieldTwo Int Maybe default=1
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

    describe "insert" $ do
      it "respects default" $ do
        runDB $ do
          rawExecute "DROP TABLE IF EXISTS def_test;" []
          runMigration migrate1

          insert_ DefaultTest
            { defaultTestFieldOne = 1
            , defaultTestFieldTwo = Nothing
            }

          Just (Entity _ DefaultTest{..}) <- selectFirst [] []
          liftIO $ do
            defaultTestFieldOne @?= 1
            defaultTestFieldTwo @?= Just 1

      it "overrides defaults" $ do
        runDB $ do
          rawExecute "DROP TABLE IF EXISTS def_test;" []
          runMigration migrate1

          insert_ DefaultTest
            { defaultTestFieldOne = 1
            , defaultTestFieldTwo = Just 2
            }

          Just (Entity _ DefaultTest{..}) <- selectFirst [] []
          liftIO $ do
            defaultTestFieldOne @?= 1
            defaultTestFieldTwo @?= Just 2
