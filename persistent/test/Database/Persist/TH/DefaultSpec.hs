{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Persist.TH.DefaultSpec where

import TemplateTestImports

mkPersist sqlSettings [persistLowerCase|

HasDefault
    name String
    blargh Int Maybe default=0

    deriving Eq Show
|]

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec =
  describe "DefaultSpec" $ do
    let
      def = entityDef (Proxy @HasDefault)

    describe "getEntityFields" $ do
      it "has two fields" $ do
        length (getEntityFields def) `shouldBe` 2

    describe "getEntityFieldsDatabase" $ do
      it "has two fields" $ do
        length (getEntityFieldsDatabase def) `shouldBe` 2

    describe "toPersistFields" $ do
      it "has two fields" $ do
        map toPersistValue (toPersistFields (HasDefault "asdf" Nothing))
          `shouldBe`
          [ PersistText ("asdf" :: Text)
          , PersistNull
          ]

    describe "fromPersistValues" $ do
      it "should work with only item in list" $ do
        fromPersistValues [PersistText "Hello", PersistNull]
          `shouldBe` Right (HasDefault "Hello" Nothing)
