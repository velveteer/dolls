{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import           Barbies
import           Barbies.Bare
import           Control.Monad.Trans.Writer.CPS (Writer)
import qualified Control.Monad.Trans.Writer.CPS as Writer
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Kind                      (Type)
import           Data.Schwarma.Aeson
import           Data.Schwarma.Core
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           GHC.Generics                   (Generic)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data PersonF w (f :: Type -> Type) =
  PersonF
    { name      :: Wear w f Text
    , age       :: Wear w f Int
    , likesCats :: Wear w f Bool
    } deriving Generic

instance ConstraintsB (PersonF Covered)
instance FunctorB (PersonF Covered)
instance ApplicativeB (PersonF Covered)
instance TraversableB (PersonF Covered)
instance DistributiveB (PersonF Covered)
instance BareB PersonF

type Person = PersonF Bare Identity
type PersonUpdate = PersonF Covered Maybe
type PersonFields repr = PersonF Covered (Fields repr Person)

deriving instance Show Person
deriving instance AllBF Show f (PersonF Covered) => Show (PersonF Covered f)

person :: Person
person = PersonF "Jim" 42 False

showFields :: Person -> [String]
showFields = bfoldMapC @Show (pure . show) . bcover

personFields :: Core repr => PersonFields repr
personFields =
  PersonF
    { name = required "name" name textSchema
    , age = required "age" age intSchema
    , likesCats = required "likesCats" likesCats boolSchema
    }

personDescs :: PersonF Covered (Const Text)
personDescs =
  PersonF
    { name = "First name"
    , age = "Revolutions around the sun"
    , likesCats = "Enjoys felines"
    }

personSchema :: Core repr => repr Person
personSchema = objectSchema "Person" $ bstrip <$> bsequence' personFields

personSchemaWithDescs :: Core repr => repr Person
personSchemaWithDescs = objectSchema "Person" $ bstrip <$> bsequence'
  (bzipWith (\fa (Const text) -> fa `withDescription` text) personFields personDescs)

-- $> parseEither personSchema (encodeToByteString personSchema person)

personUpdate :: PersonUpdate
personUpdate =
  PersonF
    { name = Nothing
    , age = Just 43
    , likesCats = Nothing
    }

applyUpdate :: Person -> PersonUpdate -> PersonF Covered (Writer [Text])
applyUpdate p pu = bzipWith3 update (bcover p) pu personMeta
  where
    update :: Identity a -> Maybe a -> PersonMeta a -> Writer [Text] a
    update old mNew PersonMeta{..}
      = case mNew of
          Just new -> do
            Writer.tell . pure $ T.unwords
              ["Field update for"
              , fieldName <> ":"
              , "Previous value was"
              , fieldToText (runIdentity old) <> "."
              , "New value:"
              , fieldToText new
              ]
            pure new
          Nothing ->
            pure $ runIdentity old

-- $> Writer.runWriter $ bsequence' $ applyUpdate person personUpdate

data PersonMeta a
  = PersonMeta
    { fieldName   :: Text
    , fieldToText :: a -> Text
    }

type PersonMetadata = PersonF Covered PersonMeta

personMeta :: PersonMetadata
personMeta =
  PersonF
    { name      = PersonMeta "name" id
    , age       = PersonMeta "age" (T.pack . show)
    , likesCats = PersonMeta "likesCats" (T.pack . show)
    }

person2 :: Person
person2 = PersonF "Joe" 31 True

personColumns :: PersonF Covered []
personColumns = bdistribute' [bcover person, bcover person2]
