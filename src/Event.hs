{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Event
  ( writeEvents,
  )
where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.UUID (UUID)
import Test.QuickCheck
  ( Arbitrary (..),
    Args (..),
    elements,
    listOf,
    quickCheckWith,
    stdArgs,
  )
import Test.QuickCheck.Gen (Gen, choose)
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Monadic (monadicIO, run)

data Event = Event
  { time :: UTCTime,
    remoteAddress :: Text,
    uniqueId :: Maybe UUID,
    unIdentifiedField :: Maybe AlphaNum
  }
  deriving stock (Show)

instance ToJSON Event where
  toJSON event =
    object
      [ "$time" .= event.time,
        "$remote_address" .= event.remoteAddress,
        "$unique_id" .= event.uniqueId,
        "un_identified_field" .= event.unIdentifiedField
      ]

instance Arbitrary Event where
  arbitrary = genEvent

writeEvents :: Int -> IO ()
writeEvents numOfItems =
  quickCheckWith (stdArgs {maxSuccess = numOfItems}) $ \event ->
    monadicIO $ do
      run $ writeEvent event
      pure True

writeEvent :: Event -> IO ()
writeEvent event =
  BS.appendFile "events" ((BS.toStrict $ encode event) <> "\n")

genEvent :: Gen Event
genEvent = do
  time <- genDate
  remoteAddress <- genIpAddress
  uniqueId <- arbitrary
  unIdentifiedField <- arbitrary :: Gen (Maybe AlphaNum)
  pure $
    Event
      { time,
        remoteAddress,
        uniqueId,
        unIdentifiedField
      }

genDate :: Gen UTCTime
genDate = do
  day <- choose (0, 30)
  pure $ utcTimeFormat day
  where
    utcTimeFormat :: Int -> UTCTime
    utcTimeFormat day =
      UTCTime
        (fromGregorian 2023 06 day)
        (secondsToDiffTime 0)

genIpAddress :: Gen Text
genIpAddress = do
  network1 <- choose (0, 999)
  network2 <- choose (0, 999)
  host1 <- choose (0, 999)
  host2 <- choose (0, 999)
  pure $ ipAddressFormat network1 network2 host1 host2
  where
    ipAddressFormat :: Int -> Int -> Int -> Int -> Text
    ipAddressFormat network1 network2 host1 host2 =
      show network1
        <> "."
        <> show network2
        <> "."
        <> show host1
        <> "."
        <> show host2

newtype AlphaNum = AlphaNum {alphaNum :: Text}
  deriving stock (Show)

instance ToJSON AlphaNum where
  toJSON AlphaNum {alphaNum} = toJSON alphaNum

instance Arbitrary AlphaNum where
  arbitrary = genAlphaNum

genAlphaNum :: Gen AlphaNum
genAlphaNum = fmap (AlphaNum <<< T.pack <<< concat) $ listOf $ do
  char <- elements ['a' .. 'z']
  num <- elements ['0' .. '9']
  pure $ [char, num]
