module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Bech32 as Bech32
import Test.Tasty
import qualified Test.Tasty.QuickCheck as Q
import qualified Reference.Bech32 as R

data Input = Input BS.ByteString BS.ByteString
  deriving (Eq, Show)

instance Q.Arbitrary Input where
  arbitrary = do
    h <- hrp
    b <- bytes (83 - BS.length h)
    pure (Input h b)

hrp :: Q.Gen BS.ByteString
hrp = do
  l <- Q.chooseInt (1, 83)
  v <- Q.vectorOf l (Q.choose (33, 126))
  pure (BS.pack v)

bytes :: Int -> Q.Gen BS.ByteString
bytes k = do
  l <- Q.chooseInt (0, k)
  v <- Q.vectorOf l Q.arbitrary
  pure (BS.pack v)

matches :: Input -> Bool
matches (Input h b) =
  let ref = R.bech32Encode h (R.toBase32 (BS.unpack b))
      our = Bech32.encode h b
  in  ref == our

main :: IO ()
main = defaultMain $
  Q.testProperty "encoding matches reference" matches
