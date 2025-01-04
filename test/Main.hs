module Main where

import qualified Data.Char as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Bech32 as Bech32
import qualified Data.ByteString.Bech32m as Bech32m
import qualified Data.ByteString.Base32 as B32
import Test.Tasty
import qualified Test.Tasty.QuickCheck as Q
import qualified Reference.Bech32 as R

newtype BS = BS BS.ByteString
  deriving (Eq, Show)

data ValidInput = ValidInput BS.ByteString BS.ByteString
  deriving (Eq, Show)

instance Q.Arbitrary ValidInput where
  arbitrary = do
    h <- hrp
    let l = 83 - BS.length h
        a = l * 5 `quot` 8
    b <- bytes a
    pure (ValidInput h b)

instance Q.Arbitrary BS where
  arbitrary = do
    b <- bytes 1024
    pure (BS b)

hrp :: Q.Gen BS.ByteString
hrp = do
  l <- Q.chooseInt (1, 83)
  v <- Q.vectorOf l (Q.choose (33, 126))
  pure (B8.map C.toLower (BS.pack v))

bytes :: Int -> Q.Gen BS.ByteString
bytes k = do
  l <- Q.chooseInt (0, k)
  v <- Q.vectorOf l Q.arbitrary
  pure (BS.pack v)

matches_reference :: ValidInput -> Bool
matches_reference (ValidInput h b) =
  let ref = R.bech32Encode h (R.toBase32 (BS.unpack b))
      our = Bech32.encode h b
  in  ref == our

bech32_decode_inverts_encode :: ValidInput -> Bool
bech32_decode_inverts_encode (ValidInput h b) = case Bech32.encode h b of
  Nothing -> error "generated faulty input"
  Just enc -> case Bech32.decode enc of
    Nothing -> False
    Just (h', dat) -> h == h' && b == dat

bech32m_decode_inverts_encode :: ValidInput -> Bool
bech32m_decode_inverts_encode (ValidInput h b) = case Bech32m.encode h b of
  Nothing -> error "generated faulty input"
  Just enc -> case Bech32m.decode enc of
    Nothing -> False
    Just (h', dat) -> h == h' && b == dat

base32_decode_inverts_encode :: BS -> Bool
base32_decode_inverts_encode (BS bs) = case B32.decode (B32.encode bs) of
  Nothing -> False
  Just b  -> b == bs

main :: IO ()
main = defaultMain $ testGroup "ppad-bech32" [
    testGroup "base32" [
      Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 1000 base32_decode_inverts_encode
    ]
  , testGroup "bech32" [
      Q.testProperty "Bech32.encode ~ R.bech32Encode" $
        Q.withMaxSuccess 1000 matches_reference
    , Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 1000 bech32_decode_inverts_encode
    ]
  , testGroup "bech32m" [
      Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 1000 bech32m_decode_inverts_encode
    ]
  ]

