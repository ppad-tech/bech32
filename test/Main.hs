{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Data.Char as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Bech32 as Bech32
import qualified Data.ByteString.Bech32.Internal as BI
import qualified Data.ByteString.Bech32m as Bech32m
import qualified Data.ByteString.Base32 as B32
import Data.Word (Word8)
import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as Q
import qualified Reference.Bech32 as R

newtype BS = BS BS.ByteString
  deriving (Eq, Show)

data ValidInput = ValidInput BS.ByteString BS.ByteString
  deriving (Eq, Show)

data InvalidInput = InvalidInput BS.ByteString BS.ByteString
  deriving (Eq, Show)

-- | A list of 5-bit values.
newtype Word5s = Word5s [Word8]
  deriving (Eq, Show)

-- | A canonical base32 string with one character replaced by a byte
--   outside the bech32 character set.
newtype Corrupted = Corrupted BS.ByteString
  deriving (Eq, Show)

instance Q.Arbitrary ValidInput where
  arbitrary = do
    h <- hrp
    let l = 83 - BS.length h
        a = l * 5 `quot` 8
    b <- bytes a
    pure (ValidInput h b)

instance Q.Arbitrary InvalidInput where
  arbitrary = do
    h <- invalid_hrp
    let l = 83 - BS.length h
        a = l * 5 `quot` 8
    b <- bytes a
    pure (InvalidInput h b)

instance Q.Arbitrary BS where
  arbitrary = do
    b <- bytes 1024
    pure (BS b)

instance Q.Arbitrary Word5s where
  arbitrary = do
    l <- Q.chooseInt (0, 100)
    Word5s <$> Q.vectorOf l (Q.choose (0, 31))

instance Q.Arbitrary Corrupted where
  arbitrary = do
    l <- Q.chooseInt (1, 100)
    b <- BS.pack <$> Q.vectorOf l Q.arbitrary
    let enc = B32.encode b
    i <- Q.chooseInt (0, BS.length enc - 1)
    c <- Q.elements non_charset
    pure (Corrupted (BS.take i enc <> BS.singleton c <> BS.drop (i + 1) enc))

charset :: BS.ByteString
charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

non_charset :: [Word8]
non_charset = filter (\b -> not (BS.elem b charset)) [0 .. 255]

hrp :: Q.Gen BS.ByteString
hrp = do
  l <- Q.chooseInt (1, 83)
  v <- Q.vectorOf l (Q.choose (33, 126))
  pure (B8.map C.toLower (BS.pack v))

invalid_hrp :: Q.Gen BS.ByteString
invalid_hrp = do
  l <- Q.oneof [pure 0, Q.chooseInt (84, 100)]
  v <- Q.vectorOf l (Q.oneof [Q.choose (0, 32), Q.choose (127, 255)])
  pure (B8.map C.toLower (BS.pack v))

bytes :: Int -> Q.Gen BS.ByteString
bytes k = do
  l <- Q.chooseInt (0, k)
  v <- Q.vectorOf l Q.arbitrary
  pure (BS.pack v)

lower :: BS.ByteString -> BS.ByteString
lower = B8.map C.toLower

upper :: BS.ByteString -> BS.ByteString
upper = B8.map C.toUpper

-- properties -----------------------------------------------------------------

matches_reference :: ValidInput -> Bool
matches_reference (ValidInput h b) =
  let ref = R.bech32Encode h (R.toBase32 (BS.unpack b))
      our = Bech32.encode h b
  in  ref == our

bech32_decode_inverts_encode :: ValidInput -> Bool
bech32_decode_inverts_encode (ValidInput h b) = case Bech32.encode h b of
  Nothing -> False
  Just enc -> case Bech32.decode enc of
    Nothing -> False
    Just (h', dat) -> h == h' && b == dat

bech32m_decode_inverts_encode :: ValidInput -> Bool
bech32m_decode_inverts_encode (ValidInput h b) = case Bech32m.encode h b of
  Nothing -> False
  Just enc -> case Bech32m.decode enc of
    Nothing -> False
    Just (h', dat) -> h == h' && b == dat

bech32_decode_inverts_encode_upper :: ValidInput -> Bool
bech32_decode_inverts_encode_upper (ValidInput h b) =
  case Bech32.encode h b of
    Nothing -> False
    Just (upper -> enc) ->
      Bech32.verify enc && Bech32.decode enc == Just (h, b)

bech32m_decode_inverts_encode_upper :: ValidInput -> Bool
bech32m_decode_inverts_encode_upper (ValidInput h b) =
  case Bech32m.encode h b of
    Nothing -> False
    Just (upper -> enc) ->
      Bech32m.verify enc && Bech32m.decode enc == Just (h, b)

base32_decode_inverts_encode :: BS -> Bool
base32_decode_inverts_encode (BS bs) = case B32.decode (B32.encode bs) of
  Nothing -> False
  Just b  -> b == bs

base32_decode_matches_reference :: Word5s -> Bool
base32_decode_matches_reference (Word5s ws) =
  let ours = B32.decode (BI.as_base32 (BS.pack ws))
      ref  = R.toBase256 (fmap R.word5 ws)
  in  ours == fmap BS.pack ref

base32_decode_rejects_invalid_char :: Corrupted -> Bool
base32_decode_rejects_invalid_char (Corrupted bs) = B32.decode bs == Nothing

bech32_invalid_input_fails_encode :: InvalidInput -> Bool
bech32_invalid_input_fails_encode (InvalidInput h b) =
  case Bech32.encode h b of
    Nothing -> True
    Just _ -> False

bech32m_invalid_input_fails_encode :: InvalidInput -> Bool
bech32m_invalid_input_fails_encode (InvalidInput h b) =
  case Bech32m.encode h b of
    Nothing -> True
    Just _ -> False

-- vectors --------------------------------------------------------------------

-- BIP173 and BIP350 checksum test vectors. Each valid vector is
-- paired with whether its data part converts to bytes: the long
-- all-'l' bech32m vector has a valid checksum, but its data part
-- leaves nonzero padding bits, so 'decode' (which returns bytes)
-- rejects it.

bip173_valid :: [(BS.ByteString, Bool)]
bip173_valid = [
    ("A12UEL5L", True)
  , ("a12uel5l", True)
  , ("an83characterlonghumanreadablepartthatcontainsthenumber1\
        \andtheexcludedcharactersbio1tt5tgs", True)
  , ("abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw", True)
  , ("11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq\
        \qqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j", True)
  , ("split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w", True)
  , ("?1ezyfcl", True)
  ]

bip173_invalid :: [BS.ByteString]
bip173_invalid = [
    BS.cons 0x20 "1nwldj5"
  , BS.cons 0x7f "1axkwrx"
  , BS.cons 0x80 "1eym55h"
  , "an84characterslonghumanreadablepartthatcontainsthenumber\
      \1andtheexcludedcharactersbio1569pvx"
  , "pzry9x0s0muk"
  , "1pzry9x0s0muk"
  , "x1b4n0q5v"
  , "li1dgmt3"
  , BS.snoc "de1lg7wt" 0xff
  , "A1G7SGD8"
  , "10a06t8"
  , "1qzzfhee"
  ]

bip350_valid :: [(BS.ByteString, Bool)]
bip350_valid = [
    ("A1LQFN3A", True)
  , ("a1lqfn3a", True)
  , ("an83characterlonghumanreadablepartthatcontainsthetheexcl\
        \udedcharactersbioandnumber11sg7hg6", True)
  , ("abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx", True)
  , ("11llllllllllllllllllllllllllllllllllllllllllllllllllllll\
        \llllllllllllllllllllllllllllludsr8", False)
  , ("split1checkupstagehandshakeupstreamerranterredcaperredlc445v", True)
  , ("?1v759aa", True)
  ]

bip350_invalid :: [BS.ByteString]
bip350_invalid = [
    BS.cons 0x20 "1xj0phk"
  , BS.cons 0x7f "1g6xzxy"
  , BS.cons 0x80 "1vctc34"
  , "an84characterslonghumanreadablepartthatcontainsthetheexc\
      \ludedcharactersbioandnumber11d6pts4"
  , "qyrz8wqd2c9m"
  , "1qyrz8wqd2c9m"
  , "y1b0jsk6g"
  , "lt1igcx5c0"
  , "in1muywd"
  , "mm1crxm3i"
  , "au1s5cgom"
  , "M1VUXWEZ"
  , "16plkw9"
  , "1p2gdwpf"
  ]

valid_vector
  :: (BS.ByteString -> Bool)
  -> (BS.ByteString -> Maybe (BS.ByteString, BS.ByteString))
  -> (BS.ByteString -> BS.ByteString -> Maybe BS.ByteString)
  -> (BS.ByteString, Bool)
  -> TestTree
valid_vector ver dec enc (s, convertible) = H.testCase (show s) $ do
  H.assertBool "verify" (ver s)
  if   convertible
  then H.assertEqual "decode/encode" (Just (lower s)) $ do
         (h, d) <- dec s
         enc h d
  else H.assertEqual "decode" Nothing (dec s)

invalid_vector
  :: (BS.ByteString -> Maybe (BS.ByteString, BS.ByteString))
  -> BS.ByteString
  -> TestTree
invalid_vector dec s = H.testCase (show s) $
  H.assertEqual mempty Nothing (dec s)

-- valid in the other encoding, so invalid in this one
cross_vector
  :: (BS.ByteString -> Bool)
  -> (BS.ByteString -> Maybe (BS.ByteString, BS.ByteString))
  -> (BS.ByteString, Bool)
  -> TestTree
cross_vector ver dec (s, _) = H.testCase (show s) $ do
  H.assertBool "verify" (not (ver s))
  H.assertEqual "decode" Nothing (dec s)

-- a string whose checksum is computed over an uppercase hrp, with a
-- lowercase data part
upper_hrp_checksum :: BI.Encoding -> BS.ByteString
upper_hrp_checksum enc =
  let h  = "BC"
      ws = BS.pack [0 .. 7]
  in  BS.concat
        [h, "1", BI.as_base32 ws, BI.as_base32 (BI.create_checksum enc h ws)]

mixed_case :: TestTree
mixed_case = testGroup "mixed case" [
    H.testCase "bech32 (mixed vector)" $ do
      let s = "A12uEL5L"
      H.assertBool "verify" (not (Bech32.verify s))
      H.assertEqual "decode" Nothing (Bech32.decode s)
  , H.testCase "bech32m (mixed vector)" $ do
      let s = "a1LQFN3A"
      H.assertBool "verify" (not (Bech32m.verify s))
      H.assertEqual "decode" Nothing (Bech32m.decode s)
  , H.testCase "bech32 (uppercase-hrp checksum)" $ do
      let s = upper_hrp_checksum BI.Bech32
      H.assertBool "verify" (not (Bech32.verify s))
      H.assertEqual "decode" Nothing (Bech32.decode s)
  , H.testCase "bech32m (uppercase-hrp checksum)" $ do
      let s = upper_hrp_checksum BI.Bech32m
      H.assertEqual "construction" "BC1qpzry9x8gchca4" s
      H.assertBool "verify" (not (Bech32m.verify s))
      H.assertEqual "decode" Nothing (Bech32m.decode s)
  , H.testCase "bech32 (uppercase hrp, lowercase data)" $ do
      let s = "HI1df6x7cnfdcs8wctnyp5x2un9wed5st"
      H.assertBool "verify" (not (Bech32.verify s))
      H.assertEqual "decode" Nothing (Bech32.decode s)
  , H.testCase "bech32m (uppercase hrp, lowercase data)" $ do
      let s = "HI1df6x7cnfdcs8wctnyp5x2un9m9ac4f"
      H.assertBool "verify" (not (Bech32m.verify s))
      H.assertEqual "decode" Nothing (Bech32m.decode s)
  ]

base32_rejects :: TestTree
base32_rejects = testGroup "base32 decode" [
    H.testCase "invalid tail lengths (1, 3, 6 mod 8)" $
      mapM_ (\s -> H.assertEqual (show s) Nothing (B32.decode s)) [
          "q", "qqq", "qqqqqq"
        , "qqqqqqqqq", "qqqqqqqqqqq", "qqqqqqqqqqqqqq"
        ]
  , H.testCase "nonzero padding bits" $
      mapM_ (\s -> H.assertEqual (show s) Nothing (B32.decode s)) [
          "qp", "qqqp", "qqqqp", "qqqqqqp"
        , "qqqqqqqqqp", "qqqqqqqqqqqp"
        ]
  , H.testCase "zero padding bits" $ do
      H.assertEqual "qq" (Just "\NUL") (B32.decode "qq")
      H.assertEqual "qy" (Just "\SOH") (B32.decode "qy")
      H.assertEqual "qqqqqqqqqy" (Just "\NUL\NUL\NUL\NUL\NUL\SOH")
        (B32.decode "qqqqqqqqqy")
  , H.testCase "invalid characters" $
      mapM_ (\s -> H.assertEqual (show s) Nothing (B32.decode s)) [
          "qqqqqqqb", "qqqqqqqi", "qqqqqqqo", "qqqqqqq1"
        , "Qqqqqqqq", "qqqq qqq", BS.snoc "qqqqqqq" 0x80
        , BS.snoc "qqqqqqq" 0xff, BS.snoc "qqqqqqqqq" 0x00
        , "qqqqqqqqqb"
        ]
  ]

main :: IO ()
main = defaultMain $ testGroup "ppad-bech32" [
    testGroup "base32" [
      Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 1000 base32_decode_inverts_encode
    , Q.testProperty "decode ~ reference" $
        Q.withMaxSuccess 1000 base32_decode_matches_reference
    , Q.testProperty "decode rejects invalid characters" $
        Q.withMaxSuccess 1000 base32_decode_rejects_invalid_char
    , base32_rejects
    ]
  , testGroup "bech32" [
      Q.testProperty "Bech32.encode ~ R.bech32Encode" $
        Q.withMaxSuccess 1000 matches_reference
    , Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 1000 bech32_decode_inverts_encode
    , Q.testProperty "decode . upper . encode ~ id" $
        Q.withMaxSuccess 1000 bech32_decode_inverts_encode_upper
    , Q.testProperty "invalid bech32 input fails to encode" $
        Q.withMaxSuccess 1000 bech32_invalid_input_fails_encode
    , testGroup "BIP173 valid vectors" $
        fmap (valid_vector Bech32.verify Bech32.decode Bech32.encode)
          bip173_valid
    , testGroup "BIP173 invalid vectors" $
        fmap (invalid_vector Bech32.decode) bip173_invalid
    , testGroup "BIP350 valid vectors (invalid as bech32)" $
        fmap (cross_vector Bech32.verify Bech32.decode) bip350_valid
    ]
  , testGroup "bech32m" [
      Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 1000 bech32m_decode_inverts_encode
    , Q.testProperty "decode . upper . encode ~ id" $
        Q.withMaxSuccess 1000 bech32m_decode_inverts_encode_upper
    , Q.testProperty "invalid bech32m input fails to encode" $
        Q.withMaxSuccess 1000 bech32m_invalid_input_fails_encode
    , testGroup "BIP350 valid vectors" $
        fmap (valid_vector Bech32m.verify Bech32m.decode Bech32m.encode)
          bip350_valid
    , testGroup "BIP350 invalid vectors" $
        fmap (invalid_vector Bech32m.decode) bip350_invalid
    , testGroup "BIP173 valid vectors (invalid as bech32m)" $
        fmap (cross_vector Bech32m.verify Bech32m.decode) bip173_valid
    ]
  , mixed_case
  ]
