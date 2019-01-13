-- |
-- Module      :  Kkd.Labor7.Hamming84
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  BSD3
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides encoding and decoding the extended 
-- Hamming \( (8,4) \)-code.
--
module Kkd.Labor7.Hamming84 
( encode
, decode
, fromBits
, toBits
, fromNibbles
, toNibbles
) where

import           Data.Maybe
import           Data.Bits
import           Data.Word (Word8)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Control.Monad.Trans.Writer
import           Control.Monad.State

-- | Encodes a byte string to the byte string of extended
--   Hamming \( (8,4) \)-codes.
encode :: ByteString -> ByteString
encode = BS.concatMap encodeB 

-- | Decodes a byte string of the extended Hamming \( (8,4) \)-codes.
--   
--   Main strategy is recovering codes as many as possible.
--   If some code is unrecoverable, then code is assumed as
--   valid and decoded.
decode :: ByteString -> WriterT ByteString (State Int) ()
decode bs = 
  forM_ (chunksOf 2 bs) $ \codeword -> do
    let decoded = decodeB codeword
    when (isEmpty $ decodeN $ head $ BS.unpack codeword) $ modify (+1)
    when (isEmpty $ decodeN $ BS.unpack codeword !! 1) $ modify (+1)
    tell $ BS.singleton $ fromMaybe (unsafeDecodedB codeword) decoded
  where
    isEmpty Nothing = True
    isEmpty _ = False
    chunksOf n bs 
      | BS.null bs = []
      | otherwise   = BS.take 2 bs : chunksOf n (BS.drop 2 bs)
    unsafeDecodedB bs = let [codeHi, codeLo] = BS.unpack bs in
                        let hi = unsafeDecodedN codeHi in
                        let lo = unsafeDecodedN codeLo in
                          fromNibbles hi lo
    unsafeDecodedN w = fromBits [bit' w 3, bit' w 5, bit' w 6, bit' w 7]

-- | Encodes a byte to the sequence of extended Hamming 
--   \( (8,4) \)-code.
encodeB :: Word8 -> ByteString
encodeB b = let (hi, lo) = toNibbles b in BS.pack [encodeN hi, encodeN lo]
              
-- | Decodes a byte from the sequence of extended Hamming
--   \( (8,4) \)-code and tries to correct it if possible.
--   Otherwise returns nothing.
decodeB :: ByteString -> Maybe Word8
decodeB bs = let [codeHi, codeLo] = BS.unpack bs in
  do
    hi <- decodeN codeHi
    lo <- decodeN codeLo
    return $ fromNibbles hi lo

-- | Converts a byte to the pair of high and low nibbles.
toNibbles :: Word8 -> (Word8, Word8)
toNibbles b = (b `shiftR` 4, b .&. 0x0F)

-- | Converts a pair of high and low nibbles to the byte.
fromNibbles :: Word8 -> Word8 -> Word8
fromNibbles hi lo = (hi `shiftL` 4) .|. (lo .&. 0x0F)

-- | Converts a list of bits (from MSB to LSB) to the byte.
fromBits :: [Word8] -> Word8
fromBits bs = foldr appendBit 0 $ reverse bs where appendBit b i = 2 * i + b

-- | Converts a byte to the list of bits from MSB to LSVB.
toBits :: Word8 -> [Word8]
toBits b = reverse $ toBits' b
  where 
    toBits' 0 = []
    toBits' b = b `mod` 2 : toBits' (b `div` 2)

-- | Encodes a nibble to the corresponding extended Hamming
--   \( (8,4) \)-code.
encodeN :: Word8 -> Word8
encodeN nibble = 
  fromBits [p1, p2, d1, p3, d2, d3, d4, p4] 
  where
    bit :: Int -> Word8
    bit i = fromIntegral $ fromEnum $ testBit nibble (4 - i)
    p1 = bit 1 `xor` bit 2 `xor` bit 4
    p2 = bit 1 `xor` bit 3 `xor` bit 4
    d1 = bit 1
    p3 = bit 2 `xor` bit 3 `xor` bit 4
    d2 = bit 2
    d3 = bit 3
    d4 = bit 4
    p4 = bit 1 `xor` bit 2 `xor` bit 3

-- | Decodes an extended Hamming \( (8,4) \)-code codeword to a nibble
--   if error can be corrected. Otherwise returns nothing.
decodeN :: Word8 -> Maybe Word8
decodeN codeword 
  | parity   == 1 = Just $ decoded $ codeword `complementBit` (8 - syndrome)
  | syndrome == 0 = Just $ decoded codeword
  | otherwise     = Nothing
  where
    bit = bit' codeword 
    syndrome = let s1 = bit 1 `xor` bit 3 `xor` bit 5 `xor` bit 7 in
               let s2 = bit 2 `xor` bit 3 `xor` bit 6 `xor` bit 7 in
               let s3 = bit 4 `xor` bit 5 `xor` bit 6 `xor` bit 7 in
                  fromIntegral $ fromBits [s3, s2, s1]
    parity   = foldr xor 0 $ toBits codeword
    decoded w = fromBits [bit' w 3, bit' w 5, bit' w 6, bit' w 7]

-- | Reads a \( n \)-th bit from a given byte.
bit' :: Word8 -> Int -> Word8
bit' w i = fromIntegral $ fromEnum $ testBit w (8 - i)

