-- |
-- Module      :  Kkd.Labor7.Noise
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  BSD3
--
-- Maintainer  :  konrad.kleczkowski@hotmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Module that implements noising of the file being decoded
--
module Kkd.Labor7.Noise 
( noise
) where

import Kkd.Labor7.Hamming84

import           Data.Bits
import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Control.Monad.State
import           Control.Monad.Extra
import           System.Random

-- | Reads a byte string and turns it to the noised one using the standard
--   generator.
noise :: Double -> ByteString -> State StdGen ByteString
noise p bs =
  fmap BS.pack $ forM (BS.unpack bs) $ scramble p 
  where 
    scramble :: Double -> Word8 -> State StdGen Word8
    scramble p b =
      fmap fromBits $ forM (toBits b) $ \bit -> do
        t <- state random
        ifM (pure $ t <= p) (return $ bit `xor` 1) (return bit)

