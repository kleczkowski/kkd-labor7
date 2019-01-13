-- |
-- Module      :  Kkd.Labor7.Check
-- Copyright   :  Konrad Kleczkowski 2019
-- License     :  BSD3
--
-- Maintainer  :  konrad.kleczkowski@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Module that implements nibble check.
--
module Kkd.Labor7.Check 
( check
) where

import           Kkd.Labor7.Hamming84

import           Control.Monad
import           Control.Monad.State

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS           

check :: ByteString -> ByteString -> State Int ()
check bs1 bs2 = let bytePairs = BS.zip bs1 bs2 in
                  forM_ bytePairs $ \ (b1, b2) -> do
                    let (n11, n21) = toNibbles b1
                    let (n12, n22) = toNibbles b2
                    when (n11 /= n12) $ modify (+1)
                    when (n21 /= n22) $ modify (+1)
