module Main where

import           Kkd.Labor7.Hamming84
import           Kkd.Labor7.Noise
import           Kkd.Labor7.Check

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Control.Monad.Trans.Writer
import           Control.Monad.State

import           System.Environment
import           System.Random

main :: IO ()
main = do
  args <- getArgs
  let mode = head args
  when (mode == "encode") mainEncode
  when (mode == "decode") mainDecode
  when (mode == "noise" ) mainNoise
  when (mode == "check" ) mainCheck
  return ()

mainEncode :: IO () 
mainEncode = do
  [_, fileIn, fileOut] <- getArgs
  bsIn <- BS.readFile fileIn
  BS.writeFile fileOut $ encode bsIn

mainDecode :: IO ()
mainDecode = do
  [_, fileIn, fileOut] <- getArgs
  bsIn <- BS.readFile fileIn
  let runDecode = runState $ runWriterT $ decode bsIn 
  let (((), bsOut), fails) = runDecode 0 
  print fails
  BS.writeFile fileOut bsOut

mainNoise :: IO ()
mainNoise = do
  [_, pStr, fileIn, fileOut] <- getArgs
  let p = read pStr :: Double
  bsIn <- BS.readFile fileIn
  stdGen <- getStdGen
  let runNoise p bs = (runState $ noise p bs) stdGen
  let (bsOut, _) = runNoise p bsIn
  BS.writeFile fileOut bsOut

mainCheck :: IO ()
mainCheck = do
  [_, fileIn1, fileIn2] <- getArgs
  bsIn1 <- BS.readFile fileIn1
  bsIn2 <- BS.readFile fileIn2
  let (_, diffs) = (runState $ check bsIn1 bsIn2) 0
  print diffs

