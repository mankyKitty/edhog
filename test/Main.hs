{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)

import           Hedgehog

import           System.Exit    (exitFailure, exitSuccess)
import           System.IO      (BufferMode (..), hSetBuffering)

import qualified Data.Text.IO as T

import qualified System.Process as P

import qualified Ed.Memory      as EM
import           Ed.Types

main1 :: IO ()
main1 = do
  let p = (P.shell "ed") { P.std_in = P.CreatePipe, P.std_out = P.CreatePipe }

  b <- P.withCreateProcess p $ \(Just stdin) (Just stdout) _ ph -> do

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    let edProc = EdProc stdin stdout ph

    EM.edCmd edProc "H"

    b <- checkSequential $ Group "ed (real deal)"
       [ ("property_a_p", EM.prop_ed_blackbox_memory edProc)
       ]

    unless b $ T.putStrLn "Ed Buffer:"
      *> EM.readEntireBuffer edProc
      >>= T.putStrLn

    EM.edCmd edProc "Q"

    pure b

  if b then exitSuccess else exitFailure

main :: IO ()
main = main1
