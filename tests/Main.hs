{-# LANGUAGE CPP, FlexibleInstances, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}

import Control.Monad.Extra (ifM)
import Data.GenericCache (tests)
import System.Exit
import Test.HUnit
-- import Test.QuickCheck

main :: IO ()
main =
  ifM (ok <$> runTestTT tests) exitSuccess exitFailure
  where ok (Counts{..}) = errors == 0 && failures == 0
