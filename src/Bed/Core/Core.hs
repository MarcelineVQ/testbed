{-# LANGUAGE ViewPatterns #-}

module Bed.Core.Core where

-- Testbed for Core operations
-- potential tools:
-- mouseover core output
-- diffing core modulo uniquified-names

import GHC
import CoreSyn
import DynFlags
import HscTypes

import GHC.Paths (libdir)

data Boo = Board | BoardExpansion | Video | VideoExpansion



boardgame x y = undefined
videogame x y = undefined

feh ty key = case ty of
        Board -> boardgame ty key
        BoardExpansion -> boardgame ty key
        Video -> videogame ty key
        VideoExpansion -> videogame ty key



foo :: Num a => a
foo = 2

main =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        target <- guessTarget "src/Bed/Core/test_main.hs" Nothing
        setTargets [target]
        load LoadAllTargets



-- end
