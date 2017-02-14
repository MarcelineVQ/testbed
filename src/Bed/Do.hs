{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Bed.Do where

import           Yesod.Form.Jquery
import           Control.Applicative ((<$>), (<*>))
import           Data.Text
import           Yesod


main :: IO ()
main = do
  putStrLn "hello world"
