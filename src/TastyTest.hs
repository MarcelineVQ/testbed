module Main where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

{- deps:
, tasty
, tasty-hunit
-}

-- Example of using cli options to affect resource acquisition for tests
-- Based off example at: https://ro-che.info/articles/2013-12-29-tasty-resources-2.html

data Foo = Foo Int

aquire :: Int -> IO Foo
release :: Foo -> IO ()
testWithFoo :: Foo -> Assertion

-- basic nonsense to test with
aquire x = return $ Foo x
release _ = return ()
testWithFoo (Foo x) = x @?= (-1)
-- above should fail and report supplied thread count, or default 1, from -j<n> cli option
-- not 'real thread' count though, it's simply what was supplied to Tasty

-- getNumThreads tells askOption that v is a NumThreads (found in Tasty.Runners)
nResourceTest :: (IO Foo -> TestTree) -> TestTree
nResourceTest t = askOption (\n -> withResource (aquire (getNumThreads n)) release t)


tests :: IO Foo -> TestTree
tests resource = testGroup "Test Tests"
  [ testGroup "test1" [testCase "1" (2 @?= 2)]
  , testGroup "test2" [testCase "2" $ resource >>= testWithFoo ]
  ]

main = defaultMain $ nResourceTest tests

-- If you don't want the IO requirement or the resource at the top level of the
-- test you can declare it for a single test-group instead

tests' :: TestTree
tests' = testGroup "Test Tests"
  [ testGroup "test1" [testCase "1" (2 @?= 2)]
  , testGroup "test2" [nResourceTest (\res -> testCase "2" $ res >>= testWithFoo) ]
  ]

main' = defaultMain tests'




--
