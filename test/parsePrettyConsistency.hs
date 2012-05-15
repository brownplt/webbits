-- | This test-suite checks that 'parse src == (parse . pretty
-- . parse) src' for test cases from the test262 test-suite
-- (http://test262.ecmascript.org)

module Main where

import Test.Framework
import Test.Framework.Runners.Console
import Test.Framework.Providers.HUnit

main = defaultMain tests

tests = []