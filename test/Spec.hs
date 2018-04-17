import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
-- import Utils
-- import Test.QuickCheck

trueTest :: Assertion
trueTest = True @=? True

main :: IO ()
main = defaultMainWithOpts
       [testCase "trueTest" trueTest]
       mempty
