import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
-- import Utils
-- import Test.QuickCheck
import qualified ThreeManChess.Engine.StartGameBoardTest

main :: IO ()
main = defaultMainWithOpts
       (
         ThreeManChess.Engine.StartGameBoardTest.tests
       )
       mempty
