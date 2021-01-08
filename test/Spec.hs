import qualified Spec.Data.Watson as Data.Watson
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Data" [Data.Watson.tests]
