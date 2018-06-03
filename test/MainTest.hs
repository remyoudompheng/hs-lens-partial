import Control.Lens.Traversal.Update (apply)
import Types
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "main" [testUpdate])

testUpdate :: TestTree
testUpdate = testCase "compare" $ do
    let c = newCity 2 3
        c2 = newCity 500 10
    updateAccountsLens 3 c @=? apply (updateAccounts 3) c
    updateAccountsLens 3 c @=? apply (updateAccountsSpecialized 3) c
    foldr updateAccountsLens c2 [3, 5, 24] @=?
        foldr (apply . updateAccounts) c2 [3, 5, 24]
    foldr updateAccountsLens c2 [3, 5, 24] @=?
        foldr (apply . updateAccountsSpecialized) c2 [3, 5, 24]


