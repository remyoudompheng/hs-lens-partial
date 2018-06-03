import Control.DeepSeq
import Control.Lens.Traversal.Update (apply)
import Types
import Weigh

updateLensAll :: City -> City
updateLensAll c = foldr updateAccountsLens c [10..50]

updatePartialAll :: City -> City
updatePartialAll c = foldr (apply . updateAccounts) c [10..50]

updateSpecializedAll :: City -> City
updateSpecializedAll c = foldr (apply . updateAccountsSpecialized) c [10..50]

updateSpecializedMonoid :: City -> City
updateSpecializedMonoid c = apply (overAccounts go) c
    where go = mconcat $ map updateAccount [10..50]

city :: City
city = force $ newCity 500 10

main :: IO ()
main = mainWith $ do
    func "update naive lens" updateLensAll city
    func "update traverse" updatePartialAll city
    func "update special" updateSpecializedAll city
    func "update fused" updateSpecializedMonoid city

