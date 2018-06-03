{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.DeepSeq
import Control.Lens
import Control.Lens.Traversal.Update
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics

data City = City
          { _cityName :: !T.Text
          , _street :: V.Vector Street
          } deriving (Show, Eq, Generic)

data Street = Street
            { _address :: !T.Text
            , _people :: V.Vector Person
            } deriving (Show, Eq, Generic)

data Person = Person
            { _name :: !T.Text
            , _accounts :: V.Vector Account
            } deriving (Show, Eq, Generic)

data Account = Account
             { _number :: !Int
             , _kind :: !AccountType
             , _pid :: !Int
             } deriving (Show, Eq, Generic)

data AccountType = Current | Savings deriving (Show, Eq, Generic)

instance NFData City
instance NFData Street
instance NFData Person
instance NFData Account
instance NFData AccountType

makeLenses 'City
makeLenses 'Street
makeLenses 'Person
makeLenses 'Account

newCity :: Int -> Int -> City
newCity v w = City "Paris" (V.map mkStreet $ V.fromList [1..v])
    where mkStreet i = Street (T.pack ("street " ++ show i)) (people i)
          people i = V.fromList $ map (mkPerson i) [1..w]
          mkPerson i j = Person (T.pack (show i ++ "_" ++ show j)) (accts i j)
          accts i j = V.fromList [ Account (i*1234+2*j) Current i
                                 , Account (i*1234+2*j+1) Savings (i+j) ]

updateAccountsLens :: Int -> City -> City
updateAccountsLens n = over (street.traverse.people.traverse.accounts.traverse) (apply up)
    where up = Update (incAccount, matchAccount n)

updateAccounts :: Int -> Update City
updateAccounts n = overIf (street.traverse.people.traverse) (updatePerson n)

updatePerson :: Int -> Update Person
updatePerson n = overIf (accounts.traverse) up
    where up = Update (incAccount, matchAccount n)

updateAccountsSpecialized :: Int -> Update City
updateAccountsSpecialized n =
    let up = Update (incAccount, matchAccount n)
        updatePersonSpec = overIf accounts $ overVector up
    in (overIf street . overVector . overIf people . overVector) updatePersonSpec

overAccounts :: Update Account -> Update City
overAccounts = (overIf street . overVector . overIf people . overVector . overIf accounts . overVector)

updateAccount :: Int -> Update Account
updateAccount n = Update (incAccount, matchAccount n)

matchAccount :: Int -> Account -> Bool
matchAccount n Account{_pid=p} = let !b = (p == n) in b

incAccount :: Account -> Account
incAccount (Account n k p) = Account (n+1) k p

