module Main (main) where

import FreeMonad
import Data.Map
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Free


main :: IO ()
main = print result


result :: ([String], Map AccountId Amount)
result = foldFree interpreter program `runState`
           Map.fromList [(AccountId "current_account", Amount 1800),
                         (AccountId "GB67BARC20032647753595", Amount 0)]


program :: BankAlgebra [String]
program = do
  success <- performTransactionSafe (AccountId "GB67BARC20032647753595")
                                   amountDue
  funds <- checkBalance
  success' <- if success && funds >= (amountDue +| amountTopUp)
             then topUpCardSafe amountTopUp
             else pure False
  pure [if success
        then "Completed transaction"
        else "Failed transaction. Need more funds",
        if success'
        then "Completed card topup"
        else "Failed card topup. Need more funds"]

  where
    amountDue   = Amount 1000
    amountTopUp = Amount 500
