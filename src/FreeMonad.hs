{-# LANGUAGE DeriveFunctor #-}

module FreeMonad where

import Data.Maybe
import Control.Monad.Free
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Data.Map


newtype Amount = Amount Double deriving (Eq, Ord, Show)
newtype AccountId = AccountId String deriving (Eq, Ord, Show)


(-|) :: Amount -> Amount -> Amount
(-|) (Amount am1) (Amount am2) = Amount (am1 - am2)

(+|) :: Amount -> Amount -> Amount
(+|) (Amount am1) (Amount am2) = Amount (am1 + am2)



data BankAlgebra next
  = PerformTransaction AccountId Amount next
  | TopUpCard Amount next
  | CheckBalance (Amount -> next)
  deriving Functor


type BankProgram = Free BankAlgebra



-- Primitives

performTransaction :: AccountId -> Amount -> BankProgram ()
performTransaction a b = liftF (PerformTransaction a b ())

topUpCard :: Amount -> BankProgram ()
topUpCard a = liftF (TopUpCard a ())

checkBalance :: BankProgram Amount
checkBalance = liftF (CheckBalance id)


performTransactionSafe :: AccountId -> Amount -> BankProgram Bool
performTransactionSafe account amount =
  do funds <- checkBalance
     if funds >= amount
        then performTransaction account amount *> pure True
        else pure False


topUpCardSafe :: Amount -> BankProgram Bool
topUpCardSafe amount =
  do funds <- checkBalance
     if funds >= amount
        then topUpCard amount *> pure True
        else pure False



-- Interpreter

interpreter :: BankAlgebra a -> State (Map AccountId Amount) a
interpreter x = case x of

      PerformTransaction account amount next ->
        do _ <- modify (Map.adjust (+| amount) account .
                       Map.adjust (-| amount) currentAcc)
           pure next

      TopUpCard amount next ->
        do _ <- modify (Map.adjust (-| amount) currentAcc)
           pure next


      CheckBalance next ->
        do funds <- fmap (fromMaybe (Amount 0) . Map.lookup currentAcc) get
           pure (next funds)

   where
     currentAcc = AccountId "current_account"
