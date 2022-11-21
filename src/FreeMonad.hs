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



data BankAlgebraF next
  = PerformTransaction AccountId Amount next
  | TopUpCard Amount next
  | CheckBalance (Amount -> next)
  deriving Functor


type BankAlgebra = Free BankAlgebraF



-- Primitives

performTransaction :: AccountId -> Amount -> BankAlgebra ()
performTransaction a b = liftF (PerformTransaction a b ())

topUpCard :: Amount -> BankAlgebra ()
topUpCard a = liftF (TopUpCard a ())

checkBalance :: BankAlgebra Amount
checkBalance = liftF (CheckBalance id)


performTransactionSafe :: AccountId -> Amount -> BankAlgebra Bool
performTransactionSafe account amount =
  do funds <- checkBalance
     if funds >= amount
        then performTransaction account amount *> pure True
        else pure False


topUpCardSafe :: Amount -> BankAlgebra Bool
topUpCardSafe amount =
  do funds <- checkBalance
     if funds >= amount
        then topUpCard amount *> pure True
        else pure False



-- Interpreter

interpreter :: BankAlgebraF a -> State (Map AccountId Amount) a
interpreter x =
    case x of

      PerformTransaction account amount next ->
        do _ <- modify (adjust (+| amount) account .
                       adjust (-| amount) currentAcc)
           pure next

      TopUpCard amount next ->
        do _ <- modify (adjust (-| amount) currentAcc)
           pure next


      CheckBalance next ->
        do funds <- fmap (fromMaybe (Amount 0) . Map.lookup currentAcc) get
           pure (next funds)

   where
     currentAcc = AccountId "current_account"


