{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (view,makeClassy, (.~), (^.), to)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.IORef as R
import           Data.Function ((&))
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType = Coffee | HotChocolate | Tea deriving (Bounded, Enum, Show)

data Model (v :: * -> *) = Model
  { _modelDrinkType :: DrinkType
  , _modelHasMug    :: Bool
  }
$(makeClassy ''Model)

data AddMug (v :: * -> *) = AddMug deriving Show

instance HTraversable AddMug where
  htraverse _ _ = pure AddMug

newtype SetDrinkType (v :: * -> *) = SetDrinkType DrinkType deriving Show

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

cSetDrinkType
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cSetDrinkType ref = Command gen exec
  [ Update $ \m (SetDrinkType d) _ -> m & modelDrinkType .~ d
  , Ensure $ \_ (Model d _) _ drink -> case (d, drink) of
      (Coffee, C.Coffee{}) -> success
      (HotChocolate, C.HotChocolate) -> success
      (Tea, C.Tea{}) -> success
      _ -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkType Symbolic))
    gen _ = Just $ SetDrinkType <$> Gen.enumBounded

    exec :: SetDrinkType Concrete -> m C.Drink
    exec (SetDrinkType d) = evalIO $ do
      R.modifyIORef ref $ case d of
        Coffee -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea -> C.tea
      view C.drinkSetting <$> R.readIORef ref

genAddMug :: MonadGen g => Model Symbolic -> Maybe (g (AddMug Symbolic))
genAddMug = const (Just $ pure AddMug)

cAddMugHappy
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cAddMugHappy ref = Command genAddMug exec
  [ Require $ \m _ -> m ^. modelHasMug . to not
  , Update $ \m _ _ -> m & modelHasMug .~ True
  ]
  where
    exec :: AddMug Concrete -> m ()
    exec _ = do
      ms <- evalIO $ R.readIORef ref
      evalEither (C.addMug ms) >>= evalIO . R.writeIORef ref

cAddMugSad
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => R.IORef C.MachineState
  -> Command g m Model
cAddMugSad ref = Command genAddMug exec
  [ Require $ \m _ -> m ^. modelHasMug
  , Ensure $ \ _ _ _ res -> either (=== C.MugInTheWay) (const failure) res
  ]
  where
    exec :: AddMug Concrete -> m (Either C.MachineError C.MachineState)
    exec _ = evalIO $ C.addMug <$> R.readIORef ref

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  r <- evalIO $ R.newIORef C.initialState

  let initialModel = Model HotChocolate False
      commands = ($ r) <$>
        [ cSetDrinkType
        , cAddMugHappy
        , cAddMugSad
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  evalIO $ R.writeIORef r C.initialState
  executeSequential initialModel actions
