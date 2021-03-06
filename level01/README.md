# Level 01 - Simple Commands

The first thing we're going to test is drink selection. The machine
can be switched between coffee, hot chocolate and tea, and we're going
to define commands for each operation. We'll then build the commands
up into a state machine test and walk through how it's wired into a
larger test suite.

The first thing we'll need is a model of the coffee machine. The model
for a test does not have to replicate the entire system perfectly; we
start small, capturing the things we understand as we build our
commands and observe their interactions. Open up
`level01/CoffeeMachineTests.hs`, where you'll find the following model
defined for you:

```haskell
data DrinkType = Coffee | HotChocolate | Tea
newtype Model (v :: Type -> Type) = Model DrinkType
```

Note that `Model` has a mysterious type parameter `v`, and to give the
_kind signature_ `Type -> Type` to `v` we've had turn on `{-# LANGUAGE
KindSignatures #-}` as GHC will not infer the correct kind
otherwise. You can get away with cargo-culting this for the moment,
but if you're the sort of person who needs to dig through the details,
check out the asides below:

<details>
  <summary>Kind Signatures? Wha?</summary>

  A _kind_ is the "type" of a type. The kind `Type` is the kind of
  types that can have values. Consider `Maybe` - its type argument
  must be a type that can have values, and it can have values once
  it's fully applied (like in `Maybe Int`), so its kind is `Type ->
  Type`. (You might have seen this written as `* -> *` in the past;
  `*` is now an alias for `Type`.)

  This means that our `Model` type takes one type argument of kind
  `Type -> Type`; something `Functor`-shaped.
</details>

<details>
  <summary>What's the type parameter used for? I need to know!</summary>

  Hedgehog generates complete command sequences before it runs any
  commands, and not every command can be run at any time. Example: if
  your web service has commands that need administrator powers, you
  need to register an admin before you try running those commands.

  This means that generators need to see the current model state. At
  the same time, you can't know what the exact values in the state
  are, because the test hasn't run yet!

  Hedgehog works around this with two types:
  [`Symbolic`](https://hackage.haskell.org/package/hedgehog/docs/Hedgehog.html#t:Symbolic)
  and
  [`Concrete`](https://hackage.haskell.org/package/hedgehog/docs/Hedgehog.html#t:Concrete)
  . In the generation phase of the test, your model is a `Model
  Symbolic`, but once it starts executing it becomes a `Model
  Concrete` and you can pull out real values from the model.
</details>

*****

Now that we have our model, we need to build some commands to act on
it. The core type to understand is
[`Command`](https://hackage.haskell.org/package/hedgehog/docs/Hedgehog.html#t:Command),
reproduced here with some type variables renamed:

```haskell
data Command g m (state :: (Type -> Type) -> Type) =
  forall input output.
  (HTraversable input, Show (input Symbolic), Typeable output) =>
  Command {
    -- | A generator which provides random arguments for a command. If the
    --   command cannot be executed in the current state, it should return
    --   'Nothing'.
    --
      commandGen ::
        state Symbolic -> Maybe (g (input Symbolic))

    -- | Executes a command using the arguments generated by 'commandGen'.
    --
    , commandExecute ::
        input Concrete -> m output

    -- | A set of callbacks which provide optional command configuration such
    --   as pre-condtions, post-conditions and state updates.
    --
    , commandCallbacks ::
        [Callback input output state]
    }
```

Here are some important things to understand about `Command`:

* When we pass our list of commands to `Hedgehog.Gen.sequential` (to
  generate the random sequences), we'll need `(MonadGen g, MonadTest
  m)`.

* We'll often need `MonadIO m` as well, so we can interact with the
  system being tested.

* `commandGen` can decline to return an `input` by returning
  `Nothing`. This can be faster than generating inputs and pruning
  them back with a `Require` callback.

* The kind of `state` matches the kind of our `Model` type from
  before.

* The `input` and `output` type variables are existential, so each
  `Command` can choose ones that make sense.

* The `input` type must be `HTraversable`, which we'll talk about
  soon.

* The list of callbacks is how we:

  - Assert preconditions on actions, which guarantees that shrunken
    command sequences stay valid,

  - Update our model as actions are performed, and

  - Assert postconditions on actions, checking that our system
    actually behaves correctly.

## Your first command

We're going to walk through a simple command to select tea on the
machine. After that, you'll need to set up similar commands to select
coffee and hot chocolate. You'll retain more if you type all this out
by hand, and use typed holes and a running `ghcid` session so you can
watch the types coming together.

Our goal is to write a function of this type:

```haskell
cSetDrinkTea
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  -> C.Machine
  -> Command g m Model
```

`C.Machine` is our coffee machine type from the library, a `newtype`
wrapper around an `IORef`. Later, we will pass the same `C.Machine` to
all our `Command`-returning functions so the tests all operate on the
same thing. We're writing the `forall` explicitly so we can use `{-#
LANGUAGE ScopedTypeVariables #-}` and write `g` and `m` in type
signatures for helper functions.

To write `cSetDrinkTea`, we'll need to pick a type for the input, so
let's define one:

```haskell
data SetDrinkTea (v :: Type -> Type) = SetDrinkTea deriving Show
```

We know from the type of `Command` that we'll need a
[`HTraversable`](https://hackage.haskell.org/package/hedgehog/docs/Hedgehog.html#t:HTraversable)
instance, so let's look at the class definition now:

```haskell
class HTraversable t where
  htraverse :: Applicative f => (forall a. g a -> f (h a)) -> t g -> f (t h)
```

Hedgehog uses this class with `(g ~ Symbolic, h ~ Concrete)` to
convert `Symbolic` values to `Concrete` values inside inputs, so you
have the real values when you run the command sequence against the
real system. Because our type doesn't have any `Var`s in it, we can
ignore the function, and because we don't have any data in our input
type we can ignore that argument too:

```haskell
instance HTraversable SetDrinkTea where htraverse _ _ = pure SetDrinkTea
```

Now, let's write the bulk of the `cSetDrinkTea` function:

```haskell
cSetDrinkTea
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkTea mach = Command gen exec []
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkTea Symbolic))
    gen _ = Just $ pure SetDrinkTea

    exec :: SetDrinkTea Concrete -> m C.Drink
    exec _ = evalIO $ do
      C.tea mach
      view C.drinkSetting <$> C.peek mach
```

Some things to notice:

* Our generation function can always run. Some generators won't always
  be runnable, and will return `Nothing`.

* The `C.Drink` in our `exec` function fixes our `output` type; this
  type will show up when we add callbacks.

* Our list of callbacks is empty. This is wrong, and we will fix it
  now.

## Adding Callbacks

We now need to look at the
[`Callback`](https://hackage.haskell.org/package/hedgehog/docs/Hedgehog.html#t:Callback)
type:

```haskell
data Callback input output state
  = Require (state Symbolic -> input Symbolic -> Bool)
  | Update (forall v. Ord1 v => state v -> input v -> Var output v -> state v)
  | Ensure (state Concrete -> state Concrete -> input Concrete -> output -> Test ())
```

The list of `Callbacks` in a `Command`

is walk through the `test-suite` component to familiarise ourselves
with the libraries we.

We're using [`tasty`](https://hackage.haskell.org/package/tasty) to
define the test suite, and
[`tasty-hedgehog`](https://hackage.haskell.org/package/tasty-hedgehog)
to connect our tests to the Tasty `TestTree`. We've done this so
you'll know how to integrate hedgehog tests into your existing test
suite.

`level01/Main.hs` is the entry point for the test suite, and will stay
largely the same across levels. It defines a single `TestTree` which
imports the actual `stateMachineTests :: TestTree` from
`level01/CoffeeMachineTests.hs`.

 test is drink selection
