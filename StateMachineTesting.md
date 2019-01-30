
Introduction to the types and our mode of operation.

# 1 Types & Commands, a guid`ed` tour

Provide `memed` bindings for `/bin/ed`.

a) Implement simple model for two commands (`a`,`d`), no addressing.
b) Add addressing for the above commands, add current line to model.

# 2 Simple Commands

Model type provided: `Coffee Machine`

Define the state machine for the coffee machine operations:

- 1. add mug
- 2. remove mug
- 3-N. configure beverage
- S N. add coin
- S(S N), press dispense button

```
data Drink
  = HotChocolate
  | Tea Int Int
  | Coffee Int Int
```

Array of implementations provided, all are buggy. The goal is to use state
machine testing to identify bugs in the applications, or gaps in the model.

- Don't look at the implementations
- Use the state machine testing to identify the bugs
- Will require positive and negative examples

# 3 Stateful Pizza Service

- Positive (should confirm desired functionality) & negative (should confirm
  failure expectations) commands

Structure:
Single cabal project
- Exercises/levels in subfolders.
