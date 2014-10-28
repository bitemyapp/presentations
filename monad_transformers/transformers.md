% Monad transformers
% Chris Allen
% October 30, 2014

# Functors

- We're going to talk about functors before moving on to monads and monad transformers

- You always have a functor and an applicative functor whenever you have a monad

# What's a functor?

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Plus the laws:

```haskell
fmap id  ==  id
fmap (f . g)  ==  fmap f . fmap g
```

# Why do laws matter?

This law is usually called "identity".

```haskell
fmap id == id
```

And this one is about associativity. You'll recall that arithmetic operations associate.

```haskell
fmap (f . g)  ==  fmap f . fmap g
```

Without these laws, we can assume very little about what fmap does.

# What does this mean for us?

Identity

```haskell
Prelude> let myList = [1, 2, 3]
Prelude> id myList
[1,2,3]
Prelude> fmap id myList
[1,2,3]
```

Associativity

```haskell
Prelude> fmap (*2) . fmap (+1) $ myList
[4,6,8]
Prelude> fmap ((*2) . (+1)) myList
[4,6,8]
```

# What's fmap, really?

Lets shift the parentheses a bit. Since the type constructor for functions `->` is right associative, the following re-parenthesization is correct.

From:

```haskell
(a -> b) -> f a -> f b
```

To:

```haskell
(a -> b) -> (f a -> f b)
```

There are a few ways to describe this. One is that we're lifting `(a -> b)` into the environment `f`. Another is that we're mapping `(a -> b)` over `f`. Mostly this does not matter as long as you don't confuse the map with the territory.

# List functor

List

```haskell
(a -> b) -> [a] -> [b]
```

```haskell
instance Functor [] where
  fmap f = foldr ((:) . f) []
```

We've seen this already

```haskell
Prelude> fmap (+1) [1, 2, 3]
[2,3,4]
Prelude> fmap (+1) []
[]
```

# Maybe functor

Maybe

```haskell
(a -> b) -> Maybe a -> Maybe b
```

```haskell
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap _ Nothing  = Nothing
```

```haskell
Prelude> fmap (+1) (Just 1)
Just 2
Prelude> fmap (+1) Nothing
Nothing
```

# Kinds and types

Kinds are types "one level up". We use them to describe the structure of type constructors.

`*` is the default kind in Haskell and is used for all reference types (pointer to value or thunk).

Int, Float, Char are all kind `*`.

Lists, `[]`, are kind `* -> *` because they need a type before they can become real values. Once that type argument is applied, then it's a real type.

# A mechanical demonstration of kinds

We can Haskell to infer the kind of a type and tell us what it is in the REPL using `:kind`, oft abbreviated to `:k`.

```haskell
Prelude> data Trivial0 = Trivial
Prelude> :k Trivial0
Trivial0 :: *

Prelude> data Trivial1 a = Trivial
Prelude> :k Trivial1
Trivial1 :: * -> *
```

# A mechanical demonstration of kinds

The terms, in this case a nullary constructor named "Trivial", don't matter for the purposes of this demonstration.

```haskell
Prelude> data Trivial2 a b = Trivial
Prelude> :k Trivial2
Trivial2 :: * -> * -> *

Prelude> data Trivial3 a b c = Trivial
Prelude> :k Trivial3
Trivial3 :: * -> * -> * -> *
```

# Kinds of day to day types

```haskell
data [] a = [] | a : [a]
-- with slightly different syntax
data List a = Nil | Cons a (List a)

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b
```

```haskell
Prelude> :kind []
[] :: * -> *
Prelude> :kind Maybe
Maybe :: * -> *
Prelude> :kind Either
Either :: * -> * -> *
```

# Functor and its kind

One thing to note about the definition of functor is that it applies the type `f` to an argument `a` in the typeclass.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Since we have kind inference in Haskell (unlike say, Scala), Haskell is able to infer that `f` needs to be kind `* -> *` based on the fact that it gets applied to a single type argument.

# Function functor

# Weaker and stronger algebras

- Functor is weaker than Applicative, Applicative is weaker than Monad.

- Weakness of an algebra for Haskell means there are more types that have a valid instance.

- But a weaker algebra means fewer derived operations...fewer things you can do with it.

# Compose

```haskell
```
