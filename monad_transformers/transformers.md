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

Since we have kind inference in Haskell (unlike say, Scala), Haskell is able to infer that `f` needs to be kind `* -> *` based on the fact that it gets applied to a single type argument `a`.


# Functor and its kind

To prove my point, let me demonstrate something that won't work.

```haskell
module Blah where

instance Functor Either where
  fmap = undefined
```


# Functor and its kind

```
Prelude> :l bad_code.hs
Expecting one more argument to ‘Either’
The first argument of ‘Functor’ should have
   kind ‘* -> *’,
   but ‘Either’ has kind ‘* -> * -> *’
In the instance declaration for ‘Functor Either’
```

(slightly edited to accommodate slide limitations)


# Functor and its kind

It doesn't matter that I stubbed out the implementation of `fmap` with `undefined` (useful trick btw), Either *cannot* implement Functor. But the kind is `* -> * -> *`.

If Either is a type constructor that behaves like a function at the type level, we can we do with functions to get from `* -> * -> *` to `* -> *` ?


# Fixing Either so we can get a Functor

We *apply* it! Talking a `Functor` for `Either` doesn't make any sense, but it does for `(Either a)`!


# Fixing Either so we can get a Functor

This changes our code to the following.

```haskell
instance Functor (Either a) where
  fmap = undefined
```

Now it'll type-check.


# Type variable scope

Ordinarily the `Functor` for `Either` maps over the contents of the `Right` data constructor. What happens
if we write a typeclass that maps over the `Left`?


# Either that maps over the Left

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Blah where

class Functor f where
  fmap :: (a -> b) -> f a -> f b

data Either a b = Left a | Right b

instance Functor (Either a) where
  fmap f (Left a)  = Left (f a)
  fmap _ (Right b) = Right b
```


# Either that maps over the Left

If you attempt the previous slide's code, you'll get the following type error (heavily abridged):

```
Couldn't match expected type ‘a’ with actual type ‘b’
```

What does this mean? It means it expected `a` based on our terms but the types based on definition mean it needs to be `b`.


# Why did it expect b?

```haskell
data Either a b = Left a | Right b

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor (Either a) where

Either a == f

a in f a == b in Either a b
```


# How to make a left-biased Either work

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Blah where

class Functor f where
  fmap :: (a -> b) -> f a -> f b

data Either b a = Left a | Right b

instance Functor (Either a) where
  fmap f (Left a)  = Left (f a)
  fmap _ (Right b) = Right b
```

# Function functor

`a -> b` is kind `* -> * -> *` because it has two type variables `a` and `b`. The first type variable is the argument type.

Thus, a `Functor` instance for `a -> b` must be changing the result type, rather than the argument type because that'll be the unbound type variable when `(->)` is partially applied. This gives us the following Functor instance:

```haskell
instance Functor ((->) a) where
  fmap = (.)
```

It's just function composition.


# Weaker and stronger algebras

- Functor is weaker than Applicative, Applicative is weaker than Monad.

- Weakness of an algebra for Haskell means there are more types that have a valid instance.

- But a weaker algebra means fewer derived operations...fewer things you can do with it.


# Monad

Typeclass definition

```haskell
class Monad f where
   bind :: (a -> m b) -> m a -> m b
```

Not a semicolon.

# Examples of monads

List monad

```haskell
-- since m here is []
bind :: (a -> [b]) -> [a] -> [b]
```

Maybe monad

```haskell
-- since m is Maybe
(a -> Maybe b) -> Maybe a -> Maybe b
```

# Examples of monads

List monad

```haskell
instance Monad [] where
  bind f xs = foldr ((++) . f) [] xs
```

Maybe monad

```haskell
-- maybe :: b -> (a -> b) -> Maybe a -> b

instance Monad Maybe where
  bind f a = maybe Nothing f a
```

# Examples of monads

Reader (function)

```haskell
-- applying the argument type again
instance Monad ((->) t) where
  bind f g = \x -> f (g x) x
```

# Nesting data types

What if I glue two functors, [] and Maybe together?

```haskell
value :: [Maybe a]
```

and I want to map a function f :: a -> b

```haskell
result :: [Maybe b]
result f = fmap (fmap f) value
```

# Mapping on a (f of g)

You can do this for any two functors:

```haskell
value :: f (g a)
```

to map a function f :: a -> b

```haskell
result :: f (g b)
result f = fmap (fmap f) value
```

# Compose

If f and g are functors, then (f of g) is a functor:

```haskell

data Compose f g x = Compose (f (g x))

instance (Functor f, Functor g) =>
  Functor (Compose f g) where
  fmap f (Compose z) = Compose (fmap (fmap f) z)
```

# Compose

The composition of two arbitrary functors makes a new functor. Put briefly, *functors compose*.

# Two monads

if I glue two monads, [] and Maybe together:

```haskell
value :: [Maybe a]
```

and I want to bind a function f :: a -> [Maybe b]:

```haskell
result :: [Maybe b]
result f = bind (maybe (return Nothing) f) value
```

# Two monads

We called bind on a list

```haskell
result f = bind (maybe (return Nothing) f) value
           ^     ^
```

but we destructured the Maybe using Maybe-specific calls

# Can we compose monads?

If f and g are monads, then is (f of g) a monad?

Can we generalize?

```haskell
instance (Monad f, Monad g) =>
  Monad (Compose f g) where
  bind = error "???"
```

# Nope

It's impossible, but I would invite you to try anyway as a learning exercise.

# If we know one monad

We *can* bind on (f on Maybe) for any monad f.

```haskell
result :: Monad f =>
          (a -> f (Maybe b))
          -> f (Maybe a)
          -> f (Maybe b)
result = bind (maybe (return Nothing) f) value
```

# The Maybe monad transformer

```haskell
data MaybeT f a = MaybeT {
  maybeT :: f (Maybe a)
}

instance Monad f => Monad (MaybeT f) where
  bind f (MaybeT x) =
    MaybeT (bind
              (maybe (return Nothing)
                (maybeT . f)) x
                )
```
