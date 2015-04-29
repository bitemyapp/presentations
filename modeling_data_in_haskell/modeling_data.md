% Modeling data in Haskell
% Chris Allen
% April 30, 2015


# Haskell has a nice type system

- Lets try to make proper use of it

- So no more of this:

```haskell
whoKnows :: String -> Map String String -> IO ()
```

You can't reason about it.


# Haskell datatype syntax

Nullary constructor:

```haskell
data Trivial = Trivial
--   [1]       [2]
```

1. Type constructor

2. Data constructor - takes no arguments, thus "nullary"


# Constructors?

We use type constructors to refer to types by name in type signatures.

```haskell
f :: String -> String -> String
f = ...doesn't matta...
```

Here we refer to the type `String` three times in our type signature, denoted syntactically by the double colon `::`

We use data constructors to create values. There's some special syntax for built-in types like String, List, tuples, but in most cases you'll use a data constructor explicitly introduced by the datatype.


# So which is which?

```haskell
data TrivialTypeConstructor
  = TrivialDataConstructor
```

Rule of thumb: before the `=` is the type constructor, after the `=` are the data constructors.

If there's a single data constructor, it'll often have the same name as the type constructor because types and values are strictly separated in Haskell.


# How do we use our datatype with a single nullary data constructor?

```haskell
theProofIs :: Trivial
theProofIs = Trivial

trivialityBegets :: Trivial -> Trivial
trivialityBegets Trivial = Trivial

-- alternately
trivialityBegets _ = Trivial

-- or
trivialityBegets x = Trivial
```


# Haskell datatype syntax

Unary constructor (takes one argument):

```haskell
data Identity a = Identity a
--      [1]           [2]
```

1. Type constructor, takes one argument.

2. Data constructor, takes one argument. Thus, "unary". Unary/nullary refers to the data constructor. You'll see examples with multiple data constructors of mixed arity later.


# How do we use Identity?

```haskell
unpack :: Identity a -> a
unpack (Identity a) = a

embed :: a -> Identity a
embed a = Identity a

imap :: (a -> b) -> Identity a -> Identity b
imap f (Identity a) = Identity (f a)
```

Identity doesn't do much, so if this seems pointless, you're not missing anything.


# Type constructor has to agree with data constructor

Why can't we have:

```haskell
data Identity = Identity a
```

Because you'll get the error: `Not in scope: type variable ‘a’`

Without the argument existing for the data *and* the type constructor, we have no means of expressing what we think `Identity` contains. There ways to "hide" the type variables in data constructors from the type constructors but that's for another day.


# Product

What happens if you add another argument to a unary data constructor? Products!

```haskell
data Person = Person String Int
```

Product here can also be read to mean, "record" or "struct", but be careful with assumptions about representation. Here `Person` is a product of a `String` and an `Int`.


# Person with record syntax

```haskell
data Person = Person { name = String
                     , age = Int }
```

`Person` defined using record syntax for the fields.


# Sum type


# Making the "algebra" in algebraic data types do work

- There's an actual set of operations here.

```haskell
```

# Don't do this

```haskell
data CarType = Null |
               Car { carid :: Int
                   , position :: Float
                   , speed :: Float,
                   , carLength :: Float
                   , state :: [Float]
                   } deriving (Show,Eq)
```
