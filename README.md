# `harg` :nut_and_bolt:

[![Build Status](https://travis-ci.org/alexpeits/harg.svg?branch=master)](https://travis-ci.org/alexpeits/harg)

`harg` is a library for configuring programs by scanning command line arguments, environment
variables and default values. Under the hood, it uses a subset of
[`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative) to expose regular
arguments, switch arguments and subcommands. The library relies heavily on the use of higher kinded
data (HKD) thanks to the [`barbies`](https://hackage.haskell.org/package/barbies) library. Using
[`higgledy`](https://hackage.haskell.org/package/higgledy) also allows to have significantly less
boilerplate code.

The main goal while developing `harg` was to not have to go through the usual pattern of manually
`mappend`ing the results of command line parsing, env vars and defaults.

# Usage

tl;dr: Take a look at the [example](Example.hs).

(WIP)

Here are some different usage scenarios. Let's first enable some language extensions and add some
imports:

``` haskell
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

import           Data.Function         ((&))
import           Data.Functor.Identity (Identity (..))
import           GHC.Generics          (Generic)

import qualified Data.Barbie           as B
import qualified Data.Generic.HKD      as HKD

import           Options.Harg

main :: IO ()
main = putStrLn "this is a literate haskell file"
```

## One flat (non-nested) datatype

The easiest scenario is when the target configuration type is one single record with no levels of
nesting:

``` haskell
data FlatConfig
  = FlatConfig
      { _fcHost :: String
      , _fcPort :: Int
      , _fcDir  :: String
      , _fcLog  :: Bool  -- whether to log or not
      }
  deriving (Show, Generic)
```

(The `Generic` instance is required for section `3` later on)

Let's first create the `Opt`s for each value in `FlatConfig`. `Opt` is the description for each
component of the configuration.

``` haskell
hostOpt :: Opt String
hostOpt
  = toOpt ( option strParser
          & optLong "host"
          & optShort 'h'
          & optMetavar "HOST"
          & optHelp "The database host"
          )

portOpt :: Opt Int
portOpt
  = toOpt ( option readParser
          & optLong "port"
          & optHelp "The database port"
          & optEnvVar "DB_PORT"
          & optDefault 5432
          )

dirOpt :: Opt String
dirOpt
  = toOpt ( argument strParser
          & optHelp "Some directory"
          & optDefault "/home/user/something"
          )

logOpt :: Opt Bool
logOpt
  = toOpt ( switch
          & optLong "log"
          & optHelp "Whether to log or not"
          )
```

Here, we use `option` to define a command line argument that expects a value after it, `argument` to
define a standalone argument, not prefixed by a long or short indicator, and `switch` to define a
boolean command line flag that, if present, sets the target value to `True`. The `opt*` functions
(here applied using `&` to make things look more declarative) modify the option configuration.
`optHelp` adds help text, `optDefault` adds a default value, `optShort` adds a short command line
option as an alternative to the long one (the string after `option` or `switch`), `optEnvVar` sets
the associated environment variable and `optMetavar` sets the metavariable to be shown in the help
text generated by `optparse-applicative`.

`toOpt` turns any kind of option into the internal `Opt` type. The reason for doing this is that
different types of options can have different capabilities, e.g. `long` and `short` cannot be set for
an `argument`. Another shorthand is to use the `with` variants. For example, `hostOpt` could also be
defined like this:

``` haskell
hostOpt' :: Opt String
hostOpt'
  = optionWith strParser
      ( optLong "host"
      . optShort 'h'
      . optMetavar "HOST"
      . optHelp "The database host"
      )
```

The first argument (`strParser` or `readParser`) is the parser for the argument, be it from the
command line or from an environment variable. The type of this function should be
`String -> Either String a`, which produces an error message or the parsed value. `strParser` is
equivalent to `pure` and always succeeds. `readParser` requires the type to have a `Read` constraint.
In order to use it with newtypes that wrap a type that has a `Read` constraint, using the `Functor`
instance for `Opt` should be sufficient. E.g. for the newtype:

``` haskell
newtype Port = Port Int
```

we can define the following option:

``` haskell
portOpt' :: Opt Port
portOpt'
  = Port <$> portOpt
```

Of course, any user-defined function works as well. In addition, to use a function of type `String
-> Maybe a` use `parseWith`, which runs the parser and in case of failure uses a default error
message. For example, `readParser` is defined as `parseWith readMaybe`.

There are 3 ways to configure this datatype.

### 1. Using a `barbie` type

`barbie` types are types of kind `(Type -> Type) -> Type`. The `barbie` type for `FlatConfig`
looks like this:

``` haskell
data FlatConfigB f
  = FlatConfigB
      { _fcHostB :: f String
      , _fcPortB :: f Int
      , _fcDirB  :: f String
      , _fcLogB  :: f Bool
      }
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)
```

I also derived some required instances that come from the `barbies` package. These instances allow
us to change the `f` (`bmap` from `FunctorB`) and traverse all types in the record producing side
effects (`btraverse` from `TraversableB`).

Now let's define the value of this datatype, which holds our option configuration. The type
constructor needed for the options is `Opt`:

``` haskell
flatConfigOpt1 :: FlatConfigB Opt
flatConfigOpt1
  = FlatConfigB hostOpt portOpt dirOpt logOpt
```

Because `hostOpt`, `portOpt` and `logOpt` all have type `Opt <actual type>`, `flatConfigOpt1` has
the correct type according to `FlatConfigB Opt`.

Now to actually run things:

``` haskell
getFlatConfig1 :: IO ()
getFlatConfig1 = do
  FlatConfigB host port dir log <- execOpt flatConfigOpt1
  print $ runIdentity (FlatConfig <$> host <*> port <*> dir <*> log)
```

`execOpt` returns an `Identity x` where `x` is the type of the options we are configuring, in this
case `FlatConfigB`. Here, we pattern match on the barbie-type, and then use the `Applicative`
instance of `Identity` to get back an `Identity FlatConfig`.

This is still a bit boilerplate-y. Let's look at another way.

### 2. Using an open product

Looking at `FlatConfigB`, it's only used because of it's `barbie`-like capabilities. Other than that
it's just a simple product type with the additional `f` before all its sub-types.

The most common form of an open product is an `HList` (heterogeneous list). An `HList` is like an
arbitrary length tuple. For example, `HList '[Int, Bool, String]` is exactly the same as `(Int,
Bool, String)`. `harg` defines a slightly different version of `HList`, reminiscent of servant's
`:<|>` type, in `Options.Harg.Het.Prod`, called `:*` (the `*` stands for product). This type stores
barbie-like types and also keeps the `f` handy: `data (a :* b) f = a f :* b f`. This is also easily
made an instance of `Generic`, `FunctorB` and `TraversableB`. With all that, let's rewrite the
options value and the function to get the configuration:

``` haskell
flatConfigOpt2 :: (Single String :* Single Int :* Single String :* Single Bool) Opt
flatConfigOpt2
  = single hostOpt :* single portOpt :* single dirOpt :* single logOpt

getFlatConfig2 :: IO ()
getFlatConfig2 = do
  host :* port :* dir :* log <- execOpt flatConfigOpt2
  print $ runIdentity
    (FlatConfig <$> getSingle host <*> getSingle port <*> getSingle dir <*> getSingle log)
```

This looks aufully similar to the previous version, but without having to write another datatype and
derive all the instances. `:*` is both a type-level constructor and a value-level function that acts
like list's `:`. It is also right-associative, so for example `a :* b :* c` is the same as
`a :* (b :* c)`.

The `Single` type constructor is used when talking about a single value, rather than a nested
datatype. `Single a f` is a simple newtype over `f a`. The reason for using that is simply to switch
the order of application, so that we can later apply the `f` (here `Opt`) to the compound type
(`:*`). In addition, `single` is used to wrap an `f a` into a `Single a f`, and `getSingle` is used
to unwrap it. Later on we'll see how to construct nested configurations using `Nested`.

However, the real value when having flat datatypes comes from the ability to use `higgledy`.

### 3. Using `HKD` from `higgledy`

``` haskell
flatConfigOpt3 :: HKD.HKD FlatConfig Opt
flatConfigOpt3
  = HKD.build @FlatConfig hostOpt portOpt dirOpt logOpt

getFlatConfig3 :: IO ()
getFlatConfig3 = do
  result <- execOpt flatConfigOpt3
  print $ runIdentity (HKD.construct result)
```

This is the most straightforward way to work with flat configuration types. The `build` function
takes as arguments the options (`Opt a` where `a` is each type in `FlatConfig`) **in the order they
appear in the datatype**, and returns the generic representation of a type that's exactly the same
as `FlatConfigB`. This means that we get all the `barbie` instances for free.

To go back from the `HKD` representation of a datatype to the base one, we use `construct`.
`construct` uses the applicative instance of the `f` which wraps each type in `FlatConfig` to give
back an `f FlatConfig` (in our case an `Identity FlatConfig`).

# Roadmap

- Better errors using `optparse-applicative`'s internals
- ~~Be able to provide and get back the same type for multiple subcommands~~
- Integrate config files (e.g. JSON using `aeson`)

# Credits

- [jcpetruzza](https://github.com/jcpetruzza)
- [i-am-tom](https://github.com/i-am-tom)
- [jmackie](https://github.com/jmackie)
