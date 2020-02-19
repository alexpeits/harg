<!-- -*- tab-width: 2; -*- -->
# harg

`harg` is a library for configuring programs by scanning command line arguments,
environment variables, default values and more. Under the hood, it uses a subset
of [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative)
to expose regular arguments, switch arguments and subcommands. The library
relies heavily on the use of higher kinded data (HKD) thanks to the
[`barbies`](https://hackage.haskell.org/package/barbies) library. Using
[`higgledy`](https://hackage.haskell.org/package/higgledy) also helps reduce
boilerplate code significantly.

The main goal while developing `harg` was to not have to go through the usual
pattern of manually `mappend`ing the results of command line parsing, env vars
and defaults.

This file is also a literate haskell file which is checked when tests are run,
so all examples should compile and work, and be up to date with the latest
version of the library.

# Usage

Here are some different usage scenarios. Let's first enable some language
extensions and add some imports:

``` haskell
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

import           Data.Functor.Identity (Identity (..))
import           Data.Kind             (Type)
import           GHC.Generics          (Generic)

import qualified Data.Barbie           as B
import           Data.Aeson            (FromJSON)
import           Data.Generic.HKD      (HKD, build, construct)

import           Options.Harg

main :: IO ()
main = putStrLn "this is a literate haskell file"
```

## One flat (non-nested) datatype

The easiest scenario is when the target configuration type is one single record
with no levels of nesting:

``` haskell
data FlatConfig
  = FlatConfig
      { _fcDbHost :: String
      , _fcDbPort :: Int
      , _fcDir    :: String
      , _fcLog    :: Bool  -- whether to log or not
      }
  deriving (Show, Generic)
```

(The `Generic` instance is required for section `3` later on)

Let's first create the `Opt`s for each value in `FlatConfig`. `Opt` is the
description for each component of the configuration.

``` haskell
dbHostOpt :: Opt String
dbHostOpt
  = option strParser
      ( long "host"
      . short 'h'
      . metavar "DB_HOST"
      . help "The database host"
      )

dbPortOpt :: Opt Int
dbPortOpt
  = option readParser
      ( long "port"
      . help "The database port"
      . envVar "DB_PORT"
      . defaultVal 5432
      )

dirOpt :: Opt String
dirOpt
  = argument strParser
      ( help "Some directory"
      . defaultVal "/home/user/something"
      )

logOpt :: Opt Bool
logOpt
  = switch
      ( long "log"
      . help "Whether to log or not"
      )
```

Here, we use `option` to define a command line argument that expects a value
after it, `argument` to define a standalone argument, not prefixed by a long or
short indicator, and `switch` to define a boolean command line flag that, if
present, sets the target value to `True`. The `opt*` functions (here applied
using `&` to make things look more declarative) modify the option configuration.
`help` adds help text, `defaultVal` adds a default value, `short` adds a short
command line option as an alternative to the long one (the string after `option`
or `switch`), `envVar` sets the associated environment variable and `metavar`
sets the metavariable to be shown in the help text generated by
`optparse-applicative`.

The first argument (`strParser` or `readParser`) is the parser for the argument,
be it from the command line or from an environment variable. The type of this
function should be `String -> Either String a`, which produces an error message
or the parsed value. `strParser` is equivalent to `pure` and always succeeds.
`readParser` requires the type to have a `Read` constraint. In order to use it
with newtypes that wrap a type that has a `Read` constraint, using the `Functor`
instance for `Opt` should be sufficient. E.g. for the newtype:

``` haskell
newtype Port = Port Int
```

we can define the following option:

``` haskell
dbPortOpt' :: Opt Port
dbPortOpt'
  = Port <$> dbPortOpt
```

Of course, any user-defined function works as well. In addition, to use a
function of type `String -> Maybe a` use `parseWith`, which runs the parser and
in case of failure uses a default error message. For example, `readParser` is
defined as `parseWith readMaybe`.

Finally, an optional option with type `a` can be specified by setting its type
to `Maybe a`. The declaration is exactly the same as it would be for `a`, and
adding `optional` to the modifiers turns turns the parser from `String -> Either
String a` to `String -> Either String (Maybe a)` but without using the `Read`
instance for `Maybe`:

``` hs
someOpt :: Opt (Maybe Int)
someOpt
  = option readParser
      ( long "something"
      . optional
      )
```

Note that `optional` can't be used with `defaultVal`. Using them together raises
a type error at compile time, to ensure there's no ambiguous behaviour (e.g. the
order of declaration of modifiers should not influence the resulting option).

There are 3 ways to configure this datatype.

### 1. Using a `barbie` type

`barbie` types are types of kind `(Type -> Type) -> Type`. The `barbie` type for
`FlatConfig` looks like this:

``` haskell
data FlatConfigB f
  = FlatConfigB
      { _fcDbHostB :: f String
      , _fcDbPortB :: f Int
      , _fcDirB    :: f String
      , _fcLogB    :: f Bool
      }
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)
```

I also derived some required instances that come from the `barbies` package.
These instances allow us to change the `f` (`bmap` from `FunctorB`) and traverse
all types in the record producing side effects (`btraverse` from
`TraversableB`).

Now let's define the value of this datatype, which holds our option
configuration. The type constructor needed for the options is `Opt`:

``` haskell
flatConfigOpt1 :: FlatConfigB Opt
flatConfigOpt1
  = FlatConfigB dbHostOpt dbPortOpt dirOpt logOpt
```

Because `dbHostOpt`, `dbPortOpt` and `logOpt` all have type `Opt <actual type>`,
`flatConfigOpt1` has the correct type according to `FlatConfigB Opt`.

Now to actually run things:

``` haskell
getFlatConfig1 :: IO ()
getFlatConfig1 = do
  FlatConfigB host port dir log <- execOptDef flatConfigOpt1
  print $ runIdentity $
    FlatConfig
    <$> host
    <*> port
    <*> dir
    <*> log
```

`execOpt` returns an `Identity x` where `x` is the type of the options we are
configuring, in this case `FlatConfigB`. Here, we pattern match on the
barbie-type, and then use the `Applicative` instance of `Identity` to get back
an `Identity FlatConfig`.

This is still a bit boilerplate-y. Let's look at another way.

### 2. Using a product type

Looking at `FlatConfigB`, it's only used because of it's `barbie`-like
capabilities. Other than that it's just a simple product type with the
additional `f` before all its sub-types.

`harg` defines a type almost similar to `Product` (from `Data.Functor.Product`),
which works in a similar fashion as servant's `:<|>` type. This type is defined
in `Options.Harg.Het.Prod` and is called `:*` (the `*` stands for product). This
type stores barbie-like types and also keeps the `f` handy: `data (a :* b) f = a
f :* b f`. This is also easily made an instance of `Generic`, `FunctorB`,
`TraversableB` and `ProductB`. With all that, let's rewrite the options value
and the function to get the configuration:

``` haskell
flatConfigOpt2
  :: (Single String :* Single Int :* Single String :* Single Bool) Opt
flatConfigOpt2
  = single dbHostOpt :* single dbPortOpt :* single dirOpt :* single logOpt

getFlatConfig2 :: IO ()
getFlatConfig2 = do
  host :* port :* dir :* log <- execOptDef flatConfigOpt2
  print $ runIdentity $
    FlatConfig
    <$> getSingle host
    <*> getSingle port
    <*> getSingle dir
    <*> getSingle log
```

This looks aufully similar to the previous version, but without having to write
another datatype and derive all the instances. `:*` is both a type-level
constructor and a value-level function that acts like list's `:`. It is also
right-associative, so for example `a :* b :* c` is the same as `a :* (b :* c)`.

The `Single` type constructor is used when talking about a single value, rather
than a nested datatype. `Single a f` is a simple newtype over `f a`. The reason
for using that is simply to switch the order of application, so that we can
later apply the `f` (here `Opt`) to the compound type (`:*`). This makes type
definitions look more similar to datatype definitions:

``` haskell
type FlatConfigOpt2
  =  Single String
  :* Single Int
  :* Single Bool
```

In addition, `single` is used to wrap an `f a` into a `Single a f`, and
`getSingle` is used to unwrap it. Later on we'll see how to construct nested
configurations using `Nested`.

However, the real value when having flat datatypes comes from the ability to use
`higgledy`.

### 3. Using `HKD` from `higgledy`

``` haskell
flatConfigOpt3 :: HKD FlatConfig Opt
flatConfigOpt3
  = build @FlatConfig dbHostOpt dbPortOpt dirOpt logOpt

getFlatConfig3 :: IO ()
getFlatConfig3 = do
  result <- execOptDef flatConfigOpt3
  print $ runIdentity (construct result)
```

This is the most straightforward way to work with flat configuration types. The
`build` function takes as arguments the options (`Opt a` where `a` is each type
in `FlatConfig`) **in the order they appear in the datatype**, and returns the
generic representation of a type that's exactly the same as `FlatConfigB`. This
means that we get all the `barbie` instances for free.

To go back from the `HKD` representation of a datatype to the base one, we use
`construct`. `construct` uses the applicative instance of the `f` which wraps
each type in `FlatConfig` to give back an `f FlatConfig` (in our case an
`Identity FlatConfig`).

## Nested datatypes

Let's say now that we have these two datatypes:

``` haskell
data DbConfig
  = DbConfig
      { _dcHost :: String
      , _dcPort :: Int
      }
  deriving (Show, Generic)

data ServiceConfig
  = ServiceConfig
      { _scPort :: Int
      , _scLog  :: Bool
      }
  deriving (Show, Generic)
```

And the datatype to be configured is this:

``` haskell
data Config
  = Config
      { _cDb      :: DbConfig
      , _cService :: ServiceConfig
      , _cDir     :: String
      }
  deriving (Show, Generic)
```

And a new option required for the service port:

``` haskell
portOpt :: Opt Int
portOpt
  = option readParser
      ( long "port"
      . help "The service port"
      . defaultVal 8080
      )
```

Again, there are several ways to configure these options.

### 1. Using `barbie` types

Since we now have 3 types, there's a bit more boilerplate to write:

``` haskell
data ConfigB f
  = ConfigB
      { _cDbB      :: DbConfigB f
      , _cServiceB :: ServiceConfigB f
      , _cDirB     :: f String
      }
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

data DbConfigB f
  = DbConfigB
      { _dcHostB :: f String
      , _dcPortB :: f Int
      }
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

data ServiceConfigB f
  = ServiceConfigB
      { _scPortB :: f Int
      , _scLogB  :: f Bool
      }
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)
```

To define the option parser, we need option parsers for every type inside it.
This was true for flat configs too, but we have to manually construct a
`DbConfigB Opt` and `ServiceConfigB Opt`:

``` haskell
configOpt1 :: ConfigB Opt
configOpt1
  = ConfigB dbOpt serviceOpt dirOpt

dbOpt :: DbConfigB Opt
dbOpt
  = DbConfigB dbHostOpt dbPortOpt

serviceOpt :: ServiceConfigB Opt
serviceOpt
  = ServiceConfigB portOpt logOpt
```

And to run the parser:

``` haskell
getConfig1 :: IO ()
getConfig1 = do
  ConfigB (DbConfigB dbHost dbPort) (ServiceConfigB port log) dir <-
    execOptDef configOpt1
  let
    db      = DbConfig <$> dbHost <*> dbPort
    service = ServiceConfig <$> port <*> log
  print $ runIdentity $
    Config
    <$> db
    <*> service
    <*> dir
```

### 2. Using `higgledy`

`higgledy` puts an `f` before every type, so doing something like `HKD Config f`
doesn't make sense: looking at `ConfigB` it seems like the `f` needs to go to
the right hand side of the nested types. We can, however, avoid the boilerplate
of defining `barbie` types for the nested datatypes:

``` haskell
data ConfigH f
  = ConfigH
      { _cDbH      :: HKD DbConfig f
      , _cServiceH :: HKD ServiceConfig f
      , _cDirH     :: f String
      }
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

configOpt2 :: ConfigH Opt
configOpt2
  = ConfigH dbOptH serviceOptH dirOpt

dbOptH :: HKD DbConfig Opt
dbOptH
  = build @DbConfig dbHostOpt dbPortOpt

serviceOptH :: HKD ServiceConfig Opt
serviceOptH
  = build @ServiceConfig portOpt logOpt
```

And to run the parser:

``` haskell
getConfig2 :: IO ()
getConfig2 = do
  ConfigH db service dir <- execOptDef configOpt2
  print $ runIdentity $
    Config
    <$> construct db
    <*> construct service
    <*> dir
```

### 2. Using products

Recall from previously that there's the `Single` type which in general turns `f
b` into `b f`. This means that, by using `Single` for the directory option, all
`f`s are after their types, so we can just use `:*` instead of having to declare
a new datatype:

``` haskell
type ConfigP
  =  HKD DbConfig
  :* HKD ServiceConfig
  :* Single String

configOpt3 :: ConfigP Opt
configOpt3
  = dbOptH :* serviceOptH :* single dirOpt

getConfig3 :: IO ()
getConfig3 = do
  db :* service :* dir <- execOptDef configOpt3
  print $ runIdentity $
    Config
    <$> construct db
    <*> construct service
    <*> getSingle dir
```

And, to make things look more orthogonal, `harg` defines a type called `Nested`,
which is exactly the same as `HKD`. There are functions that correspond to
`build` and `construct`, too:

```
Nested    <-> HKD
nested    <-> build
getNested <-> construct
```

This means that the previous code block might as well be:

``` haskell
type ConfigP'
  =  Nested DbConfig
  :* Nested ServiceConfig
  :* Single String

configOpt4 :: ConfigP' Opt
configOpt4
  = dbOptN :* serviceOptN :* single dirOpt
  where
    dbOptN
      = nested @DbConfig dbHostOpt dbPortOpt
    serviceOptN
      = nested @ServiceConfig portOpt logOpt

getConfig4 :: IO ()
getConfig4 = do
  db :* service :* dir <- execOptDef configOpt4
  print $ runIdentity $
    Config
    <$> getNested db
    <*> getNested service
    <*> getSingle dir
```

Pretty cool.

## Subcommands

`harg` also supports (somewhat limited) subcommands, again by using `optparse-applicative`
underneath.

Because of limitations with higher kinded data when it comes to sum types,
`harg` uses a different way to define subcommands. `optparse-applicative` allows
defining subcommands that result to the same type, which means the user needs to
define a sum type, and each subcommand results in a different constructor. In
contrast, `harg` defines subcommands that can return completely different types.
Instead of the result being a sum type, where the user has to pattern match on
constructors, the result is a `Variant`, which is defined (almost) like this:

``` haskell
data Variant (xs :: [Type]) where
  Here :: x -> Variant (x ': xs)
  There :: Variant xs -> Variant (y ': xs)
```

`Variant` is like a sum type which holds all the summands in a type-level list.
Instead of pattern matching in `Left` or `Right` like when using `Either`, we
pattern match on `Here x`, `There (Here x)` etc. For a pretty thorough
introduction to `Variant` and more heterogeneous types, check out
[this repo](https://github.com/i-am-tom/learn-me-a-haskell) by
[i-am-tom](https://github.com/i-am-tom).

``` haskell
x :: Variant '[Int, Bool, Char]
x = There (Here True)

run :: Variant '[Int, Bool, Char] -> Maybe Bool
run (Here _)                 = Nothing
run (There (Here b))         = Just b
run (There (There (Here _))) = Nothing

-- > run x
-- Just True
```

`harg` defines another kind of variant called `VariantF`:

``` hs
data VariantF (xs :: [(Type -> Type) -> Type]) (f :: Type -> Type) where
```

to hold a type-level list of `barbie` types and the `f` to wrap every type with.

To define a type to be used in a subcommand parser we need the target type and
the subcommand name, which is encoded as a type-level string `Symbol`. There's a
handy way to define this. Suppose that the the `Config` type above is the
configuration type when the command is `app` and another type, e.g.`TestConfig`
is the configuration when the command is `test`:

``` haskell
data TestConfig
  = TestConfig
      { _tcFoo :: String
      , _tcBar :: Int
      }
  deriving Show

fooOpt :: Opt String
fooOpt
  = option strParser
      ( short 'f'
      . help "Something foo"
      . defaultVal "this is the default foo"
      )

barOpt :: Opt Int
barOpt
  = option readParser
      ( short 'b'
      . help "Something bar"
      . defaultVal 42
      )

type TestConfigP
  = Single String :* Single Int

testConfigOpt :: TestConfigP Opt
testConfigOpt
  = single fooOpt :* single barOpt
```

The subcommand type looks like this:

``` haskell
type SubcommandConfig
  =  "app" :-> ConfigP'
  :+ "test" :-> TestConfigP
```

The `+` here stands for sum. The associated option type is:

``` haskell
subcommandOpt :: SubcommandConfig Opt
subcommandOpt
  = configOpt4 :+ testConfigOpt :+ ANil
```

The `ANil` here marks the end of the association list (which is a heterogeneous
list that associates symbols with types).

Here's how to run this parser:

``` haskell
getSubcommand :: IO ()
getSubcommand = do
  result <- execCommandsDef subcommandOpt
  case result of
    HereF (db :* service :* dir)
      -> print $ runIdentity $
           Config
           <$> getNested db
           <*> getNested service
           <*> getSingle dir
    ThereF (HereF (foo :* bar))
      -> print $ runIdentity $
           TestConfig
           <$> getSingle foo
           <*> getSingle bar
```

Or use `fromVariantF`, which is similar to the `either` function:

``` haskell
getSubcommand' :: IO ()
getSubcommand' = do
  result <- execCommandsDef subcommandOpt
  fromVariantF result
    (\(db :* service :* dir)
       -> print $ runIdentity $
            Config
            <$> getNested db
            <*> getNested service
            <*> getSingle dir
    )
    (\(foo :* bar)
       -> print $ runIdentity $
            TestConfig
            <$> getSingle foo
            <*> getSingle bar
    )
```

The type of `fromVariantF` can be thought of as being:

``` hs
fromVariantF
  :: VariantF '[a, b, c, ...] f
  -> (a f -> r)
  -> (b f -> r)
  -> (c f -> r)
  -> ...
  -> r
```

The signature will accept the appropriate number of functions depending on the
length of the type level list.

## More than just environment variables

You may have noticed the use of `execOptDef` and `execCommandsDef` in all of the
examples up to now. There are actually more configurable versions of these,
called `execOpt` and `execCommands` respectively. With these functions the user
can select where to get options from. For example, `execOptDef` is a shorthand
for `execOpt EnvSource`, which means that options will be fetched from
environment variables only (along with the command line, which is always
required, and defaults, which can be optionally provided by the user).

The sources currently supported are environment variables, json and yaml files.

### Configuring using a json file

First of all, let's use `FlatConfig` from the first example:

``` hs
data FlatConfig
  = FlatConfig
      { _fcDbHost :: String
      , _fcDbPort :: Int
      , _fcDir    :: String
      , _fcLog    :: Bool  -- whether to log or not
      }
  deriving (Show, Generic)

dbHostOpt :: Opt String
dbHostOpt
  = option strParser
      ( long "host"
      . short 'h'
      . metavar "DB_HOST"
      . help "The database host"
      )

dbPortOpt :: Opt Int
dbPortOpt
  = option readParser
      ( long "port"
      . help "The database port"
      . envVar "DB_PORT"
      . defaultVal 5432
      )

dirOpt :: Opt String
dirOpt
  = argument strParser
      ( help "Some directory"
      . defaultVal "/home/user/something"
      )

logOpt :: Opt Bool
logOpt
  = switch
      ( long "log"
      . help "Whether to log or not"
      )

flatConfigOpt3 :: HKD FlatConfig Opt
flatConfigOpt3
  = build @FlatConfig dbHostOpt dbPortOpt dirOpt logOpt
```

To use the JSON source, a `FromJSON` instance is required. Thankfully that's
easy, since `FlatConfig` has `Generic` instance:

``` haskell
instance FromJSON FlatConfig
```

In `harg`, sources are defined as products (using `:*`) of options, which means
that the definition of the sources is not very different than defining options!
If we only needed the environment variable source, the options would be:

``` hs
envSource :: EnvSource Opt
envSource = EnvSource
```

There's no need to actually define an option for the environment because there's
no meaningful configuration for this. To use the `EnvSource` along with a json
config, we use the following option:

``` haskell
sourceOpt :: (EnvSource :* JSONSource) Opt
sourceOpt
  = EnvSource :* JSONSource jsonOpt
  where
    jsonOpt :: Opt ConfigFile
    jsonOpt
      = option strParser
          ( long "json"
          . short 'j'
          . help "JSON config filepath"
          )
```

Here, the type of the option for the JSON source is `ConfigFile`. This type is a
wrapper around `FilePath`, which looks like this:

``` hs
data ConfigFile
  = ConfigFile FilePath
  | NoConfigFile
```

This has the advantage that, if the user wants to specify an optional
configuration file, they can simply say:

``` haskell
jsonOpt :: Opt ConfigFile
jsonOpt
  = option strParser
      ( long "json"
      . defaultVal NoConfigFile
      )
```

Also, because `ConfigFile` has an `IsString` instance, there's no need to say
`long (ConfigFile "json")` (if `OverloadedStrings` is enabled).

There's a bit of a disconnect between `ConfigFile` and the ability to make
optional options using `Maybe` and `optional`. The reason for it is that the
type that `JSONSource` wraps is not polymorphic, since it needs to be a filepath
specifically.