# harg :nut_and_bolt:

[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Falexpeits%2Fharg%2Fbadge&style=flat)](https://actions-badge.atrox.dev/alexpeits/harg/goto)
[![Hackage](https://img.shields.io/hackage/v/harg.svg)](https://hackage.haskell.org/package/harg)

`harg` is a library for configuring programs by scanning command line arguments,
environment variables, default values and more. Under the hood, it uses a subset
of [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative)
to expose regular arguments, switch arguments and subcommands. The library
relies heavily on the use of higher kinded data (HKD) thanks to the
[`barbies`](https://hackage.haskell.org/package/barbies) library. Using
[`higgledy`](https://hackage.haskell.org/package/higgledy) also helps reduce
boilerplate code significantly.

## Documentation

To find out more, check out the [docs](https://alexpeits.github.io/harg) or the
package page on [hackage](https://hackage.haskell.org/package/harg). There is
also an [example](https://github.com/alexpeits/harg/blob/master/Example.hs)
module that serves as an extensive demonstration of the library.

## Roadmap

- Better errors using `optparse-applicative`'s internals
- Allow user to pass `optparse-applicative` preferences
- Write tests

## Credits

- [jcpetruzza](https://github.com/jcpetruzza)
- [i-am-tom](https://github.com/i-am-tom)
- [jmackie](https://github.com/jmackie)
