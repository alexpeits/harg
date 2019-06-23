# origin

Configuration library that allows using command line variables, environment variables and defaults,
with minimal boilerplate and the ability to configure without re-declaring datatypes. Also supports
subparsers (using `optparse-applicative`).

Readme coming soon, in the meantime take a look at `Example.hs` for a usage example (simple parser
and subparser with 2 different targets). Run `stack exec example -- --help` to see options, and
`stack exec example -- <option> --help` to see options for specific subcommands.

## TODO

- Print errors using `optparse-applicative`'s internals
- Be able to use the same type for many tagged subcommands
