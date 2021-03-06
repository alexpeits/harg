# Changelog for harg

## 0.5.0.0 [2021.03.02]

Library updates:

- Add support for ghc >= 8.8 by updating `barbies` and `higgledy`. This means
  that **support is dropped** for ghc <= 8.2 (because of `barbies`)
- Expose `HasX` classes from `Construct.hs`
- Because of changes in `barbies`, `ApplicativeB` is exposed from `Options.Harg`
  instead of `ProductB`

CI & development updates:

- Update nix sources for using cached ghc v8.8.4 and dependencies
- Add stack configuration for ghc v8.8.4 (lts-16.31 resolver)
- Update github actions workflow to build both v8.8.4 and v8.6.5 with cabal and
  stack

## 0.4.2.1 [2020.08.01]

Only cosmetic and functional changes, no library changes:

- Minor CI tweaks
- Format everything with ormolu

## 0.4.2.0 [2020.01.27]

- Add explicit export and import lists
- Some documentation fixes

## 0.4.1.0 [2019.12.22]

- Parsers now stop immediately if a source error is encountered
- Updated the JSON source to return a bytestring instead of an `aeson` `Value`
- Fix broken deriving for `barbie` typeclasses by re-exporting `Rec`

## 0.4.0.0 [2019.09.16]

- Fix wrong name in previous release (`HasDefaultValStr` -> `HasDefaultStr`)
- Expose `fromSingle` and `fromNested` for when f ~ Identity
- Expose classes `Build` and `Construct` from `higgledy`

## 0.3.0.0 [2019.09.16]

- Remove `*With` variants of option constructors and make the `*With` variant
  behaviour the default (meaning now options are constructed using function
  composition and not `toOpt`)
- Remove `opt` prefix from modifiers. Because `default` is a reserved keyword,
  this is now named `defaultVal` (to mirror `defaultStr`)

  NOTE: the above introduce breaking changes

## 0.2.0.0 [2019.09.06]

- Trigger a parser failure when any option in the sources fails to parse

  NOTE: this introduces a breaking change, in that some parsers that failed
        silently and selected the default (if applicable) will now fail.

## 0.1.3.0 [2019.08.28]

- Add `manyParser` to parse list of options separated by delimiter

## 0.1.2.0 [2019.08.19]

- Add `optRequired` (renamed to `required` for 0.3.0.0) to mark option as
  required

## 0.1.1.0 [2019.08.16]

- Add `optDefaultStr` (renamed to `defaultStr` for 0.3.0.0) to provide defaults
  as unparsed strings
- Bump dependencies (`barbies` and `higgledy`)

## 0.1.0.1 [2019.07.19]

- Minor documentation changes

## 0.1.0.0 [2019.07.18]

- Initial release
