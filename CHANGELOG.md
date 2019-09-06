# Changelog for harg

## 0.2.0.0 [2019.09.06]

- Trigger a parser failure when any option in the sources fails to parse
  NOTE: this introduces a breaking change, in that some parsers that failed silently
        and selected the default (if applicable) will now fail.

## 0.1.3.0 [2019.08.28]

- Add `manyParser` to parse list of options separated by delimiter

## 0.1.2.0 [2019.08.19]

- Add `optRequired` to mark option as required

## 0.1.1.0 [2019.08.16]

- Add `optDefaultStr` to provide defaults as unparsed strings
- Bump dependencies (`barbies` and `higgledy`)

## 0.1.0.1 [2019.07.19]

- Minor documentation changes

## 0.1.0.0 [2019.07.18]

- Initial release
