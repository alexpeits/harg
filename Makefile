.PHONY: ghcid

ghcid:
	ghcid -a --command="stack ghci -- src/**/*.hs Example.hs"

dist:
	cabal new-sdist

haddock:
	cabal new-haddock --haddock-options="--show-all --hyperlinked-source"

haddock-hackage:
	cabal new-haddock --haddock-options="--show-all --hyperlinked-source" --haddock-for-hackage
