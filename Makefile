.PHONY: ghcid

NIV_VERSION?=nixos-stable

cabal-configure:
	nix-shell --argstr pkgs ${NIV_VERSION} --command 'cabal new-configure -w $$(which ghc)'

ghcid:
	ghcid -a --command='cabal new-repl' --restart=harg.cabal

ghcid-stack:
	ghcid -a --command='stack ghci -- src/**/*.hs Example.hs'

dist:
	cabal new-sdist

haddock:
	cabal new-haddock --haddock-options="--show-all --hyperlinked-source"

haddock-hackage:
	cabal new-haddock --haddock-options="--show-all --hyperlinked-source" --haddock-for-hackage

hoogle:
	hoogle server --port 8888 --local

list-ghcs:
	nix-instantiate --eval -E "with import (import ./nix/sources.nix).${NIV_VERSION} {}; lib.attrNames haskell.compiler"

