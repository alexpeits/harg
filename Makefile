.PHONY: test

NIV_VERSION?=nixos-stable
HOOGLE_PORT?=8888
HADDOCK_CMD=cabal new-haddock --haddock-options='--show-all --hyperlinked-source'

cabal-configure:
	nix-shell --argstr pkgs ${NIV_VERSION} --arg withHoogle false --run 'cabal new-configure --enable-tests -w $$(which ghc)'

ghcid:
	ghcid -a --command='cabal new-repl' --restart=harg.cabal

ghcid-stack:
	ghcid -a --command='stack ghci -- src/**/*.hs Example.hs'

dist:
	cabal new-sdist

build:
	cabal new-build

test:
	cabal new-test

haddock:
	${HADDOCK_CMD}

haddock-hackage:
	${HADDOCK_CMD} --haddock-for-hackage

hoogle:
	hoogle server --port ${HOOGLE_PORT} --local

list-ghcs:
	nix-instantiate --eval -E "with import (import ./nix/sources.nix).${NIV_VERSION} {}; lib.attrNames haskell.compiler"

