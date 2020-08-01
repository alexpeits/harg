.PHONY: test

NIV_VERSION?=nixos-stable
HOOGLE_PORT?=8888
HADDOCK_CMD=cabal new-haddock --haddock-options="--show-all --hyperlinked-source"
NIX_SHELL_CMD=nix-shell --argstr pkgs ${NIV_VERSION} --arg withHoogle false

configure:
	${NIX_SHELL_CMD} --run 'cabal new-configure -w $$(which ghc)'

dist:
	cabal new-sdist

haddock:
	${HADDOCK_CMD}

haddock-hackage:
	${HADDOCK_CMD} --haddock-for-hackage

ghcid:
	ghcid -a --command="cabal new-repl" --restart=harg.cabal

ghcid-stack:
	ghcid -a --command='stack ghci -- src/**/*.hs Example.hs'

hoogle:
	nix-shell --argstr pkgs ${NIV_VERSION} --run 'hoogle server --port ${HOOGLE_PORT} --local'

list-ghcs:
	nix-instantiate --eval -E "with import (import ./nix/sources.nix).${NIV_VERSION} {}; lib.attrNames haskell.compiler"

