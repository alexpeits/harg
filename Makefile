.PHONY: test

NIV_VERSION?=nixos-stable
HOOGLE_PORT?=8888
HADDOCK_CMD=cabal new-haddock --haddock-options="--show-all --hyperlinked-source"
NIX_SHELL_CMD=nix-shell --argstr pkgs ${NIV_VERSION} --arg withHoogle false

configure:
	${NIX_SHELL_CMD} --run 'cabal new-configure -w $$(which ghc)'

cabal-update:
	${NIX_SHELL_CMD} --run 'cabal update'

build:
	${NIX_SHELL_CMD} --run 'cabal new-build'

test:
	${NIX_SHELL_CMD} --run 'cabal new-test'

haddock:
	${NIX_SHELL_CMD} --run '${HADDOCK_CMD}'

ghcid:
	${NIX_SHELL_CMD} --run 'ghcid -a --command="cabal new-repl" --restart=harg.cabal'

ghcid-stack:
	ghcid -a --command='stack ghci -- src/**/*.hs Example.hs'

dist:
	${NIX_SHELL_CMD} --run 'cabal new-sdist'

haddock-hackage:
	${NIX_SHELL_CMD} --run '${HADDOCK_CMD} --haddock-for-hackage'

hoogle:
	nix-shell --argstr pkgs ${NIV_VERSION} --run 'hoogle server --port ${HOOGLE_PORT} --local'

list-ghcs:
	nix-instantiate --eval -E "with import (import ./nix/sources.nix).${NIV_VERSION} {}; lib.attrNames haskell.compiler"

