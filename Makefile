.PHONY: ghcid

ghcid:
	ghcid -a --command="stack ghci -- src/**/*.hs Example.hs"
