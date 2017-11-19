.PHONY: bench

bench:
	stack test --ghc-options -O2
