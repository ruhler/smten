
all: sm sm_prof hs_prof hs

sm: wc.smtn Map.smtn State.smtn
	smten --haskellf --hsdir build --include . -f wc.smtn
	ghc -o sm -O0 -ibuild build/Smten/Lib/Main.hs -main-is Smten.Lib.Main.main__

sm_prof: wc.smtn Map.smtn State.smtn
	smten --haskellf --hsdir build --include . -f wc.smtn
	ghc -o sm_prof -O1 -ibuild build/Smten/Lib/Main.hs -main-is Smten.Lib.Main.main__ -rtsopts -prof

hs_prof: wc.hs Map.hs State.hs
	ghc -o $@ -O0 --make $< -rtsopts -prof -fprof-auto-top

hs: wc.hs Map.hs State.hs
	ghc -o $@ -O0 --make $<


