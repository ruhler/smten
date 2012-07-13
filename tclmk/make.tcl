
# Explicitly initialize the environment.
foreach key [array names ::env] {
    array unset ::env $key
}

# Get infomation about the local environment.
# local.tcl should set
#   ::HAPPY - path to the happy executable
#   ::GHC - path to ghc
#   ::env(...) - needed environment variables, such as:
#       PATH, GHC_PACKAGE_PATH
#   ::PACKAGE_DB - package-db to use for cabal 
source tclmk/local.tcl
set ::env(LANG) "en_US.UTF-8"

proc indir {dir script} {
    set wd [pwd]
    puts "tclmk: Entering directory `$wd/$dir'"
    cd $dir
    eval $script
    puts "tclmk: Leaving directory `$wd/$dir'"
    cd $wd
}


proc run {args} {
    puts $args
    exec {*}$args "2>@" stderr
}

# same as run, but output stdout of the command here.
proc hrun {args} {
    puts $args
    exec {*}$args "2>@" stderr ">@" stdout
}

# Create and set up a build directory for the build.
hrun find src -type d -exec mkdir -p build/{} {;}
hrun find src -type f -exec ln -sf [pwd]/{} build/{} {;}

# The cabal package
source tclmk/haskell.tcl
source tclmk/cabal.tcl
cabal build/src/seri.cabal
hrun mkdir -p build/home
set ::env(HOME) [pwd]/build/home
indir build/src {
    hrun cabal configure --package-db $::PACKAGE_DB \
        --extra-lib-dirs $::env(LD_LIBRARY_PATH) \
        --with-happy=$::HAPPY
    hrun cabal build
    hrun cabal haddock
    hrun cabal sdist
}
    
set SERIT build/src/dist/build/serit/serit
set SERIE build/src/dist/build/serie/serie
set SERIH build/src/dist/build/serih/serih
set SERIQ build/src/dist/build/seriq/seriq
set SERIQ2 build/src/dist/build/seriq2/seriq2

# The general seri test
hrun $SERIT -o build/src/tests.typed -i build/src build/src/Seri/Lib/Tests.sri
hrun $SERIE -o build/src/tests.got -i build/src -m testall \
    build/src/Seri/Lib/Tests.sri
run echo -n "(True :: Bool)" > build/src/tests.wnt
hrun cmp build/src/tests.got build/src/tests.wnt

# Poorly typed tests.
proc badtypetest {name} {
    set btdir "build/src/Seri/Lambda/Tests"
    set cmd {hrun $::SERIT -o $btdir/$name.typed -i build/src $btdir/$name.sri}
    if { [catch $cmd] == 0 } {
        error "expected type error, but $name passed type check"
    }
}

badtypetest "BadType1"
badtypetest "BadType2"
badtypetest "InstCtx"
badtypetest "LetRecM"
badtypetest "LetRecS"


# Test the haskell target.
set hsdir build/src/Seri/Target/Haskell
hrun $SERIH -o $hsdir/hstests.hs -i build/src -m testall \
    build/src/Seri/Lib/Tests.sri
hrun -ignorestderr $GHC -o $hsdir/hstests -ibuild/src $hsdir/hstests.hs
run ./$hsdir/hstests > $hsdir/hstests.got
run echo "True" > $hsdir/hstests.wnt
hrun cmp $hsdir/hstests.got $hsdir/hstests.wnt

# The SMT query tests
proc querytest {name args} {
    run $::SERIQ -d build/src/Seri/SMT/Tests/$name.dbg -i build/src \
         build/src/Seri/SMT/Tests/$name.sri {*}$args \
         > build/src/Seri/SMT/Tests/$name.out
}

querytest "Query1"
querytest "Query2"
querytest "Complex"
querytest "If"
querytest "Casenomatch"
querytest "Bluespec"
querytest "Array"

# The SMT query2 tests
proc query2test {name args} {
    run $::SERIQ2 -d build/src/Seri/SMT/Tests/$name.2.dbg -i build/src \
         build/src/Seri/SMT/Tests/$name.sri {*}$args \
         > build/src/Seri/SMT/Tests/$name.2.out
}

query2test "Query1"
query2test "Query2"
query2test "Complex"
query2test "If"
query2test "Casenomatch"
query2test "Bluespec"
query2test "Array"

puts "BUILD COMPLETE"

