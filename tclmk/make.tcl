
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
        #--enable-executable-profiling
    hrun cabal build
    #hrun cabal haddock --executables
    hrun cabal sdist
}
    
set SERIT build/src/dist/build/serit/serit
set SERIE build/src/dist/build/serie/serie
set SERIH build/src/dist/build/serih/serih
set SERIO build/src/dist/build/serio/serio
set SERIQ1 build/src/dist/build/seriq1/seriq1
set SERIQ2 build/src/dist/build/seriq2/seriq2
set ENOCH build/src/dist/build/enoch/enoch
set SUDOKU build/src/dist/build/sudoku/sudoku

# The general seri test
hrun $SERIT -o build/src/tests.typed -i build/src build/src/Seri/Lib/Tests.sri
hrun $SERIE -o build/src/tests.got -i build/src -m Seri.Lib.Tests.testall \
    build/src/Seri/Lib/Tests.sri +RTS -K1g
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


# Test the haskell target.
set hsdir build/src/Seri/Haskell
hrun $SERIH -o $hsdir/hstests.hs -i build/src -m testall \
    build/src/Seri/Lib/Tests.sri
hrun -ignorestderr $GHC -o $hsdir/hstests -ibuild/src $hsdir/hstests.hs
run ./$hsdir/hstests > $hsdir/hstests.got
run echo "True" > $hsdir/hstests.wnt
hrun cmp $hsdir/hstests.got $hsdir/hstests.wnt

# The SMT query1 tests
proc query1test {name args} {
    run $::SERIQ1 -d build/src/Seri/SMT/Tests/$name.1.dbg -i build/src \
         -m Seri.SMT.Tests.$name.main \
         build/src/Seri/SMT/Tests/$name.sri {*}$args \
         > build/src/Seri/SMT/Tests/$name.1.out
}

# The SMT query2 tests
proc query2test {name args} {
    run $::SERIQ2 -d build/src/Seri/SMT/Tests/$name.2.dbg -i build/src \
         -m Seri.SMT.Tests.[string map {/ .} $name].main \
         build/src/Seri/SMT/Tests/$name.sri {*}$args \
         > build/src/Seri/SMT/Tests/$name.2.out
}


query1test "Query1"
query1test "Query2"
query1test "Complex"
query1test "If"
query1test "Bluespec"
query1test "Array"
query1test "Share"
query1test "Tuple"
query1test "Bit"
query1test "AllQ"

query2test "Query1"
query2test "Query2"
query2test "Complex"
query2test "If"
query2test "Bluespec"
query2test "Array"
query2test "Share"
query2test "Tuple"
query2test "Bit"
query2test "AllQ"
query2test "AllQ2"
query2test "Squares2/Squares"
query2test "BCL3Small"
query2test "Sudoku"
query2test "Sudoku2"
query2test "Sudoku3"

# The IO tests
proc iotest {name args} {
    run $::SERIO -i build/src \
         -m Seri.IO.Tests.[string map {/ .} $name].main \
         build/src/Seri/IO/Tests/$name.sri {*}$args \
         > build/src/Seri/IO/Tests/$name.out
}

iotest "Simple"

# The enoch tests
hrun $::ENOCH
hrun $::SUDOKU

puts "BUILD COMPLETE"

