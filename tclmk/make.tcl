
# Explicitly initialize the environment.
foreach key [array names ::env] {
    array unset ::env $key
}

# Get infomation about the local environment.
# local.tcl should set
#   ::HAPPY - path to the happy executable
#   ::GHC - path to ghc
#   ::env(...) - needed environment variables, such as: ATH
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
set ::VERSION 0.1.1.1
source tclmk/haskell.tcl
source tclmk/cabal.tcl
cabal build/src/seri.cabal
hrun mkdir -p build/home
set ::env(HOME) [pwd]/build/home
indir build/src {
    hrun cabal update

    # Add the flag --enable-executable-profiling to this command to enable
    # profiling.
    hrun cabal install \
        --extra-lib-dirs $::env(LD_LIBRARY_PATH) \
        --with-happy=$::HAPPY

    hrun cabal haddock
    hrun cabal sdist
}

indir build {
    proc ghcexe {name path} {
        hrun ghc -c src/$path/$name.hs
        hrun ghc -o src/$name src/$path/$name.hs \
            -L$::env(LD_LIBRARY_PATH) -lHSseri-$::VERSION -lyices1
    }

    ghcexe seri Seri
    ghcexe enoch Seri/Enoch
    ghcexe sudoku Seri/Enoch
}
    
set SERI build/src/seri
set ENOCH build/src/enoch
set SUDOKU build/src/sudoku

# The general seri test
run $SERI --type \
    --include build/src \
    -f build/src/Seri/Lib/Tests.sri \
    > build/src/tests.typed
run $SERI --io \
    --include build/src \
    -m Seri.Lib.Tests.testallio \
    -f build/src/Seri/Lib/Tests.sri \
    > build/src/tests.got 
run echo "PASSED" > build/src/tests.wnt
hrun cmp build/src/tests.got build/src/tests.wnt

# Poorly typed tests.
proc badtypetest {name} {
    set btdir "build/src/Seri/Lambda/Tests"
    set cmd {
        run $::SERI --type \
            --include build/src \
            -f $btdir/$name.sri \
            > $btdir/$name.typed
        }

    if { [catch $cmd] == 0 } {
        error "expected type error, but $name passed type check"
    }
}

badtypetest "BadType1"
badtypetest "BadType2"
badtypetest "Ctx"
badtypetest "InstCtx"


# Test the haskell target.
set hsdir build/src/Seri/Haskell
run $SERI --haskell \
    --include build/src \
    -m testallio \
    -f build/src/Seri/Lib/Tests.sri \
    > $hsdir/hstests.hs
hrun -ignorestderr $GHC -o $hsdir/hstests -ibuild/src $hsdir/hstests.hs
run ./$hsdir/hstests > $hsdir/hstests.got
run echo "PASSED" > $hsdir/hstests.wnt
hrun cmp $hsdir/hstests.got $hsdir/hstests.wnt

# The SMT query1 tests
proc querytest {solver name} {
    run $::SERI --query \
         -s $solver \
         --include=build/src \
         -m Seri.SMT.Tests.[string map {/ .} $name].main \
         -f build/src/Seri/SMT/Tests/$name.sri \
         -d build/src/Seri/SMT/Tests/$name.$solver.dbg \
         > build/src/Seri/SMT/Tests/$name.$solver.out
}

proc yices1test {name} { querytest Yices1 $name }
proc yices2test {name} { querytest Yices2 $name }

yices1test "Query1"
yices1test "Query2"
yices1test "Complex"
yices1test "If"
yices1test "Bluespec"
yices1test "Array"
yices1test "Share"
yices1test "Tuple"
yices1test "Bit"
yices1test "AllQ"

yices2test "Query1"
yices2test "Query2"
yices2test "Complex"
yices2test "If"
yices2test "Bluespec"
yices2test "Array"
yices2test "Share"
yices2test "Tuple"
yices2test "Bit"
yices2test "AllQ"
yices2test "AllQ2"
yices2test "Squares2/Squares"
yices2test "BCL3Small"
yices2test "Sudoku"
yices2test "Sudoku2"
yices2test "Sudoku3"

# The IO tests
proc iotest {name args} {
    run $::SERI --io \
         --include=build/src \
         -m Seri.IO.Tests.[string map {/ .} $name].main \
         -f build/src/Seri/IO/Tests/$name.sri {*}$args \
         > build/src/Seri/IO/Tests/$name.out
}

iotest "Simple"
iotest "Query"

# The enoch tests
hrun $::ENOCH
hrun $::SUDOKU

puts "BUILD COMPLETE"

