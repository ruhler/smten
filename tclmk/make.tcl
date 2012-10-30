
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
    --include src/sri \
    -f src/sri/Seri/Lib/Tests.sri \
    > build/src/tests.typed
run $SERI --io \
    --include src/sri \
    -m Seri.Lib.Tests.testallio \
    -f src/sri/Seri/Lib/Tests.sri \
    > build/src/tests.got 
run echo "PASSED" > build/src/tests.wnt
hrun cmp build/src/tests.got build/src/tests.wnt

# Poorly typed tests.
proc badtypetest {name} {
    set cmd {
        run $::SERI --type \
            --include src/sri \
            -f src/Seri/Lambda/Tests/$name.sri \
            > "build/src/$name.typed"
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
    --include src/sri \
    -m testallio \
    -f src/sri/Seri/Lib/Tests.sri \
    > $hsdir/hstests.hs
hrun -ignorestderr $GHC -o $hsdir/hstests -ibuild/src $hsdir/hstests.hs
run ./$hsdir/hstests > $hsdir/hstests.got
run echo "PASSED" > $hsdir/hstests.wnt
hrun cmp $hsdir/hstests.got $hsdir/hstests.wnt

# The SMT query tests
proc smttest {name} {
    run $::SERI --io \
         --include src/sri \
         -m Seri.SMT.Tests.[string map {/ .} $name].main \
         -f src/sri/Seri/SMT/Tests/$name.sri \
         > build/src/Seri/SMT/Tests/$name.out
}

smttest "Query1"
smttest "Query2"
smttest "Complex"
smttest "If"
smttest "Bluespec"
smttest "Array"
smttest "Share"
smttest "Tuple"
smttest "Bit"
smttest "AllQ"
smttest "AllQ2"
smttest "Squares2/Squares"
smttest "BCL3Small"
smttest "Sudoku"
smttest "Sudoku2"
smttest "Sudoku3"

# The IO tests
proc iotest {name args} {
    run $::SERI --io \
         --include src/sri \
         -m Seri.IO.Tests.[string map {/ .} $name].main \
         -f src/sri/Seri/IO/Tests/$name.sri {*}$args \
         > build/src/Seri/IO/Tests/$name.out
}

iotest "Simple"
iotest "Query"

# The enoch tests
hrun $::ENOCH
hrun $::SUDOKU

puts "BUILD COMPLETE"

