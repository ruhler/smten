
# Explicitly initialize the environment.
foreach key [array names ::env] {
    array unset ::env $key
}

# Get infomation about the local environment.
# local.tcl should set
#   ::HAPPY - path to the happy executable
#   ::GHC - path to ghc
#   ::env(...) - needed environment variables, such as: PATH
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
hrun mkdir -p build/home build/seri-bin build/Squares2

set ::env(HOME) [pwd]/build/home
#hrun cabal update

# The seri package
indir seri {
    # Add the flag --enable-executable-profiling to this command to enable
    # profiling.
    hrun cabal install \
        --builddir ../build/seri \
        --with-happy=$::HAPPY \
        --force-reinstalls 

    #hrun cabal haddock --builddir ../build/seri
    hrun cabal sdist --builddir ../build/seri
}

# The seri-smt package
indir seri-smt {
    # Add the flag --enable-executable-profiling to this command to enable
    # profiling.
    hrun cabal install \
        --builddir ../build/seri-smt \
        --extra-lib-dirs $::env(LD_LIBRARY_PATH) \
        --force-reinstalls 

    #hrun cabal haddock --builddir ../build/seri-smt
    hrun cabal sdist --builddir ../build/seri-smt
}

# The binary executables
indir build/seri-bin {
    hrun ln -sf ../../seri-bin/seri.hs seri.hs
    hrun ghc -o seri seri.hs

    hrun ln -sf ../../seri-bin/enoch.hs enoch.hs
    hrun ghc -o enoch enoch.hs

    hrun ln -sf ../../seri-bin/sudoku.hs sudoku.hs
    hrun ghc -o sudoku sudoku.hs
}
    
set SERI build/seri-bin/seri
set ENOCH build/seri-bin/enoch
set SUDOKU build/seri-bin/sudoku

set SRI_SERI seri/sri
set SRI_SMT seri-smt/sri

# The general seri test
run $SERI --type \
    --include $::SRI_SERI --include $::SRI_SMT \
    -f $::SRI_SERI/Seri/Lib/Tests.sri \
    > build/tests.typed
run $SERI --io \
    --include $::SRI_SERI --include $::SRI_SMT \
    -m Seri.Lib.Tests.testallio \
    -f $::SRI_SERI/Seri/Lib/Tests.sri \
    > build/tests.got 
run echo "PASSED" > build/tests.wnt
hrun cmp build/tests.got build/tests.wnt

# Poorly typed tests.
proc badtypetest {name} {
    set cmd {
        run $::SERI --type \
            --include $::SRI_SERI --include $::SRI_SMT \
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
set hsdir build/
run $SERI --haskell \
    --include $::SRI_SERI --include $::SRI_SMT \
    -m testallio \
    -f $::SRI_SERI/Seri/Lib/Tests.sri \
    > $hsdir/hstests.hs
hrun -ignorestderr $GHC -o $hsdir/hstests $hsdir/hstests.hs
run ./$hsdir/hstests > $hsdir/hstests.got
run echo "PASSED" > $hsdir/hstests.wnt
hrun cmp $hsdir/hstests.got $hsdir/hstests.wnt

# The SMT query tests
proc smttest {name} {
    run $::SERI --io \
         --include $::SRI_SERI --include $::SRI_SMT \
         -m Seri.SMT.Tests.[string map {/ .} $name].main \
         -f $::SRI_SMT/Seri/SMT/Tests/$name.sri \
         > build/$name.out
}

smttest "Core"
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
        --include $::SRI_SERI --include $::SRI_SMT \
         -m Seri.IO.Tests.[string map {/ .} $name].main \
         -f $::SRI_SMT/Seri/IO/Tests/$name.sri {*}$args \
         > build/$name.out
}

iotest "Simple"
iotest "Query"

# The enoch tests
hrun $::ENOCH
hrun $::SUDOKU

puts "BUILD COMPLETE"

