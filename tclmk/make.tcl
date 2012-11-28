
# Explicitly initialize the environment.
foreach key [array names ::env] {
    array unset ::env $key
}

# Get infomation about the local environment.
# local.tcl should set
#   ::HAPPY - path to the happy executable
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
hrun mkdir -p build/home build/seri-bin build/test build/test/Squares2

set ::env(HOME) [pwd]/build/home
#hrun cabal update
hrun cabal install cmdargs syb

# The seri package
indir seri {
    hrun cabal install \
        --builddir ../build/seri \
        --with-happy=$::HAPPY \
        --force-reinstalls 

    #hrun cabal haddock --builddir ../build/seri
    hrun cabal sdist --builddir ../build/seri
}

# The seri-smt package
indir seri-smt {
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
    #hrun ghc -o seri seri.hs
    hrun ghc -rtsopts -prof -auto-all -o seri seri.hs

    #hrun ln -sf ../../seri-bin/dsel.hs dsel.hs
    #hrun ghc -o dsel dsel.hs

    #hrun ln -sf ../../seri-bin/sudoku.hs sudoku.hs
    #hrun ghc -o sudoku sudoku.hs
}
    
set SERI build/seri-bin/seri
set DSEL build/seri-bin/dsel
set SUDOKU build/seri-bin/sudoku

set SRI_SERI seri/sri

# Poorly typed tests.
proc badtypetest {name} {
    set cmd {
        run $::SERI --type \
            --include $::SRI_SERI \
            -f $::SRI_SERI/Seri/Tests/MalTyped/$name.sri \
            > "build/test/$name.typed"
        }

    if { [catch $cmd] == 0 } {
        error "expected type error, but $name passed type check"
    }
}

badtypetest "BadType1"
badtypetest "BadType2"
badtypetest "Ctx"
badtypetest "InstCtx"

# Run an IO Test
proc io {module} {
    set smtdir build/test
    hrun $::SERI --io \
        --include $::SRI_SERI \
        -m $module.main \
        -f $::SRI_SERI/[string map {. /} $module].sri
}

# Run a HaskellF Test
proc haskellf {module} {
    set hsdir build/test
    run $::SERI --haskellf \
        --include $::SRI_SERI \
        -f $::SRI_SERI/[string map {. /} $module].sri \
        > $hsdir/[string map {. _} $module].hs
    hrun -ignorestderr ghc -fno-warn-overlapping-patterns \
        -fno-warn-missing-fields \
        -o $hsdir/[string map {. _} $module] $hsdir/[string map {. _} $module].hs
    hrun ./$hsdir/[string map {. _} $module]
}

io Seri.Tests.Basic
io Seri.SMT.Tests.Core
io Seri.SMT.Tests.Datatype
io Seri.SMT.Tests.Scoped
io Seri.SMT.Tests.Integer
io Seri.SMT.Tests.Bit

haskellf Seri.Tests.Basic
haskellf Seri.SMT.Tests.Core
haskellf Seri.SMT.Tests.Datatype
haskellf Seri.SMT.Tests.Scoped
haskellf Seri.SMT.Tests.Integer
#haskellf Seri.SMT.Tests.Bit

io Seri.SMT.Tests.Bluespec
io Seri.SMT.Tests.Array
io Seri.SMT.Tests.Share
io Seri.SMT.Tests.Tuple
io Seri.SMT.Tests.AllQ
io Seri.SMT.Tests.AllQ2
io Seri.SMT.Tests.Squares2.Squares
io Seri.SMT.Tests.Sudoku
io Seri.SMT.Tests.Sudoku2
io Seri.SMT.Tests.Sudoku3
io Seri.SMT.Tests.Isolate0

#hrun $DSEL
#hrun $SUDOKU

puts "BUILD COMPLETE"

