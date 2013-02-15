
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
hrun mkdir -p build/home build/test build/test/Squares2

set ::env(HOME) [pwd]/build/home
#hrun cabal update
hrun cabal install cmdargs syb

# The smten package
indir smten {
    hrun cabal install \
        --builddir ../build/smten \
        --with-happy=$::HAPPY \
        --force-reinstalls 

    #hrun cabal haddock --builddir ../build/smten
    hrun cabal sdist --builddir ../build/smten
}

# The smten-smt package
indir smten-smt {
    hrun cabal install \
        --builddir ../build/smten-smt \
        --extra-lib-dirs $::env(LD_LIBRARY_PATH) \
        --force-reinstalls 

    #hrun cabal haddock --builddir ../build/smten-smt
    hrun cabal sdist --builddir ../build/smten-smt
}

# The smten-bin package
indir smten-bin {
    hrun cabal install \
        --builddir ../build/smten-bin \
        --force-reinstalls 

    hrun cabal sdist --builddir ../build/smten-bin
}

set SMTEN build/home/.cabal/bin/smten
set DSEL build/smten-bin/dsel
set SUDOKU build/smten-bin/sudoku

set SMTN smten/smtn

# Poorly typed tests.
proc badtypetest {name} {
    set cmd {
        run $::SMTEN --type \
            -f $::SMTN/Smten/Tests/MalTyped/$name.smtn \
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
badtypetest "ClassCtx1"
#badtypetest "DupInst"

# Run an IO Test
proc io {module} {
    set smtdir build/test
    hrun $::SMTEN --io \
        --main-is $module.main \
        -f $::SMTN/[string map {. /} $module].smtn
}

# Run a HaskellF Test
proc haskellf {module} {
    set hsdir build/test
    run $::SMTEN --haskellf \
        -f $::SMTN/[string map {. /} $module].smtn \
        > $hsdir/[string map {. _} $module].hs
    hrun -ignorestderr ghc -fno-warn-overlapping-patterns \
        -fno-warn-missing-fields \
        -main-is __main \
        -o $hsdir/[string map {. _} $module] $hsdir/[string map {. _} $module].hs
    hrun ./$hsdir/[string map {. _} $module]
}

io Smten.Tests.Concrete

# Tests in the new SMT API
io Smten.SMT.Tests.Core2
io Smten.SMT.Tests.Datatype2
io Smten.SMT.Tests.QRef
io Smten.SMT.Tests.Nest
io Smten.SMT.Tests.Integer2
io Smten.SMT.Tests.Bit2
io Smten.SMT.Tests.Share2

# HaskellF tests
haskellf Smten.Tests.Concrete
haskellf Smten.SMT.Tests.Core2
haskellf Smten.SMT.Tests.Datatype2
haskellf Smten.SMT.Tests.QRef
haskellf Smten.SMT.Tests.Nest
haskellf Smten.SMT.Tests.Integer2
haskellf Smten.SMT.Tests.Bit2
haskellf Smten.SMT.Tests.Share2

io Smten.SMT.Tests.AllQ
io Smten.SMT.Tests.AllQ2
io Smten.SMT.Tests.Isolate0
io Smten.SMT.Tests.Sudoku

# Tests in the old SMT API
io Smten.SMT.Tests.Core
io Smten.SMT.Tests.Datatype
io Smten.SMT.Tests.Scoped
io Smten.SMT.Tests.Integer
io Smten.SMT.Tests.Bit
io Smten.SMT.Tests.Share

io Smten.SMT.Tests.Bluespec
io Smten.SMT.Tests.Tuple
io Smten.SMT.Tests.Squares2.Squares


# The pretty printer test
indir build/smten-bin {
    hrun ln -sf ../../smten-bin/pprtest.hs pprtest.hs
    hrun ghc --make -o pprtest pprtest.hs
}
hrun ./build/smten-bin/pprtest

# The semantics test
indir build/smten-bin {
    hrun ln -sf ../../smten-bin/semtest.hs semtest.hs
    hrun ghc --make -o semtest semtest.hs
}
hrun ./build/smten-bin/semtest


## The sudoku haskell integration test.
#run $::SMTEN --haskellf \
#    --include $::SMTN \
#    --no-main \
#    --mod-name Smten_SMT \
#    -f $::SMTN/Smten/SMT/Symbolic.smtn \
#    > build/smten-bin/Smten_SMT.hs
#indir build/smten-bin {
#    hrun ln -sf ../../smten-bin/sudoku.hs sudoku.hs
#    hrun ghc --make -o sudoku sudoku.hs
#}
#hrun ./build/smten-bin/sudoku
#
## The dsel haskell integration test.
#run $::SMTEN --haskellf \
#    --include $::SMTN \
#    --no-main \
#    --mod-name Smten_DSEL \
#    -f $::SMTN/Smten/Tests/DSEL.smtn \
#    > build/smten-bin/Smten_DSEL.hs
#indir build/smten-bin {
#    hrun ln -sf ../../smten-bin/dsel.hs dsel.hs
#    hrun ghc --make -o dsel dsel.hs
#}
#hrun ./build/smten-bin/dsel


puts "BUILD COMPLETE"

