
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
hrun mkdir -p build/home build/test

set ::env(HOME) [pwd]/build/home
#hrun cabal update
hrun cabal install cmdargs syb missingh

# The smten package
indir smten {
    hrun cabal install \
        --builddir ../build/smten \
        --with-happy=$::HAPPY \
        --force-reinstalls  -O0

    #hrun cabal haddock --builddir ../build/smten
    hrun cabal sdist --builddir ../build/smten
}

# The smten-smt package
indir smten-smt {
    hrun cabal install \
        --builddir ../build/smten-smt \
        --extra-lib-dirs $::env(LD_LIBRARY_PATH) \
        --force-reinstalls  -O0

    #hrun cabal haddock --builddir ../build/smten-smt
    hrun cabal sdist --builddir ../build/smten-smt
}

# The smten-bin package
indir smten-bin {
    hrun cabal install \
        --builddir ../build/smten-bin \
        --force-reinstalls -O0

    hrun cabal sdist --builddir ../build/smten-bin
}

set SMTEN build/home/.cabal/bin/smten
set SMTN smten/share/lib

# Poorly typed tests.
proc shouldfail {name} {
    set cmd {
        hrun $::SMTEN --phases \
            -f $::SMTN/Smten/Tests/ShouldFail/$name.smtn \
            -o "build/test/$name"
        }

    if { [catch $cmd] == 0 } {
        error "expected failure, but $name passed type check"
    }
}

proc expectfail {cmd} {
    if { [catch $cmd] == 0 } {   
        error "expected failure, but $cmd succeeded"
    }
}

proc hscomp {module} {
    set hsdir build/test
    hrun $::SMTEN --haskellf \
        -f $::SMTN/[string map {. /} $module].smtn \
        --hsdir $hsdir
}

proc hsghc {module} {
    set hsdir build/test
    hrun -ignorestderr ghc -fno-warn-overlapping-patterns \
        -fno-warn-missing-fields \
        -main-is Smten.Lib.$module.main__ -i$hsdir \
        -prof -rtsopts -O0 \
        -o $hsdir/[string map {. _} $module] $hsdir/Smten/Lib/[string map {. /} $module].hs
}

# Run a HaskellF Test
proc hf {module} {
    set hsdir build/test
    hscomp $module
    hsghc $module
    hrun ./$hsdir/[string map {. _} $module] +RTS -K1g
}


hf Smten.Tests.All
expectfail { hf Smten.SMT.Tests.MalError }

shouldfail "Ambiguous"
#shouldfail "BadKind"
shouldfail "BadType1"
shouldfail "BadType2"
shouldfail "BadType3"
shouldfail "BadType4"
shouldfail "ClassCtx1"
shouldfail "ClauseArgCount"
shouldfail "Ctx"
#shouldfail "DupInst"
#shouldfail "DupVar"
shouldfail "FreeDataCon"
shouldfail "FreeTyCon"
shouldfail "FreeTypeVar"
shouldfail "FreeVar"
shouldfail "InstCtx"

puts "BUILD COMPLETE"

