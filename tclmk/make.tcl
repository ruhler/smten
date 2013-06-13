
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
#hrun cabal install

# The smten package
indir smten {
    hrun cabal install \
        --builddir ../build/smten \
        --with-happy=$::HAPPY \
        --force-reinstalls

    #hrun cabal haddock --builddir ../build/smten
    hrun cabal sdist --builddir ../build/smten
}

set SMTEN build/home/.cabal/bin/smten
set SMTN smten/share/lib

proc hscomp {module} {
    set hsdir build/test
    hrun $::SMTEN --include $::SMTN -f $::SMTN/[string map {. /} $module].smtn --hsdir $hsdir
}

proc hsghc {module} {
    set hsdir build/test
    hrun -ignorestderr ghc -fno-warn-overlapping-patterns \
        -fno-warn-missing-fields \
        -main-is Smten.Lib.$module.main__ -i$hsdir \
        -prof -rtsopts \
        -o $hsdir/[string map {. _} $module] $hsdir/Smten/Lib/[string map {. /} $module].hs
}

# Run a HaskellF Test
proc hf {module} {
    set hsdir build/test
    hscomp $module
    hsghc $module
    hrun ./$hsdir/[string map {. _} $module]
}


hf Smten.Tests.All
puts "BUILD COMPLETE"

