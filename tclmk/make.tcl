
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

# The smten-plugin package
indir smten-plugin {
    hrun cabal install \
        --builddir ../build/smten-plugin-build \
        --with-happy=$::HAPPY \
        --force-reinstalls

    #hrun cabal haddock --builddir ../build/smten-plugin-build
    hrun cabal sdist --builddir ../build/smten-plugin-build
}

# The smten-base package
hrun cp -r -f -l smten-base build/
indir build/smten-base {
    hrun ghc --make -c -fplugin=Smten.Plugin.Plugin Smten/Prelude.hs

    hrun cabal install \
        --builddir smten-base-build \
        --with-happy=$::HAPPY \
        --force-reinstalls

    hrun cabal sdist --builddir smten-base-build
}

set SMTN smten-lib/

proc hscomp {module} {
    set hsdir build/test
    hrun -ignorestderr ghc -fplugin=Smten.Plugin.Plugin \
    -O0 -i$::SMTN -odir $hsdir \
    -main-is $module.main \
    -o $hsdir/[string map {. _} $module].haskell -odir $hsdir -hidir $hsdir\
    $::SMTN/[string map {. /} $module].hs
}

proc hsghc {module} {
    set hsdir build/test
    hrun -ignorestderr ghc \
        -prof -rtsopts \
        -main-is Smten.Compiled.$module.main \
        -i$hsdir -ismten-lib -hidir $hsdir -odir $hsdir \
        -o $hsdir/[string map {. _} $module].smten \
        $hsdir/[string map {. /} Smten.Compiled.$module].hs
}

proc hsrun {module} {
    set hsdir build/test
    hscomp $module
    hrun ./$hsdir/[string map {. _} $module].haskell

    hsghc $module
    hrun ./$hsdir/[string map {. _} $module].smten
}


hsrun Smten.Tests.All
puts "BUILD COMPLETE"

