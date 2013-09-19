
# Explicitly initialize the environment.
foreach key [array names ::env] {
    array unset ::env $key
}

# Get infomation about the local environment.
# local.tcl should set
#   ::env(...) - needed environment variables, such as:
#           PATH, LD_LIBRARY_PATH
#   ::GHC - the path to the ghc executable to use for building
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
hrun mkdir -p build/home

set ::env(HOME) [pwd]/build/home
#hrun cabal update

# The smten-plugin package
proc smten-plugin {} {
    indir smten-plugin {
        hrun cabal install \
            --builddir ../build/smten-plugin-build \
            --with-compiler=$::GHC \
            --force-reinstalls

        hrun cabal sdist --builddir ../build/smten-plugin-build
    }
}

# The smten-base package
proc smten-base {} {
    hrun cp -r -f -l smten-base build/
    indir build/smten-base {
        hrun $::GHC --make -osuf o_smten -hisuf hi_smten -c \
            -fplugin=Smten.Plugin.Plugin Smten/Prelude.hs

        hrun cabal install --force-reinstalls --with-compiler=$::GHC
        hrun cabal sdist
    }
}

# The smten-lib package
proc smten-lib {} {
    hrun cp -r -f -l smten-lib build/
    indir build/smten-lib {
        hrun $::GHC --make -osuf o_smten -hisuf hi_smten -c \
            -main-is Smten.Tests.All.main \
            -fplugin=Smten.Plugin.Plugin Smten/Tests/All.hs

        hrun cabal configure --enable-tests --enable-benchmarks \
            --with-compiler=$::GHC
        hrun cabal build
        hrun cabal test
        hrun cabal sdist
        hrun cabal install --force-reinstalls --with-compiler=$::GHC
    }
}

proc smten-yices2 {} {
    # The smten-yices2 package
    hrun cp -r -f -l smten-yices2 build/
    indir build/smten-yices2 {
        hrun $::GHC --make -c -osuf o_smten -hisuf hi_smten \
            -main-is Smten.Tests.Yices2.main \
            -fplugin=Smten.Plugin.Plugin Smten/Tests/Yices2.hs

        hrun cabal configure --enable-tests --enable-benchmarks --with-compiler=$::GHC
        hrun cabal build
        hrun cabal test
        hrun cabal sdist
        hrun cabal install --force-reinstalls --with-compiler=$::GHC
    }
}

proc smten-yices1 {} {
    # The smten-yices1 package
    hrun cp -r -f -l smten-yices1 build/
    indir build/smten-yices1 {
        hrun $::GHC --make -c -osuf o_smten -hisuf hi_smten \
            -main-is Smten.Tests.Yices1.main \
            -fplugin=Smten.Plugin.Plugin Smten/Tests/Yices1.hs

        hrun cabal configure --enable-tests --enable-benchmarks --with-compiler=$::GHC
        hrun cabal build
        hrun cabal test
        hrun cabal sdist
        hrun cabal install --force-reinstalls --with-compiler=$::GHC
    }
}

proc smten-stp {} {
    # The smten-stp package
    hrun cp -r -f -l smten-stp build/
    indir build/smten-stp {
        hrun $::GHC --make -c -osuf o_smten -hisuf hi_smten \
            -main-is Smten.Tests.STP.main \
            -fplugin=Smten.Plugin.Plugin Smten/Tests/STP.hs

        hrun cabal configure --enable-tests --enable-benchmarks --with-compiler=$::GHC
        hrun cabal build
        hrun cabal test
        hrun cabal sdist
        hrun cabal install --force-reinstalls --with-compiler=$::GHC
    }
}

proc smten-z3 {} {
    # The smten-z3 package
    hrun cp -r -f -l smten-z3 build/
    indir build/smten-z3 {
        hrun $::GHC --make -c -osuf o_smten -hisuf hi_smten \
            -main-is Smten.Tests.Z3.main \
            -fplugin=Smten.Plugin.Plugin Smten/Tests/Z3.hs

        hrun cabal configure --enable-tests --enable-benchmarks --with-compiler=$::GHC
        hrun cabal build
        hrun cabal test
        hrun cabal sdist
        hrun cabal install --force-reinstalls --with-compiler=$::GHC
    }
}

proc smten-minisat {} {
    # The smten-minisat package
    hrun cp -r -f -l smten-minisat build/
    indir build/smten-minisat {
        hrun $::GHC --make -c -osuf o_smten -hisuf hi_smten \
            -main-is Smten.Tests.MiniSat.main \
            -fplugin=Smten.Plugin.Plugin Smten/Tests/MiniSat.hs

        hrun cabal configure --enable-tests --enable-benchmarks --with-compiler=$::GHC
        hrun cabal build
        hrun cabal test
        hrun cabal sdist
        hrun cabal install --force-reinstalls --with-compiler=$::GHC
    }
}



smten-plugin
smten-base
smten-lib

smten-yices2
smten-yices1
smten-stp
smten-z3
#smten-minisat

