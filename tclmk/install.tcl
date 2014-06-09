
# same as run, but output stdout of the command here.
proc hrun {args} {
    puts $args
    exec {*}$args "2>@" stderr ">@" stdout
}

proc indir {dir script} {
    set wd [pwd]
    puts "tclmk: Entering directory `$wd/$dir'"
    cd $dir
    eval $script
    puts "tclmk: Leaving directory `$wd/$dir'"
    cd $wd
}

set ::USER $::env(USER)

proc unreg {x} {
   catch "hrun ghc-pkg unregister $x"
}   

proc install {x} {
    indir build/$x "hrun cabal install --builddir ../$x-$::USER"
}

unreg smten-yices1
unreg smten-minisat
unreg smten-stp
unreg smten-lib
unreg smten-base
unreg smten

install smten
install smten-base
install smten-lib
install smten-stp
install smten-minisat
install smten-yices1

