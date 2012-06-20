
# Explicitly initialize the environment.
foreach key [array names ::env] {
    array unset ::env $key
}

# Get infomation about the local environment.
# local.tcl should set
#   ::HAPPY - path to the happy executable
#   ::GHC - path to ghc
#   ::env(...) - needed environment variables, such as:
#       PATH, GHC_PACKAGE_PATH
source tclmk/local.tcl

proc run {args} {
    puts $args
    exec {*}$args
}

# Create and set up a build directory for the build.
run mkdir -p build
run cp -l -r -n -t build src

# Generate the Seri Parser.hs from Parser.y
run -ignorestderr $HAPPY -o build/src/Seri/Lambda/Parser.hs \
    -ibuild/src/Seri/Lambda/Parser.info \
    build/src/Seri/Lambda/Parser.y
    
# Build some haskell programs.
proc ghcprog {target source args} {
    run $::GHC --make -rtsopts -o build/src/$target \
        -ibuild/src \
        build/src/$source {*}$args 
}

ghcprog "serie" "Seri/Target/Elaborate/serie.hs"
ghcprog "serih" "Seri/Target/Haskell/serih.hs"
ghcprog "serim" "Seri/Target/Monomorphic/serim.hs"
ghcprog "runquery" "Seri/SMT/runquery.hs"
ghcprog "type" "Seri/Lambda/type.hs"

set SERIE build/src/serie
set SERIH build/src/serih
set RUNQUERY build/src/runquery
set TYPE build/src/type

# The general seri test
run $TYPE -o build/src/tests.typed -i build/src build/src/Seri/Lib/Tests.sri
run $SERIE -o build/src/tests.got -i build/src -m testall \
    build/src/Seri/Lib/Tests.sri
run echo -n "(True :: Bool)" > build/src/tests.wnt
run cmp build/src/tests.got build/src/tests.wnt

# Test the haskell target.
set hsdir build/src/Seri/Target/Haskell
run $SERIH -o $hsdir/hstests.hs -i build/src -m testall \
    build/src/Seri/Lib/Tests.sri
run -ignorestderr $GHC -o $hsdir/hstests -ibuild/src $hsdir/hstests.hs
run ./$hsdir/hstests > $hsdir/hstests.got
run echo "True" > $hsdir/hstests.wnt
run cmp $hsdir/hstests.got $hsdir/hstests.wnt

# The SMT query tests
proc querytest {name} {
    run $::RUNQUERY -d build/src/Seri/SMT/Tests/$name.dbg -i build/src \
         build/src/Seri/SMT/Tests/$name.sri \
         > build/src/Seri/SMT/Tests/$name.out
}

querytest "Query1"
querytest "Query2"
querytest "Complex"
querytest "If"
querytest "Casenomatch"

