
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
#   ::YICESLIB - path the the yices library
source tclmk/local.tcl
set ::env(LANG) "en_US.UTF-8"

proc run {args} {
    puts $args
    exec {*}$args "2>@" stderr
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

# Yices2  tests
ghcprog "Yices2/Tests/bool_eqs2" "Yices2/Tests/bool_eqs2.hs" $YICES2LIB
run ./build/src/Yices2/Tests/bool_eqs2

ghcprog "serie" "Seri/Target/Elaborate/serie.hs"
ghcprog "serih" "Seri/Target/Haskell/serih.hs"
ghcprog "serim" "Seri/Target/Monomorphic/serim.hs"
ghcprog "seriq" "Seri/SMT/seriq.hs" $YICESLIB
ghcprog "seriq2" "Seri/SMT/seriq2.hs" $YICES2LIB
ghcprog "serit" "Seri/Lambda/serit.hs"

set SERIE build/src/serie
set SERIH build/src/serih
set SERIQ build/src/seriq
set SERIQ2 build/src/seriq2
set SERIT build/src/serit

# The general seri test
run $SERIT -o build/src/tests.typed -i build/src build/src/Seri/Lib/Tests.sri
run $SERIE -o build/src/tests.got -i build/src -m testall \
    build/src/Seri/Lib/Tests.sri
run echo -n "(True :: Bool)" > build/src/tests.wnt
run cmp build/src/tests.got build/src/tests.wnt

# Poorly typed tests.
proc badtypetest {name} {
    set btdir "build/src/Seri/Lambda/Tests"
    set cmd {run $::SERIT -o $btdir/$name.typed -i build/src $btdir/$name.sri}
    if { [catch $cmd] == 0 } {
        error "expected type error, but $name passed type check"
    }
}

badtypetest "BadType1"
badtypetest "BadType2"
badtypetest "InstCtx"


# Test the haskell target.
set hsdir build/src/Seri/Target/Haskell
run $SERIH -o $hsdir/hstests.hs -i build/src -m testall \
    build/src/Seri/Lib/Tests.sri
run -ignorestderr $GHC -o $hsdir/hstests -ibuild/src $hsdir/hstests.hs
run ./$hsdir/hstests > $hsdir/hstests.got
run echo "True" > $hsdir/hstests.wnt
run cmp $hsdir/hstests.got $hsdir/hstests.wnt

# The SMT query tests
proc querytest {name args} {
    run $::SERIQ -d build/src/Seri/SMT/Tests/$name.dbg -i build/src \
         build/src/Seri/SMT/Tests/$name.sri {*}$args \
         > build/src/Seri/SMT/Tests/$name.out
}

querytest "Query1"
querytest "Query2"
querytest "Complex"
querytest "If"
querytest "Casenomatch"
querytest "Bluespec"

# The SMT query2 tests
proc query2test {name args} {
    run $::SERIQ2 -d build/src/Seri/SMT/Tests/$name.dbg -i build/src \
         build/src/Seri/SMT/Tests/$name.sri {*}$args \
         > build/src/Seri/SMT/Tests/$name.out
}

query2test "Query1"
query2test "Query2"
query2test "Complex"
query2test "If"
query2test "Casenomatch"
query2test "Bluespec"

# The cabal package
set wd [pwd]
cd build/src
run cabal sdist 
cd $wd



puts "BUILD COMPLETE"

