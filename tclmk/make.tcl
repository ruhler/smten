
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
run rm -rf build
run mkdir -p build
run cp -lr src build/src

# Generate the Seri Parser.hs from Parser.y
run -ignorestderr $HAPPY -o build/src/Seri/Lambda/Parser.hs \
    -ibuild/src/Seri/Lambda/Parser.info \
    build/src/Seri/Lambda/Parser.y
    
# Build some haskell programs.
proc ghcprog {target source args} {
    run $::GHC --make -o build/src/$target \
        -ibuild/src \
        build/src/$source {*}$args
}

ghcprog "serie" "Seri/Target/Elaborate/serie.hs"
ghcprog "monomorphic" "Seri/Target/Monomorphic/monomorphic.hs"
ghcprog "serif" "Seri/Serif/serif.hs"
ghcprog "runquery" "Seri/SMT/runquery.hs"

set SERIE build/src/serie
set SERIF build/src/serif
set RUNQUERY build/src/runquery

# Build the seri prelude
ghcprog "prelude.srigen" "Seri/Serif/prelude.hs"
run build/src/prelude.srigen > build/src/Seri/Lib/Prelude.hs

# Build the .sri files
set sris [list \
    "Seri/Lib/Bool" \
    "Seri/Lib/Integer" \
    "Seri/Lib/List" \
    "Seri/Lib/Maybe" \
    "Seri/Lib/Monad" \
    "Seri/Lib/Tuple" \
    "Seri/Lib/Tests" \
    "Seri/SMT/SMT"]

foreach {name} $sris {
    run $SERIF -o build/src/$name.hs build/src/$name.srif
}

foreach {name} $sris {
    ghcprog $name.srigen $name.hs -main-is [string map {/ .} $name]
    run build/src/$name.srigen > build/src/$name.sri
}

# The general seri test
run $SERIE -o build/src/tests.got -i build/src -m testall \
    build/src/Seri/Lib/Tests.sri
run echo -n "(True :: Bool)" > build/src/tests.wnt
run cmp build/src/tests.got build/src/tests.wnt

# The SMT query tests
proc querytest {name} {
    run $::SERIF -o build/src/Seri/SMT/Tests/$name.hs \
         build/src/Seri/SMT/Tests/$name.srif
    ghcprog Seri/SMT/Tests/$name.srigen Seri/SMT/Tests/$name.hs \
         -main-is [string map {/ .} Seri/SMT/Tests/$name]
    run build/src/Seri/SMT/Tests/$name.srigen \
         > build/src/Seri/SMT/Tests/$name.sri
    run $::RUNQUERY -d build/src/Seri/SMT/Tests/$name.dbg -i build/src \
         build/src/Seri/SMT/Tests/$name.sri \
         > build/src/Seri/SMT/Tests/$name.out
}

querytest "Query1"
querytest "Query2"
querytest "Complex"
querytest "If"
querytest "Casenomatch"

