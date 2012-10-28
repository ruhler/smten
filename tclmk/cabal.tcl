
# Output the cabal file for seri to the given file.
proc cabal {fcabal} {
    set props {
        name seri
        version 0.1.1.1
        synopsis {Seri language compilers and infrastructure}
        license AllRightsReserved
        license-file LICENSE
        author {Richard Uhler}
        maintainer ruhler@csail.mit.edu
        category Language
        build-type Simple
        cabal-version >=1.8
        stability experimental
        data-files {Seri/Lib/*.sri Seri/SMT/*.sri Seri/Enoch/*.sri}
    }

    # exe hsfile libraries
    set exes {
        seri Seri/seri.hs {}
        enoch Seri/Enoch/enoch.hs {}
        sudoku Seri/Enoch/sudoku.hs {}
    }

    set builddeps {
        {base ==4.5.*}
        {mtl ==2.1.*}
        {pretty ==1.1.*}
        {array ==0.4.*}
        {directory ==1.1.*}
        {template-haskell ==2.7.*}
        {hashable ==1.1.*}
        {containers ==0.4.*}
        {bytestring ==0.9.*}
        {cmdargs ==0.10.*}
        {syb ==0.3.*}
    }

    set deps [dict create]
    set libmods [list]
    foreach {exe file libs} $exes {
        set mods [dict keys [hsdepends src "build/src/$file"]]
        dict set deps $exe $mods
        set libmods [lsort -uniq [concat $libmods $mods]]
    }

    set fout [open $fcabal "w"]
    foreach {n v} $props {
        puts $fout "$n: $v"
    }

    puts $fout "source-repository this"
    puts $fout "  type: git"
    puts $fout "  location: /afs/csail.mit.edu/u/r/ruhler/git/seri"
    set branch [lindex [exec git status | head -1] 3]
    puts $fout "  branch: $branch"
    puts $fout "  tag: [exec cat .git/refs/heads/$branch]"

    puts $fout library
    puts $fout "  exposed-modules: [join $libmods {, }]"
    puts $fout "  build-depends: [join $builddeps {, }]"
    puts $fout "  build-tools: happy"
    puts $fout "  extra-libraries: yices1_dummy yices2"

    # Don't build any executables, because cabal can't handle mixing of the
    # yices1 library and template haskell.
#    foreach {exe file libs} $exes {
#        puts $fout "executable $exe"
#        puts $fout "  main-is: $file"
#        puts $fout "  other-modules: [join [dict get $deps $exe] {, }]"
#        puts $fout "  build-depends: [join $builddeps {, }]"
#        puts $fout "  extra-libraries: [join $libs {, }]"
#        puts $fout "  build-tools: happy"
#        puts $fout "  ghc-options: -rtsopts"
#        puts $fout "  ghc-prof-options: -auto-all"
#    }
    close $fout
}

