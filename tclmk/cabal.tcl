
# Output the cabal file for seri to the given file.
proc cabal {fcabal} {
    set props {
        name seri
        version 0.1.0.0
        synopsis {Seri language compilers and infrastructure}
        license AllRightsReserved
        license-file LICENSE
        author {Richard Uhler}
        maintainer ruhler@csail.mit.edu
        category Language
        build-type Simple
        cabal-version >=1.8
        data-files {Seri/Lib/*.sri Seri/SMT/SMT.sri}
    }

    # exe hsfile libraries
    set exes {
        serit Seri/Lambda/serit.hs {}
        serie Seri/Elaborate/serie.hs {}
        serih Seri/Haskell/serih.hs {}
        serio Seri/IO/serio.hs {}
        seriq1 Seri/SMT/seriq1.hs yices1
        seriq2 Seri/SMT/seriq2.hs yices2
        enoch Seri/Enoch/enoch.hs yices2
        sudoku Seri/Enoch/sudoku.hs yices2
    }

    set builddeps {
        {base ==4.5.*}
        {mtl ==2.1.*}
        {pretty ==1.1.*}
        {array ==0.4.*}
        {directory ==1.1.*}
        {template-haskell ==2.7.*}
        {hashable ==1.1.*}
        {containers ==0.5.*}
        {bytestring ==0.9.*}
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

    puts $fout library
    puts $fout "  exposed-modules: [join $libmods {, }]"
    puts $fout "  build-depends: [join $builddeps {, }]"
    puts $fout "  build-tools: happy"

    foreach {exe file libs} $exes {
        puts $fout "executable $exe"
        puts $fout "  main-is: $file"
        puts $fout "  other-modules: [join [dict get $deps $exe] {, }]"
        puts $fout "  build-depends: [join $builddeps {, }]"
        puts $fout "  extra-libraries: [join $libs {, }]"
        puts $fout "  build-tools: happy"
        puts $fout "  ghc-options: -rtsopts"
        puts $fout "  ghc-prof-options: -auto-all"
    }
    close $fout
}

