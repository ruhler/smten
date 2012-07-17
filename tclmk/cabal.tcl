
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
        serie Seri/Target/Elaborate/serie.hs {}
        serih Seri/Target/Haskell/serih.hs {}
        serim Seri/Target/Monomorphic/serim.hs {}
        seriq Seri/SMT/seriq.hs yices
        seriq2 Seri/SMT/seriq2.hs yices2
    }

    set builddeps {
        {base ==4.5.*}
        {syb ==0.3.*}
        {mtl ==2.1.*}
        {pretty ==1.1.*}
        {array ==0.4.*}
        {directory ==1.1.*}
        {template-haskell ==2.7.*}
        {yices ==0.0.*}
        {unix ==2.5.*}
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
        puts $fout "  ghc-options: -rtsopts -dcore-lint -debug"
        puts $fout "  ghc-prof-options: -auto-all"
    }
    close $fout
}

