
# Output the cabal file for seri to the given file.
proc cabal {fcabal} {
    set props {
        name seri
        version $::VERSION
        synopsis {Seri language compilers and infrastructure}
        license AllRightsReserved
        license-file LICENSE
        author {Richard Uhler}
        maintainer ruhler@csail.mit.edu
        category Language
        build-type Simple
        cabal-version >=1.8
        stability experimental
        data-files {[string map {src/sri sri "\n" " "} [exec find src/sri -name "*.sri"]]}
    }

    # exe hsfiles.
    # These determine which modules should be included in the seri library.
    set exes {
        Seri/seri.hs
        Seri/Enoch/enoch.hs
        Seri/Enoch/sudoku.hs
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

    set libmods [list]
    foreach {file} $exes {
        set mods [dict keys [hsdepends src "build/src/$file"]]
        set libmods [lsort -uniq [concat $libmods $mods]]
    }

    set fout [open $fcabal "w"]
    foreach {n v} $props {
        puts $fout [subst "$n: $v"]
    }

    puts $fout "source-repository this"
    puts $fout "  type: git"
    puts $fout "  location: /afs/csail.mit.edu/u/r/ruhler/git/seri"
    set branch [lindex [exec git status -sb | head -1] 1]
    puts $fout "  branch: $branch"
    puts $fout "  tag: [exec cat .git/refs/heads/$branch]"

    puts $fout library
    puts $fout "  exposed-modules: [join $libmods {, }]"
    puts $fout "  build-depends: [join $builddeps {, }]"
    puts $fout "  build-tools: happy"
    puts $fout "  extra-libraries: yices1_dummy yices2"

    close $fout
}

