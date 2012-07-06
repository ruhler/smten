
# Return the list of modules a given haskell file imports.
proc hsimports {file} {
    set fin [open $file "r"]
    set text [read $fin]
    set import_re {import\s+(?:qualified\s+)?([[:alnum:].]+)}
    set matches [regexp -all -inline $import_re $text]
    return [dict values $matches]
}

# Return the list of modules in the given search path that
# the given haskell file depends on.
# Returns a dictionary mapping module to file name.
#   path - a list of paths to search for files in
#   file - the file to find dependencies for
#   exclude - an optional list of modules to exclude from analysis
proc hsdepends {path file {exclude ""}} {
    set depends [dict create]
    foreach m [hsimports $file] {
        if {([lsearch $exclude $m] == -1)} {
            foreach p $path {
                set f [file join $p [string map {. /} $m].hs]
                if {![file isfile $f]} {
                    set f [file join $p [string map {. /} $m].y]
                }
                if {[file isfile $f]} {
                    set nexclude [concat $exclude [dict keys $depends]]
                    set ndeps [hsdepends $path $f $nexclude]
                    dict set depends $m $f
                    set depends [dict merge $depends $ndeps]
                }
            }
        }
    }
    return $depends
}

