
# GHC Tool

# Builder: GHCProgram
#   Builds a program given the main .hs file. Figures out dependencies
#   automatically.
#
# Influential Variables:
#   GHC - The ghc executable to use.
#   GHCPATH - Path used in searching for imported files.
#   GHCFLAGS - Additional flags to pass to ghc
#   CPPPATH - Path used in searching for included c files.
#   LIBPATH - Path used in searching for libraries.
#   LIBS - List of libraries to link with.
#
#   ['ENV']['GHC_PACKAGE_PATH'] -- Where to look for ghc packages.

from SCons.Builder import Builder
from SCons.Scanner import Scanner, FindPathDirs
import SCons.Node.FS

import re
import os.path

import_re = re.compile(r"[\s]*import[\s]+(?:qualified[\s]+)?([A-Za-z0-9.]+)")

def ghcfile_scan(node, env, path):
    contents = node.get_text_contents()
    modules = import_re.findall(contents)
    files = []
    for mod in modules:
        f = SCons.Node.FS.find_file(mod.replace('.', '/') + ".hs", path)
        if f:
            files.append(f)
    return files

def xxxflags(xxxs, prefix):
    flags = ""
    for x in xxxs:
        flags += " %s%s " % (prefix, x)
    return flags

def cpppathflags(cpppath):
    return xxxflags(cpppath, "-I")

def libpathflags(libpath):
    return xxxflags(libpath, "-L")

def libflags(libs):
    return xxxflags(libs, "-l")

def iflags(path):
    return xxxflags(path, "-i")

def exists(env):
    # This might be right. You'll know if it isn't I suspect.
    return True

def generate(env):
    ghcscan = Scanner(function = ghcfile_scan, skeys=['.hs'],
            path_function=FindPathDirs('GHCPATH'), recursive=True)

    ghcbld = Builder(action =
            "$GHC --make -o $TARGET $SOURCES"
            + " ${GHCIFLAGS(GHCPATH)}"
            + " ${GHCCPPPATHFLAGS(CPPPATH)}"
            + " ${GHCLIBPATHFLAGS(LIBPATH)}"
            + " ${GHCLIBFLAGS(LIBS)}"
            + " ${GHCFLAGS}",
            src_suffix = ".hs")

    env['GHCIFLAGS'] = iflags
    env['GHCCPPPATHFLAGS'] = cpppathflags
    env['GHCLIBPATHFLAGS'] = libpathflags
    env['GHCLIBFLAGS'] = libflags

    env.Append(SCANNERS = ghcscan )
    env.Append(BUILDERS = {'GHCProgram' : ghcbld})

