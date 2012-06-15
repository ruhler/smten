
# Seri Tool
# Has a Seri Scanner.

from SCons.Builder import Builder
from SCons.Scanner import Scanner, FindPathDirs
import SCons.Node.FS

import re
import os.path

import_re = re.compile(r"[\s]*import[\s]+(?:qualified[\s]+)?([A-Za-z0-9.]+)")

def sriffile_scan(node, env, path):
    contents = node.get_text_contents()
    modules = import_re.findall(contents)
    files = []
    for mod in modules:
        f = SCons.Node.FS.find_file(mod.replace('.', '/') + ".srif", path)
        if f:
            files.append(f)
    return files

def exists(env):
    # This might be right. You'll know if it isn't I suspect.
    return True

def generate(env):
    srifscan = Scanner(function = sriffile_scan, skeys=['.srif'],
            path_function=FindPathDirs('LIBPATH'), recursive=True)

    env.Append(SCANNERS = srifscan)

