Smten Yices1 Solver
===================
Richard Uhler <ruhler@csail.mit.edu>
July 26, 2013

Installing Yices1
-----------------
The smten-yices2 solver requires yices1 is installed. Yices1 can be downloaded
from http://yices.csl.sri.com. The smten-yices1 package
looks for the yices1 library under the name "yices1". Because the yices1
library ships as "yices", you will need to provide a link to "yices1".

In order to use both yices1 and yices2 libraries in a single executable, smten
assumes the symbols in the yices1 library have been renamed so that prefixes
yices_ are now yices1_. The rename map is included in this directory.

For example, if you have libyices.a for yices1 to create the "yices1"
library suitable for use with smten, run the command:

   objcopy --redefine-syms=yicse1rename.txt libyices.a libyices1.a

You may need to add the library path containing libyices1.a as an extra
library directory to your cabal configuration file. For example, if the yices1
library is installed in /home/ruhler/local/lib, and your cabal configuration
is in ~/.cabal/config, add the following line to ~/.cabal/config:

    extra-lib-dirs: /home/ruhler/local/lib

