Smten STP Solver
===================
Richard Uhler <ruhler@csail.mit.edu>
July 24, 2013

Installing STP
-----------------
The smten-stp solver requires STP is installed. STP can be downloaded
from http://sites.google.com/site/stpfastprover.

You may need to add the library path containing libstp as an extra
library directory to your cabal configuration file. For example, if the stp
library is installed in /home/ruhler/local/lib, and your cabal configuration
is in ~/.cabal/config, add the following line to ~/.cabal/config:

    extra-lib-dirs: /home/ruhler/local/lib

