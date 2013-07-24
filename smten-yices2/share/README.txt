Smten Yices2 Solver
===================
Richard Uhler <ruhler@csail.mit.edu>
July 24, 2013

Installing Yices2
-----------------
The smten-yices2 solver requires yices2 is installed. Yices2 can be downloaded
from http://yices.csl.sri.com/download-yices2.shtml. The smten-yices2 package
looks for the yices2 library under the name "yices2". Because the yices2
library ships as "yices", you will need to provide a link to "yices2".

For example, if you have libyices.so.2.1.0 for yices2 installed, to create the
"yices2" library suitable for use with smten, run the commands:

   ln -s libyices.so.2.1.0 libyices2.so
   ln -s libyices.so.2.1.0 libyices.so.2.0 

You may need to add the library path containing libyices.so.2.1.0 as an extra
library directory to your cabal configuration file. For example, if the yices2
library is installed in /home/ruhler/local/lib, and your cabal configuration
is in ~/.cabal/config, add the following line to ~/.cabal/config:

    extra-lib-dirs: /home/ruhler/local/lib


