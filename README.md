# ATDlib and Model Judge

This repository contains all the code used in my Master thesis: 
"Model-Agnostic Process Modeling". The projects can be built with
maven, although some external depencencies need to be manually handled:

- FreeLing:
    - FreeLing 4.0 or higher must be installed as a system library and available
      at the system's library path (LD_LIBRARY_PATH)
    - The FreeLing JNI wrapper must be installed. freeling.jar must be available 
      in the local maven repository (must be manually installed as version 4.0.0). 
      Additionally, freeling.so / freeling.dll must be available at the system's
      library path (LD_LIBRARY_PATH on linux)

- Gurobi ILP Solver
    - Gurobi must be installed and the 'gurobi-cl' program must be available to 
      the system executable path. Please note that a commercial or academic license 
      is required to run this sofware.


