#!/bin/bash
export GUROBI_HOME="/home/josep/builds/gurobi/gurobi700/linux64/"
export PATH="$PATH:/home/josep/builds/gurobi/gurobi700/linux64/bin/"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}/usr/local/lib/:/home/josep/Repositories/freeling/APIs/java/"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/home/josep/builds/gurobi/gurobi700/linux64/lib/:$PWD/native/linux64/"


mvn clojure:nrepl -Dclojure.nrepl.handler=cider.nrepl/cider-nrepl-handler

