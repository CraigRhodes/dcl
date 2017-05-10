#This script compiles a `float.dcl` source script. (These are the manual steps behind our `run` command.)

./dcl.native < float.dcl > float.ll
llc < float.ll > float.s
cc -o float float.s externalcalls.o -lm
./float
