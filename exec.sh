#!/usr/bin/env bash

AGENTLIB_HPROF_ARGS="-agentlib:hprof=cpu=samples,heap=all,interval=20,depth=10,verbose=n,doe=y"

rm -f output.gnucash ; cp ../GNUCash_Files/all_finances.gnucash ./ ;   \
    ( time \
        ( java \
            "$AGENTLIB_HPROF_ARGS" \
            -jar ~/git/accregex/target/scala-2.12/accregex-assembly-1.0.jar \
                -i all_finances.gnucash -o output.gnucash --debug 1> accregex.stdout 2> accregex.stderr \
                ) ) 1> time.stdout 2> time.stderr
