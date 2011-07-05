#!/bin/sh

cd `dirname $0`
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname tables_dev \
    -s tables \
    -s reloader \
    -mnesia dir '"db/dev"' 
    -detached