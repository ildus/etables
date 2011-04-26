#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.

cd `dirname $0`
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -s tables \
    -mnesia dir '"db/dev"'
    -detached
