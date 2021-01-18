#!/usr/bin/env sh
if rebar3 dialyzer
then
    sh -c "$(cat credentials.sh) && rebar3 shell"
fi