#!/usr/bin/env sh
if rebar3 check
then
    sh -c "$(cat credentials.sh) && rebar3 shell"
fi