#!/usr/bin/env sh

rebar3 compile
sh -c "$(cat credentials.sh) && rebar3 shell"