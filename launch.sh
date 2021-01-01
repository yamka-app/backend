#!/usr/bin/env sh

rebar3 compile
sudo -E sh -c "$(cat credentials.sh) && rebar3 shell"