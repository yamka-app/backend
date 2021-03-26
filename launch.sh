#!/usr/bin/env sh
if rebar3 check
then
    rebar3 shell
fi