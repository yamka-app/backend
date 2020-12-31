#!/usr/bin/env sh

rebar3 compile
sudo -E sh -c "export $(cat credentials) && cd _build/default/lib/orderbackend/ebin/ && erl -pa /usr/lib/cqerl -noshell -s main start -s init stop"