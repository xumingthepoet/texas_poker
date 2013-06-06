#!/bin/bash

echo compiling ...

./rebar compile

erl -pa ebin -pa deps/*/ebin -s texas_poker
