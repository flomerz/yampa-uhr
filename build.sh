#!/bin/bash

cabal clean
cabal sandbox delete

cabal sandbox init

cabal install --only-dependencies --enable-tests
