#!/bin/bash

cabal clean
cabal sandbox delete

cabal sandbox init
cabal sandbox add-source libs/sdl2-ttf

cabal install --only-dependencies --enable-tests
