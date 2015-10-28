# Building instructions

- Have [stack](https://github.com/commercialhaskell/stack)
- Have SDL 2.0.3 with library headers
  - If you don't have it, see [this instructions][build_sdl]
- Clone the repo:

  ```shell
  $ git clone "https://github.com/flomerz/yampa-uhr.git"
  Cloning into 'yampy-uhr'...
  ...
  ```
- Build it with `stack` or `cabal`:

  ```shell
  $ cd yampy-cube
  
  $ stack build

  $ cabal build
  ...
  ```
- Run it:

  ```shell
  $ stack exec yampy-cube

  $ cabal run
  ```

[build_sdl]: https://github.com/haskell-game/sdl2#building
