# Unity Tutorial in Haskell with Apecs and sdl2

Problems

- Some .aif files wouldn't work so converted them to ogg
- Because of https://github.com/haskell-game/sdl2-mixer/issues/5

## Building on Windows

### 1. Install SDL2 via stack & pacman:

`stack exec -- pacman -Syu`

`stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2`

### 2. Edit stack.yaml and remove integersimple ghc

The current code uses a custom compiled ghc for license reasons (see: https://ro-che.info/articles/2017-03-10-haskell-without-gmp). Since you may only compile the game for private use, you can comment out the following lines in the `stack.yaml`

```
# ghc-variant: integersimple

# setup-info:
#   ghc:
#     windows64-integersimple:
#       8.6.5:
#           url: "C:/Users/Nikolas/Downloads/ghc-8.6.5-x86_64-unknown-mingw32.tar.xz"
```

and

```
# flags:
#   # text:
#   #   integer-simple: true
#   hashable:
#     integer-gmp: false
#   scientific:
#     integer-simple: true
#   integer-logarithms:
#     integer-gmp: false
```

### 3. Build and install via stack:

`stack install`

`stack run`
