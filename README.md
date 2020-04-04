# mallRL

mallRL is a grocery shopping roguelike developed for the 7DRL game jam.
A binary version can be downloaded here: https://nmaehlmann.itch.io/mallrl

![GIF showing mallRL](https://github.com/nmaehlmann/mallRL/blob/master/screenshots/mallrl.gif)

As the game was developed under strict time constraints the code is neither well organized nor high quality, but might still be interesting for people looking to develop a game in haskell.

The jam version is tagged as 7drl-release and was developed from monday 02.03.2020 to sunday 08.03.2020. The week before the jam I took 2 days to prepare the technical side, as I was developing the game from scratch without an engine using SDL2 and an ECS library. After my preparation I had the ECS library included, was able to read keyboard inputs and render tiles. On monday I started the jam with a tile that could be moved on screen.

## Building on Windows
### 1. Install SDL2 via stack & pacman:

```stack exec -- pacman -Syu```

```stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2```

### 2. Edit stack.yaml and remove integersimple ghc

The current code uses a custom compiled ghc for license reasons (see: https://ro-che.info/articles/2017-03-10-haskell-without-gmp). Since you may only compile the game for private use, you can comment out the following lines in the ```stack.yaml```

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

```stack install```

```stack run```
