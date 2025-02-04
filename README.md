# Unity Tutorial Project in Haskell with Apecs and SDL2

This is a project based on the Unity 2D Roguelike tutorial. It doesn't follow the tutorial by the letter, but is very similar. Assets are from [https://learn.unity.com/project/2d-roguelike-tutorial](https://learn.unity.com/project/2d-roguelike-tutorial). It is built upon the ECS [apecs](https://hackage.haskell.org/package/apecs) for game logic and [sdl2](https://www.libsdl.org/download-2.0.php) for window management, images, audio and fonts.

![GIF of the game](./public/footage.gif)

## Issues during development

- Some `.aif` files wouldn't work so converted them to `ogg`
- Some documentation about SDL2 bindings that I had a hard time finding, but managed to find what I needed from the respective Github projects of the different bindings.

## Inspiration

[https://hackage.haskell.org/package/apecs](https://hackage.haskell.org/package/apecs)

- `apecs` is an ECS I always wanted to try out

[http://jxv.io/blog/2018-02-28-A-Game-in-Haskell.html](http://jxv.io/blog/2018-02-28-A-Game-in-Haskell.html)

- Non-`apecs` example with `SDL2`

[https://nmaehlmann.itch.io/mallrl](https://nmaehlmann.itch.io/mallrl)

- Main inspiration of how to use `apecs` and `SDL2` in a game

[https://lazyfoo.net/tutorials/SDL/](https://lazyfoo.net/tutorials/SDL/)

- Tutorials on how to use SDL2

[https://github.com/haskell-game/sdl2](https://github.com/haskell-game/sdl2)  
[https://hackage.haskell.org/package/sdl2-image](https://hackage.haskell.org/package/sdl2-image)  
[https://hackage.haskell.org/package/sdl2-mixer](https://hackage.haskell.org/package/sdl2-mixer)  
[https://hackage.haskell.org/package/sdl2-ttf](https://hackage.haskell.org/package/sdl2-ttf)

- Couldn't have done it without the SDL2 bindings

[https://hackage.haskell.org/package/rapid-0.1.4/docs/Rapid.html](https://hackage.haskell.org/package/rapid-0.1.4/docs/Rapid.html)  
[https://github.com/ndmitchell/ghcid](https://github.com/ndmitchell/ghcid)

- For hot reloading

## Building on Linux

### 1. Install SDL2 via apt

`sudo apt install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev libsdl2-mixer-dev libsdl2-gfx-dev libsdl2-net-dev`

### 2. Build and run

`stack run`

## Building on Windows

### 1. Install SDL2 via stack & pacman:

`stack exec -- pacman -Syu`

`stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2`

### 2. Build and run

I couldn't make `stack run` work, so the easiest way to play it is to do either

- Execute `stack ghci` and run `main`
- Execute `stack build --copy-bins --local-bin-path .` and run the built binary (`game-exe`)

## Development

For development use [rapid](https://hackage.haskell.org/package/rapid-0.1.4/docs/Rapid.html) and [ghcid](https://github.com/ndmitchell/ghcid) for some kind of "hot reloading". `Rapid` allows one to persist state (eg. window, renderer and game state) across `ghci` reloads and `ghcid` a simple way of recompiling changed code and running the `Rapid` entrypoint.

- Install `ghcid` by executing `stack install ghcid`
- Execute `ghcid`
  - Passed flags can be found in `.ghcid`
  - Window should appear and update whenever there is a code change

## Input
* Main Menu
  * ArrowUp, ArrowDown, Enter, Esc (Return to game)
* Game
  * ArrowUp, ArrowRight, ArrowDown, ArrowLeft, Esc (Return to menu)

## Related projects

Check out [https://github.com/Simre1/hero](https://github.com/Simre1/hero), it's an ECS like APECS and seems like a really cool project! I hope to make this unity tutorial project for it as well.

Check out [https://github.com/matthunz/aztecs](https://github.com/matthunz/aztecs), it's _also_ an ECS! 

## Thanks

If I forgot something or you think something should be added just open an issue! Free to do whatever with my code, but the assets aren't mine.
