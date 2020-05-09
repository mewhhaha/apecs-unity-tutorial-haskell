# Unity Tutorial Project in Haskell with Apecs and SDL2

This is a project loosely based on the Unity 2D Roguelike tutorial. Assets are from [https://learn.unity.com/project/2d-roguelike-tutorial](https://learn.unity.com/project/2d-roguelike-tutorial). It is built upon the ECS [apecs](https://hackage.haskell.org/package/apecs) for game logic and [sdl2](https://www.libsdl.org/download-2.0.php) for window management, images, audio and fonts.

![GIF of the game](./public/footage.gif)

## Issues during development

- Some `.aif` files wouldn't work so converted them to `ogg`
- Had issues described in [https://github.com/haskell-game/sdl2-mixer/issues/5](https://github.com/haskell-game/sdl2-mixer/issues/5) and solved it the same way, using a `Bytestring` instead of `Music`

## Inspiration

[https://nmaehlmann.itch.io/mallrl](https://nmaehlmann.itch.io/mallrl)

- Good example on how to use `apecs` and `SDL2` in a game

[https://lazyfoo.net/tutorials/SDL/](https://lazyfoo.net/tutorials/SDL/)

- Tutorials on how to use SDL2

## Building on Windows

### 1. Install SDL2 via stack & pacman:

`stack exec -- pacman -Syu`

`stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2`

### 3. Build and run

`stack build --copy-bins --local-bin-path game`  
run `game-exe` in the game folder
