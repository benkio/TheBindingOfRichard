# The Binding Of Richard

Game with Richard Benson as main character inspired by 2d platform games such as: `the binding of Isaac`, `pokemon`...

## Nix Integration

Nix integration is enabled, but not working at the moment 😢
Ideally would be great to have `shell.nix` to be cross platform and installing all the necessary dependencies.
Until it's fixed, we need to follow the standard installation of software necessary

## Prerequisites

All the packages should be handled by `stack`, apart from `SDL2` that needs to be properly installed in the system.
Check out [this link](https://github.com/haskell-game/sdl2#building) for the instruction in how to make `SDL2` and it's components installed in the system

### SDL2 on Mac

``` shell
brew install sdl2 sdl2_gfx sdl2_image sdl2_mixer sdl2_ttf
```

## Compiling

``` shell
stack update
stack build --no-nix
```

## Testing

## Running

`stack run --no-nix`
