let
  pkgs = import <nixpkgs> {};
  ghcVersion = "ghc928";
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    pkgs.pkg-config
  ];
  buildInputs = with pkgs; [
    pkgs.SDL2
    pkgs.SDL2_ttf
    pkgs.SDL2_mixer
    pkgs.SDL2_gfx
    pkgs.SDL2_image
    pkgs.libtiff
    pkgs.glib
    pkgs.libwebp
    pkgs.pcre2
    pkgs.libsndfile
    pkgs.alsa-lib
    pkgs.jack2
    pkgs.libpressureaudio
    haskellPackages.cabal-pkg-config-version-hook
    haskellPackages.bytestring
    haskellPackages.linear
    haskellPackages.StateVar
    haskellPackages.text
    haskellPackages.transformers
    haskellPackages.ghc
    haskellPackages.cabal-install
    haskellPackages.base
    haskellPackages.sdl2
    haskellPackages.sdl2-gfx
    haskellPackages.sdl2-image
    haskellPackages.sdl2-mixer
    haskellPackages.lens
    haskellPackages.containers
    haskellPackages.vector
    haskellPackages.data-default-class
  ];

  shellHook = ''
    export PKG_CONFIG_PATH="${pkgs.pkg-config}/bin/pkg-config:${pkgs.SDL2}/lib:${pkgs.libtiff}/lib:${pkgs.glib}/lib:${pkgs.libwebp}/lib:${pkgs.pcre2}/lib:${pkgs.libsndfile}/lib:${pkgs.libpressureaudio}/lib:${pkgs.alsa-lib}/lib:${pkgs.jack2}/lib:$PKG_CONFIG_PATH"
   '';
}
