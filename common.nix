{ lib, options, ... }:

with lib;
{
  imports = [
    ./browser
    ./dogfood
    ./fonts
    ./kits/3d-printing
    ./kits/brain
    ./kits/collaboration
    ./kits/cnc
    ./kits/develop
    ./kits/pcbs
    ./kits/thinking
    ./kits/workstation
    ./modules/bubbles
    ./modules/environment/xdg
    ./networking
    ./networking/ses-email
    ./networking/ssh
    ./networking/tls
    ./networking/update-dns
    ./networking/wifi
    ./os/common
    ./pkg
    ./tools/1password
    ./tools/anki
    ./tools/aws
    ./tools/babashka
    ./tools/bCNC
    ./tools/buildkite
    ./tools/clang
    ./tools/claude
    ./tools/clojure
    ./tools/coscreen
    ./tools/cue
    ./tools/discord
    ./tools/distrobox
    ./tools/firefox
    ./tools/freecad
    ./tools/git
    ./tools/gnupg
    ./tools/gnuplot
    ./tools/go
    ./tools/gtypist
    ./tools/hammerspoon
    ./tools/inkscape
    ./tools/k9s
    ./tools/kakoune
    ./tools/kicad
    ./tools/kitty
    ./tools/meshlab
    ./tools/minicom
    ./tools/nats
    ./tools/nix
    ./tools/npm
    ./tools/obsidian
    ./tools/openscad
    ./tools/pcb2gcode
    ./tools/postgres
    ./tools/prusa-slicer
    ./tools/qemu
    ./tools/R
    ./tools/rlwrap
    ./tools/signal
    ./tools/slack
    ./tools/tex
    ./tools/tmux
    ./tools/x
    ./tools/xmonad
    ./tools/zoom
    ./tools/zsh
    ./shell
  ];

  config = mkMerge [
    (if (builtins.hasAttr "systemPath" options.environment)
     then {
      environment.systemPath = [ (toString ./bin) ];
     }
     else {
       environment.variables.PATH = [ (toString ./bin) ];
     })

    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
    }
  ];
}
