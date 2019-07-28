{ pkgs, lib, options, ... }:

with lib;
{
  imports = [
    ./bash
    ./git
    ./tmux
  ];

  config = {
    environment.variables = {
      CDPATH = [
        "."
        "~/src"
        "~/src/dotfiles/kak/config/kak.symlink/autoload"
      ];
    };
  }
  // (if (builtins.hasAttr "systemPath" options.environment) then {
    environment.systemPath = [ (toString ./bin) ];
  } else {
    environment.variables.PATH = [ (toString ./bin) ];
  });
}
