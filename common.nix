{ pkgs, lib, ... }:

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

    environment.systemPath = lib.mkIf pkgs.stdenv.isDarwin [ (toString ./bin) ];
    environment.variables.PATH = lib.mkIf (!pkgs.stdenv.isDarwin) [ (toString ./bin) ];
  };
}
