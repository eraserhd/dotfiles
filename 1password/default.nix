{ pkgs, lib, ... }:

{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    homebrew.casks = [ "1password" ];
  };
}
