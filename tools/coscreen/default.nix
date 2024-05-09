{ lib, pkgs, ... }:

with lib;
{
  config = {
    homebrew.casks = mkIf pkgs.stdenv.isDarwin [ "coscreen" ];
  };
}
