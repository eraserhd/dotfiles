{ pkgs, lib, ... }:

{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    environment.systemPackages = with pkgs; [
      alacritty
      reattach-to-user-namespace
    ];
  };
}
