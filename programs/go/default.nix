{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      go
      gopls
      gotools
    ];
  };
}
