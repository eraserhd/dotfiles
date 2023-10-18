{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      go_1_21
      gopls
      gotestsum
      gotools
    ];
  };
}
