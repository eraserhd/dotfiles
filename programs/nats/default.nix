{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      natscli
    ];
  };
}
